#!/usr/bin/env julia

using GraphKernels
using Graphs
using JSON3
using SHA
using SimpleValueGraphs
using Statistics

const REPOSITORY = normpath(joinpath(@__DIR__, "..", ".."))
const REVISION = "f04797de0b86a39172b6adbcfcee35ec4b39f91b"
const TARGET_SAMPLE_NS = 120_000_000
const MAX_BATCH_SIZE = 100_000
const THREAD_ENVIRONMENT = [
    "GRAKERN_COURT_APPLICATION_THREADS",
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "JULIA_NUM_THREADS",
]

struct VertexHistogramKernel <: GraphKernels.AbstractGraphKernel end

function GraphKernels.preprocessed_form(
    ::VertexHistogramKernel,
    graph::SimpleValueGraphs.AbstractValGraph,
)
    counts = Dict{Any, Int}()
    for vertex in vertices(graph)
        label = get_vertexval(graph, vertex, 1)
        counts[label] = get(counts, label, 0) + 1
    end
    return counts
end

function GraphKernels.apply_preprocessed(
    ::VertexHistogramKernel,
    left::Dict,
    right::Dict,
)
    return sum(
        left_count * get(right, label, 0)
        for (label, left_count) in left
    )
end

struct NativeCorpus
    training::Vector{Any}
    query::Vector{Any}
    training_keys::Vector{String}
    query_keys::Vector{String}
end

struct OperationContext
    payload::Dict{String, Any}
    neutral::Dict{String, Any}
    prepared::Union{Nothing, NativeCorpus}
end

function read_json(path::AbstractString)
    return JSON3.read(read(path, String), Dict{String, Any})
end

function graph_from(sample::Dict{String, Any})
    labels = Int.(sample["node_labels"])
    topology = SimpleGraph(length(labels))
    for raw_edge in sample["edges"]
        add_edge!(
            topology,
            Int(raw_edge[1]) + 1,
            Int(raw_edge[2]) + 1,
        )
    end
    return ValGraph(
        topology;
        vertexval_types = (label = Int,),
        vertexval_init = vertex -> (label = labels[vertex],),
    )
end

function native_corpus(neutral::Dict{String, Any})
    training = neutral["training_graphs"]
    query = neutral["query_graphs"]
    return NativeCorpus(
        Any[graph_from(sample) for sample in training],
        Any[graph_from(sample) for sample in query],
        String[String(sample["key"]) for sample in training],
        String[String(sample["key"]) for sample in query],
    )
end

function load_context(payload::Dict{String, Any})
    path = joinpath(REPOSITORY, payload["corpus"]["path"])
    neutral = read_json(path)
    prepared = payload["job"]["boundary"] == "prepared_topology" ?
        native_corpus(neutral) :
        nothing
    return OperationContext(payload, neutral, prepared)
end

function kernel_from(configuration::Dict{String, Any})
    kernel = configuration["kernel"]
    if kernel == "wl_subtree"
        return WeisfeilerLehmanGraphKernel(
            base_kernel = VertexHistogramKernel(),
            vertex_labels = :,
            num_iterations = Int(configuration["rounds"]) + 1,
        )
    elseif kernel == "categorical_shortest_path"
        return ShortestPathGraphKernel(
            tol = 0.0,
            vertex_kernel = ConstVertexKernel(1.0),
            dist_key = nothing,
        )
    end
    error("unsupported GraphKernels.jl court kernel: $kernel")
end

function operation(context::OperationContext)
    started = time_ns()
    corpus = isnothing(context.prepared) ?
        native_corpus(context.neutral) :
        context.prepared
    graphs_ready = time_ns()
    configuration = context.payload["kernel_configuration"]
    kernel = kernel_from(configuration)
    observed = Matrix{Float64}(kernelmatrix(kernel, corpus.training))
    kernel_complete = time_ns()
    return (
        observed,
        Dict(
            "public_graph_construction" => Int(graphs_ready - started),
            "kernel_and_matrix" => Int(kernel_complete - graphs_ready),
        ),
        corpus.training_keys,
        corpus.training_keys,
    )
end

function translation_divisor(configuration::Dict{String, Any})
    return configuration["kernel"] == "categorical_shortest_path" ? 8.0 : 1.0
end

function translated_matrix(
    matrix::Matrix{Float64},
    configuration::Dict{String, Any},
)
    divisor = translation_divisor(configuration)
    return divisor == 1.0 ? matrix : matrix ./ divisor
end

function evaluate_once(context::OperationContext)
    raw, phases, rows, columns = operation(context)
    return (
        translated_matrix(
            raw,
            context.payload["kernel_configuration"],
        ),
        phases,
        rows,
        columns,
    )
end

function matrix_bytes(matrix::Matrix{Float64})
    result = UInt8[]
    sizehint!(result, length(matrix) * 8)
    for row in axes(matrix, 1), column in axes(matrix, 2)
        observed = matrix[row, column]
        isfinite(observed) || error(
            "GraphKernels.jl produced a nonfinite matrix value",
        )
        canonical = iszero(observed) ? 0.0 : observed
        bits = reinterpret(UInt64, canonical)
        for shift in 56:-8:0
            push!(result, UInt8((bits >> shift) & 0xff))
        end
    end
    return result
end

function sha256_hex(value::Vector{UInt8})
    return bytes2hex(sha256(value))
end

function sample_order_sha256(
    rows::Vector{String},
    columns::Vector{String},
)
    payload = JSON3.write((columns = columns, rows = rows)) * "\n"
    return bytes2hex(sha256(codeunits(payload)))
end

function choose_batch_size(calibration_ns::Int, mode::String)
    if mode == "cold" || calibration_ns >= 100_000_000
        return 1
    end
    estimate = max(1, cld(TARGET_SAMPLE_NS, max(1, calibration_ns)))
    return min(estimate, MAX_BATCH_SIZE)
end

function runtime_environment(job::Dict{String, Any})
    names = Set(THREAD_ENVIRONMENT)
    for name in keys(get(job, "environment", Dict{String, Any}()))
        push!(names, name)
    end
    return Dict(name => get(ENV, name, "") for name in sort!(collect(names)))
end

function proof(identifier::String, passed::Bool, evidence::String)
    return Dict(
        "id" => identifier,
        "passed" => passed,
        "evidence" => evidence,
    )
end

function unavailable(reason::String)
    return Dict(
        "available" => false,
        "reason_unavailable" => reason,
    )
end

function available(value)
    return Dict(
        "available" => true,
        "value" => value,
    )
end

function run_worker(payload::Dict{String, Any})
    context = load_context(payload)
    contract = payload["sampling_contract"]
    mode = String(contract["mode"])
    runtime_baseline = Int(Sys.maxrss())

    last_matrix = Matrix{Float64}(undef, 0, 0)
    last_rows = String[]
    last_columns = String[]
    for _ in 1:Int(contract["warmups"])
        last_matrix, _, last_rows, last_columns = evaluate_once(context)
    end

    calibration_ns = nothing
    batch_size = 1
    if mode != "cold"
        calibration_started = time_ns()
        last_matrix, _, last_rows, last_columns =
            evaluate_once(context)
        calibration_ns = max(1, Int(time_ns() - calibration_started))
        batch_size = choose_batch_size(calibration_ns, mode)
    end

    durations = Int[]
    phase_samples = Dict{String, Vector{Float64}}()
    for _ in 1:Int(contract["samples"])
        sample_phases = Dict{String, Int}()
        raw = Matrix{Float64}(undef, 0, 0)
        rows = String[]
        columns = String[]
        sample_started = time_ns()
        for _ in 1:batch_size
            raw, phases, rows, columns = operation(context)
            for (name, duration) in phases
                sample_phases[name] =
                    get(sample_phases, name, 0) + duration
            end
        end
        push!(durations, max(1, Int(time_ns() - sample_started)))
        last_rows = rows
        last_columns = columns
        last_matrix = translated_matrix(
            raw,
            payload["kernel_configuration"],
        )
        for (name, duration) in sample_phases
            push!(
                get!(phase_samples, name, Float64[]),
                duration / batch_size,
            )
        end
    end
    expected = payload["expected_output"]
    observed_sha = sha256_hex(matrix_bytes(last_matrix))
    observed_order = sample_order_sha256(last_rows, last_columns)
    shape = [size(last_matrix, 1), size(last_matrix, 2)]
    expected_shape = Int.(expected["shape"])
    exact =
        shape == expected_shape &&
        observed_order == expected["sample_order_sha256"] &&
        observed_sha == expected["expected_sha256"]
    phases = Dict(
        name => round(Int, median(values))
        for (name, values) in phase_samples
    )
    per_operation = durations ./ batch_size
    peak_rss = max(runtime_baseline, Int(Sys.maxrss()))
    boundary = payload["job"]["boundary"]
    divisor = translation_divisor(payload["kernel_configuration"])
    return Dict(
        "schema_version" => "grakern.performance-worker-result.v1",
        "job_id" => payload["job"]["id"],
        "runtime" => Dict(
            "language" => "julia",
            "language_version" => string(VERSION),
            "compiler_version" => string(Base.libllvm_version),
            "operating_system" => string(Sys.KERNEL),
            "architecture" => string(Sys.ARCH),
            "environment" => runtime_environment(payload["job"]),
            "vm_version" => (
                "GraphKernels.jl 0.2.0; source $REVISION"
            ),
        ),
        "semantic_output" => Dict(
            "shape" => shape,
            "sample_order_sha256" => observed_order,
            "comparison" => expected["comparison"],
            "observed_sha256" => observed_sha,
            "expected_sha256" => expected["expected_sha256"],
            "finite" => all(isfinite, last_matrix),
        ),
        "sampling" => Dict(
            "mode" => mode,
            "policy" => contract["policy"],
            "fork_index" => payload["execution"]["fork_index"],
            "warmups" => contract["warmups"],
            "samples" => contract["samples"],
            "calibration_ns" => calibration_ns,
            "batch_size" => batch_size,
            "timed_work_ns" => sum(durations),
            "durations_ns" => durations,
        ),
        "metrics" => Dict(
            "wall_clock_median_ns" => round(Int, median(per_operation)),
            "cpu_time_ns" => unavailable(
                "Julia 1.12 does not expose a stable public process CPU-time counter",
            ),
            "runtime_baseline_rss_bytes" => runtime_baseline,
            "peak_rss_bytes" => peak_rss,
            "peak_rss_delta_bytes" => peak_rss - runtime_baseline,
            "allocated_bytes" => unavailable(
                "GraphKernels.jl worker does not expose complete-operation allocations",
            ),
            "gc_count" => unavailable(
                "Julia cumulative GC collection count is not a stable public API",
            ),
            "phases" => phases,
        ),
        "proof_checks" => [
            proof(
                "semantic.independent_reference",
                exact,
                (
                    "shape=$(shape)/$(expected_shape) " *
                    "order=$(observed_order)/$(expected["sample_order_sha256"]) " *
                    "output=$(observed_sha)/$(expected["expected_sha256"])"
                ),
            ),
            proof(
                "operation.state_reset",
                true,
                "a fresh GraphKernels.jl kernel is constructed for every operation",
            ),
            proof(
                "boundary.complete",
                true,
                boundary == "public_end_to_end" ?
                    "public graph construction is timed" :
                    "native GraphKernels.jl graph topology is prepared; all kernel work is timed",
            ),
            proof(
                "semantic.external_translation",
                true,
                "post-timing exact divisor=$divisor",
            ),
        ],
    )
end

function argument_value(name::String)
    index = findfirst(==(name), ARGS)
    isnothing(index) && error("missing required argument $name")
    index == length(ARGS) && error("missing value for argument $name")
    return ARGS[index + 1]
end

function main()
    payload = read_json(argument_value("--job"))
    output = argument_value("--output")
    result = run_worker(payload)
    mkpath(dirname(output))
    open(output, "w") do io
        JSON3.pretty(io, result)
        write(io, "\n")
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
