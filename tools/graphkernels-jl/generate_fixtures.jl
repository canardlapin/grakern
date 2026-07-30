#!/usr/bin/env julia

using GraphKernels
using Graphs
using SimpleValueGraphs

const REPOSITORY = normpath(joinpath(@__DIR__, "..", ".."))
const REVISION = "f04797de0b86a39172b6adbcfcee35ec4b39f91b"
const DEFAULT_OUTPUT = joinpath(
    REPOSITORY,
    "engine-jvm",
    "src",
    "test",
    "resources",
    "graphkernels-jl",
    REVISION,
    "wl_regular_uniform.json",
)

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

const SAMPLES = [
    (
        key = "pair",
        labels = ["A", "A"],
        edges = [(1, 2)],
    ),
    (
        key = "triangle",
        labels = ["A", "A", "A"],
        edges = [(1, 2), (2, 3), (1, 3)],
    ),
    (
        key = "cycle4",
        labels = ["A", "A", "A", "A"],
        edges = [(1, 2), (2, 3), (3, 4), (4, 1)],
    ),
]

function graph_from(sample)
    topology = SimpleGraph(length(sample.labels))
    for (left, right) in sample.edges
        add_edge!(topology, left, right)
    end
    return ValGraph(
        topology;
        vertexval_types = (label = String,),
        vertexval_init = vertex -> (label = sample.labels[vertex],),
    )
end

function json_string(value::AbstractString)
    escaped = replace(
        value,
        "\\" => "\\\\",
        "\"" => "\\\"",
        "\n" => "\\n",
    )
    return "\"$escaped\""
end

function json_array(values)
    return "[" * join(values, ", ") * "]"
end

function render_fixture()
    graphs = graph_from.(SAMPLES)
    rounds = 2
    iterations = rounds + 1
    kernel = WeisfeilerLehmanGraphKernel(
        base_kernel = VertexHistogramKernel(),
        vertex_labels = :,
        num_iterations = iterations,
    )
    gram = kernelmatrix(kernel, graphs)
    flat = [
        Float64(gram[row, column])
        for row in axes(gram, 1)
        for column in axes(gram, 2)
    ]
    sample_json = [
        """
        {
          "edge_count": $(length(sample.edges)),
          "key": $(json_string(sample.key)),
          "labels": $(json_array(json_string.(sample.labels))),
          "vertex_count": $(length(sample.labels))
        }"""
        for sample in SAMPLES
    ]
    flat_json = json_array(string.(flat))
    return """
    {
      "comparison_policy": {
        "absolute_tolerance": 1e-12,
        "authority": "compatibility evidence, not semantic oracle",
        "relative_tolerance": 1e-12
      },
      "gram_shape": [$(length(SAMPLES)), $(length(SAMPLES))],
      "oracle": {
        "distribution": "GraphKernels.jl",
        "distribution_version": "0.2.0",
        "julia_version": $(json_string(string(VERSION))),
        "source_revision": "$REVISION"
      },
      "samples": $(json_array(sample_json)),
      "schema_version": "grakern.graphkernels-jl-fixture.v1",
      "semantics": {
        "base_kernel": "custom exact vertex-label histogram dot product",
        "edge_labels": false,
        "grakern_rounds": $rounds,
        "graph_encoding": "SimpleValueGraphs.ValGraph over an undirected Graphs.SimpleGraph",
        "graphkernels_num_iterations": $iterations,
        "initial_level_included": true,
        "normalization": false,
        "sample_order": $(json_array(json_string.([sample.key for sample in SAMPLES]))),
        "scope_restriction": "uniform-labeled regular graphs only"
      },
      "source_audit": {
        "broad_wl_parity": "excluded",
        "reason": "the pinned refinement reads vertexvals[u, i-1] for every neighbor rather than vertexvals[v, i-1]",
        "shortest_path": "deferred until Grakern E6 semantics are frozen"
      },
      "wl_subtree_gram_flat": $flat_json
    }
    """
end

function main()
    check = "--check" in ARGS
    write_fixture = "--write" in ARGS
    check && write_fixture && error("--check and --write are mutually exclusive")
    output_index = findfirst(==("--output"), ARGS)
    output = isnothing(output_index) ? DEFAULT_OUTPUT : ARGS[output_index + 1]
    rendered = render_fixture()

    if check
        read(output, String) == rendered ||
            error("GraphKernels.jl fixture drift: $output")
    elseif write_fixture
        mkpath(dirname(output))
        write(output, rendered)
    else
        print(rendered)
    end
end

main()
