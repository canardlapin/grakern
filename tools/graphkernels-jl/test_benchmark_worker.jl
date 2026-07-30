#!/usr/bin/env julia

include(joinpath(@__DIR__, "benchmark_worker.jl"))

const TIER1 = read_json(
    joinpath(REPOSITORY, "benchmarks", "court", "tier1.json"),
)

function payload(row::Dict{String, Any}, boundary::String)
    return Dict{String, Any}(
        "job" => Dict{String, Any}(
            "boundary" => boundary,
            "id" => "test-$(row["id"])-$boundary",
            "environment" => Dict{String, Any}(),
        ),
        "kernel_configuration" => TIER1["kernel_configurations"][row["kernel"]],
        "corpus" => TIER1["corpora"][row["corpus"]],
        "expected_output" => row["expected_output"],
    )
end

function verify_eligible_rows()
    verified = 0
    for row in TIER1["rows"]
        "graphkernels_jl" in row["comparators"] || continue
        for boundary in ["public_end_to_end", "prepared_topology"]
            context = load_context(payload(row, boundary))
            observed, _, rows, columns = evaluate_once(context)
            expected = row["expected_output"]
            @assert collect(size(observed)) == Int.(expected["shape"])
            @assert sample_order_sha256(rows, columns) ==
                expected["sample_order_sha256"]
            @assert sha256_hex(matrix_bytes(observed)) ==
                expected["expected_sha256"]
            verified += 1
        end
    end
    return verified
end

function verify_protocol_result()
    row = first(
        row
        for row in TIER1["rows"]
        if "graphkernels_jl" in row["comparators"]
    )
    worker_payload = payload(row, "prepared_topology")
    worker_payload["execution"] = Dict("fork_index" => 0)
    worker_payload["sampling_contract"] = Dict(
        "mode" => "cold",
        "policy" => "standard",
        "warmups" => 0,
        "samples" => 1,
    )
    result = run_worker(worker_payload)
    @assert result["schema_version"] ==
        "grakern.performance-worker-result.v1"
    @assert all(check["passed"] for check in result["proof_checks"])
    @assert result["semantic_output"]["observed_sha256"] ==
        result["semantic_output"]["expected_sha256"]
end

verified = verify_eligible_rows()
@assert verified == 8
@assert translation_divisor(
    Dict{String, Any}("kernel" => "categorical_shortest_path"),
) == 8.0
verify_protocol_result()
println("GraphKernels.jl benchmark worker: verified $verified row-boundaries")
