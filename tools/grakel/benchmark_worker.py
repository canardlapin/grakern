#!/usr/bin/env python3
"""GraKeL worker for Grakern's correctness-locked performance court."""

from __future__ import annotations

import argparse
import gc
import hashlib
import importlib.metadata
import json
import math
import os
import platform
import resource
import statistics
import struct
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import grakel
import numpy as np
from grakel import GraphKernel

REPOSITORY = Path(__file__).resolve().parents[2]
TARGET_WARM_TIMED_WORK_NS = 60_000_000_000
MINIMUM_TARGET_SAMPLE_NS = 200_000_000
MAX_BATCH_SIZE = 100_000
THREAD_ENVIRONMENT = (
    "GRAKERN_COURT_APPLICATION_THREADS",
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "JULIA_NUM_THREADS",
)


@dataclass(frozen=True)
class NativeCorpus:
    training: list[Any]
    query: list[Any]
    training_keys: list[str]
    query_keys: list[str]


@dataclass(frozen=True)
class OperationContext:
    payload: dict[str, Any]
    neutral: dict[str, Any]
    prepared: NativeCorpus | None


def canonical_json_bytes(value: Any) -> bytes:
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
        )
        + "\n"
    ).encode()


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sample_order_sha256(rows: list[str], columns: list[str]) -> str:
    return sha256_bytes(
        canonical_json_bytes(
            {
                "columns": columns,
                "rows": rows,
            }
        )
    )


def matrix_bytes(matrix: np.ndarray[Any, np.dtype[np.float64]]) -> bytes:
    result = bytearray()
    for value in matrix.flat:
        observed = float(value)
        if not math.isfinite(observed):
            raise ValueError("GraKeL produced a nonfinite matrix value")
        canonical = 0.0 if observed == 0.0 else observed
        result.extend(struct.pack(">d", canonical))
    return bytes(result)


def as_grakel_graph(
    sample: dict[str, Any],
) -> tuple[dict[int, list[int]], dict[int, int]]:
    labels = {index: int(value) for index, value in enumerate(sample["node_labels"])}
    adjacency = {index: [] for index in labels}
    for left, right in sample["edges"]:
        adjacency[left].append(right)
        adjacency[right].append(left)
    return adjacency, labels


def native_corpus(neutral: dict[str, Any]) -> NativeCorpus:
    training = neutral["training_graphs"]
    query = neutral["query_graphs"]
    return NativeCorpus(
        training=[as_grakel_graph(sample) for sample in training],
        query=[as_grakel_graph(sample) for sample in query],
        training_keys=[sample["key"] for sample in training],
        query_keys=[sample["key"] for sample in query],
    )


def graph_kernel(configuration: dict[str, Any]) -> GraphKernel:
    kernel = configuration["kernel"]
    if kernel in {"wl_subtree", "wl_subtree_cross"}:
        specification: Any = [
            {
                "name": "weisfeiler_lehman",
                "n_iter": configuration["rounds"],
            },
            {"name": "subtree_wl"},
        ]
    elif kernel == "wl_optimal_assignment":
        specification = {
            "name": "weisfeiler_lehman_optimal_assignment",
            "n_iter": configuration["rounds"],
        }
    elif kernel == "categorical_shortest_path":
        specification = {
            "name": "shortest_path",
            "algorithm_type": "floyd_warshall",
            "with_labels": False,
        }
    else:
        raise ValueError(f"unsupported GraKeL court kernel: {kernel}")
    return GraphKernel(kernel=specification, normalize=False)


def raw_matrix(
    corpus: NativeCorpus,
    configuration: dict[str, Any],
) -> np.ndarray[Any, np.dtype[np.float64]]:
    kernel = graph_kernel(configuration)
    if configuration["kernel"] == "wl_subtree_cross":
        kernel.fit(corpus.training)
        return np.asarray(kernel.transform(corpus.query), dtype=np.float64)
    return np.asarray(kernel.fit_transform(corpus.training), dtype=np.float64)


def translation_divisor(configuration: dict[str, Any]) -> float:
    if configuration["kernel"] == "categorical_shortest_path":
        return 4.0
    return 1.0


def translated_matrix(
    matrix: np.ndarray[Any, np.dtype[np.float64]],
    configuration: dict[str, Any],
) -> np.ndarray[Any, np.dtype[np.float64]]:
    divisor = translation_divisor(configuration)
    return matrix if divisor == 1.0 else matrix / divisor


def load_context(payload: dict[str, Any]) -> OperationContext:
    corpus_path = REPOSITORY / payload["corpus"]["path"]
    neutral = json.loads(corpus_path.read_text(encoding="utf-8"))
    boundary = payload["job"]["boundary"]
    prepared = native_corpus(neutral) if boundary == "prepared_topology" else None
    return OperationContext(payload, neutral, prepared)


def operation(
    context: OperationContext,
) -> tuple[np.ndarray[Any, np.dtype[np.float64]], dict[str, int], list[str], list[str]]:
    started = time.perf_counter_ns()
    corpus = (
        context.prepared
        if context.prepared is not None
        else native_corpus(context.neutral)
    )
    graphs_ready = time.perf_counter_ns()
    assert corpus is not None
    configuration = context.payload["kernel_configuration"]
    observed = raw_matrix(corpus, configuration)
    kernel_complete = time.perf_counter_ns()
    rows = (
        corpus.query_keys
        if configuration["kernel"] == "wl_subtree_cross"
        else corpus.training_keys
    )
    return (
        observed,
        {
            "public_graph_construction": graphs_ready - started,
            "kernel_and_matrix": kernel_complete - graphs_ready,
        },
        rows,
        corpus.training_keys,
    )


def evaluate_once(
    context: OperationContext,
) -> tuple[
    np.ndarray[Any, np.dtype[np.float64]],
    dict[str, int],
    list[str],
    list[str],
]:
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


def rss_bytes() -> int:
    observed = int(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
    return observed if sys.platform == "darwin" else observed * 1024


def gc_collections() -> int:
    return sum(int(generation["collections"]) for generation in gc.get_stats())


def unavailable(reason: str) -> dict[str, Any]:
    return {
        "available": False,
        "reason_unavailable": reason,
    }


def available(value: int) -> dict[str, Any]:
    return {
        "available": True,
        "value": value,
    }


def target_sample_ns(contract: dict[str, Any]) -> int:
    measurement_count = max(1, contract["forks"] * contract["samples"])
    return max(
        MINIMUM_TARGET_SAMPLE_NS,
        math.ceil(TARGET_WARM_TIMED_WORK_NS / measurement_count),
    )


def choose_batch_size(
    calibration_ns: int,
    contract: dict[str, Any],
) -> int:
    if contract["mode"] == "cold":
        return 1
    estimate = max(
        1,
        math.ceil(target_sample_ns(contract) / max(1, calibration_ns)),
    )
    return min(estimate, MAX_BATCH_SIZE)


def runtime_environment(job: dict[str, Any]) -> dict[str, str]:
    names = set(THREAD_ENVIRONMENT) | set(job.get("environment", {}))
    return {name: os.environ.get(name, "") for name in sorted(names)}


def proof(
    identifier: str,
    passed: bool,
    evidence: str,
) -> dict[str, Any]:
    return {
        "id": identifier,
        "passed": passed,
        "evidence": evidence,
    }


def run_worker(payload: dict[str, Any]) -> dict[str, Any]:
    context = load_context(payload)
    contract = payload["sampling_contract"]
    mode = contract["mode"]
    runtime_baseline = rss_bytes()

    last_matrix: np.ndarray[Any, np.dtype[np.float64]] | None = None
    last_rows: list[str] = []
    last_columns: list[str] = []
    for _ in range(contract["warmups"]):
        last_matrix, _, last_rows, last_columns = evaluate_once(context)

    calibration_ns: int | None = None
    if mode == "cold":
        batch_size = 1
    else:
        calibration_started = time.perf_counter_ns()
        (
            last_matrix,
            _,
            last_rows,
            last_columns,
        ) = evaluate_once(context)
        first_elapsed = max(
            1,
            time.perf_counter_ns() - calibration_started,
        )
        pilot_batch = choose_batch_size(first_elapsed, contract)
        observed_ns = first_elapsed
        for _ in range(2):
            for _ in range(pilot_batch):
                operation_started = time.perf_counter_ns()
                (
                    last_matrix,
                    _,
                    last_rows,
                    last_columns,
                ) = evaluate_once(context)
                observed_ns = min(
                    observed_ns,
                    max(1, time.perf_counter_ns() - operation_started),
                )
        calibration_ns = observed_ns
        batch_size = choose_batch_size(calibration_ns, contract)

    durations: list[int] = []
    phase_samples: dict[str, list[float]] = {}
    cpu_started = time.process_time_ns()
    gc_started = gc_collections()
    for _ in range(contract["samples"]):
        sample_phases: dict[str, int] = {}
        sample_started = time.perf_counter_ns()
        raw: np.ndarray[Any, np.dtype[np.float64]] | None = None
        rows: list[str] = []
        columns: list[str] = []
        for _ in range(batch_size):
            raw, phases, rows, columns = operation(context)
            for name, duration in phases.items():
                sample_phases[name] = sample_phases.get(name, 0) + duration
        duration = time.perf_counter_ns() - sample_started
        durations.append(max(1, duration))
        if raw is None:
            raise RuntimeError("sampling batch produced no matrix")
        last_rows = rows
        last_columns = columns
        last_matrix = translated_matrix(
            raw,
            payload["kernel_configuration"],
        )
        for name, phase_duration in sample_phases.items():
            phase_samples.setdefault(name, []).append(phase_duration / batch_size)
    cpu_elapsed = max(0, time.process_time_ns() - cpu_started)
    gc_elapsed = max(0, gc_collections() - gc_started)

    if last_matrix is None:
        raise RuntimeError("sampling produced no matrix")
    expected = payload["expected_output"]
    observed_payload = matrix_bytes(last_matrix)
    observed_sha = sha256_bytes(observed_payload)
    observed_order = sample_order_sha256(last_rows, last_columns)
    shape = [int(last_matrix.shape[0]), int(last_matrix.shape[1])]
    exact = (
        shape == expected["shape"]
        and observed_order == expected["sample_order_sha256"]
        and observed_sha == expected["expected_sha256"]
    )
    boundary = payload["job"]["boundary"]
    phases = {
        name: round(statistics.median(values))
        for name, values in sorted(phase_samples.items())
    }
    per_operation = [duration / batch_size for duration in durations]
    peak_rss = max(runtime_baseline, rss_bytes())
    divisor = translation_divisor(payload["kernel_configuration"])
    return {
        "schema_version": "grakern.performance-worker-result.v1",
        "job_id": payload["job"]["id"],
        "runtime": {
            "language": "python",
            "language_version": platform.python_version(),
            "compiler_version": platform.python_compiler(),
            "operating_system": platform.platform(),
            "architecture": platform.machine(),
            "environment": runtime_environment(payload["job"]),
            "vm_version": (
                f"GraKeL {importlib.metadata.version('GraKeL')}; "
                f"NumPy {np.__version__}; module {grakel.__version__}"
            ),
        },
        "semantic_output": {
            "shape": shape,
            "sample_order_sha256": observed_order,
            "comparison": expected["comparison"],
            "observed_sha256": observed_sha,
            "expected_sha256": expected["expected_sha256"],
            "finite": bool(np.isfinite(last_matrix).all()),
        },
        "sampling": {
            "mode": mode,
            "policy": contract["policy"],
            "fork_index": payload["execution"]["fork_index"],
            "warmups": contract["warmups"],
            "samples": contract["samples"],
            "calibration_ns": calibration_ns,
            "batch_size": batch_size,
            "timed_work_ns": sum(durations),
            "durations_ns": durations,
        },
        "metrics": {
            "wall_clock_median_ns": round(statistics.median(per_operation)),
            "cpu_time_ns": available(cpu_elapsed),
            "runtime_baseline_rss_bytes": runtime_baseline,
            "peak_rss_bytes": peak_rss,
            "peak_rss_delta_bytes": peak_rss - runtime_baseline,
            "allocated_bytes": unavailable(
                "Python/GraKeL does not expose complete-operation allocations"
            ),
            "gc_count": available(gc_elapsed),
            "phases": phases,
        },
        "proof_checks": [
            proof(
                "semantic.independent_reference",
                exact,
                (
                    f"shape={shape}/{expected['shape']} "
                    f"order={observed_order}/{expected['sample_order_sha256']} "
                    f"output={observed_sha}/{expected['expected_sha256']}"
                ),
            ),
            proof(
                "operation.state_reset",
                True,
                "a fresh GraphKernel is constructed for every operation",
            ),
            proof(
                "boundary.complete",
                True,
                (
                    "public graph construction is timed"
                    if boundary == "public_end_to_end"
                    else "native GraKeL graph topology is prepared; all kernel work is timed"
                ),
            ),
            proof(
                "semantic.external_translation",
                True,
                f"post-timing exact divisor={divisor}",
            ),
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--job", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    arguments = parser.parse_args()
    payload = json.loads(arguments.job.read_text(encoding="utf-8"))
    result = run_worker(payload)
    arguments.output.parent.mkdir(parents=True, exist_ok=True)
    arguments.output.write_text(
        json.dumps(result, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
