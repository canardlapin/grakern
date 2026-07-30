#!/usr/bin/env python3
"""Generate reproducible diagnostic or championship performance-court plans."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
from collections.abc import Iterable
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator

REPOSITORY = Path(__file__).resolve().parents[2]
GRAPHKERNELS_REVISION = "f04797de0b86a39172b6adbcfcee35ec4b39f91b"
IMPLEMENTATION_LANGUAGE = {
    "grakern": "scala-jvm",
    "grakel": "python",
    "graphkernels_jl": "julia",
}
IMPLEMENTATION_VERSION = {
    "grakern": "0.1.0-SNAPSHOT",
    "grakel": "0.1.10",
    "graphkernels_jl": f"0.2.0+{GRAPHKERNELS_REVISION}",
}


def read_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def canonical_json(value: Any) -> str:
    return json.dumps(
        value,
        indent=2,
        sort_keys=True,
        ensure_ascii=False,
    ) + "\n"


def git_revision(path: Path) -> str | None:
    completed = subprocess.run(
        ["git", "-C", str(path), "rev-parse", "HEAD"],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        return None
    return completed.stdout.strip()


def git_clean(path: Path) -> bool:
    completed = subprocess.run(
        [
            "git",
            "-C",
            str(path),
            "status",
            "--porcelain=v1",
            "--untracked-files=all",
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    return completed.returncode == 0 and completed.stdout == ""


def revision_from_build(repository: Path, name: str) -> str:
    build = (repository / "build.sbt").read_text(encoding="utf-8")
    match = re.search(
        rf'val {re.escape(name)}Revision\s*=\s*"([0-9a-f]{{40}})"',
        build,
    )
    if match is None:
        raise ValueError(f"build.sbt does not define {name}Revision")
    return match.group(1)


def find_pinned_checkout(
    repository: Path,
    project_name: str,
    revision: str,
    explicit: Path | None,
) -> Path:
    candidates: list[Path] = []
    if explicit is not None:
        candidates.append(explicit.expanduser().resolve())
    candidates.extend(
        [
            repository.parent / project_name,
            Path.home() / "code" / project_name,
            Path.home() / "code" / "scala" / project_name,
        ]
    )
    candidates.extend(
        sorted(
            (Path.home() / ".sbt" / "1.0" / "staging").glob(
                f"*/{project_name}"
            )
        )
    )
    seen: set[Path] = set()
    for candidate in candidates:
        resolved = candidate.expanduser().resolve()
        if resolved in seen:
            continue
        seen.add(resolved)
        if git_revision(resolved) == revision and git_clean(resolved):
            return resolved
    raise ValueError(
        f"cannot find a clean {project_name} checkout at pinned revision "
        f"{revision}; "
        f"pass --{project_name}-source"
    )


def host_policy(coverage_mode: str) -> dict[str, Any]:
    if coverage_mode == "championship":
        return {
            "probe_seconds": 30,
            "maximum_non_court_cpu_percent": 5,
            "minimum_free_memory_bytes": 2_147_483_648,
            "maximum_calibration_cv": 0.02,
            "maximum_calibration_drift": 0.02,
            "calibration_samples": 10,
            "require_ac_power": True,
            "require_performance_mode": True,
            "require_thermal_probe": True,
        }
    return {
        "probe_seconds": 0.1,
        "maximum_non_court_cpu_percent": 100,
        "minimum_free_memory_bytes": 0,
        "maximum_calibration_cv": 100,
        "maximum_calibration_drift": 100,
        "calibration_samples": 3,
        "require_ac_power": False,
        "require_performance_mode": False,
        "require_thermal_probe": False,
    }


def classification_policy() -> dict[str, Any]:
    return {
        "bootstrap_seed": 730_204,
        "bootstrap_resamples": 20_000,
        "confidence_level": 0.99,
        "win_median_ratio": 0.9,
        "win_confidence_upper": 0.95,
        "loss_median_ratio": 1.0,
        "memory_ratio": 1.1,
        "memory_absolute_bytes": 67_108_864,
    }


def semantic_translation(row: dict[str, Any], implementation: str) -> str:
    value = row["semantic_alignment"][implementation]["translation"]
    if isinstance(value, str):
        return value
    return json.dumps(value, sort_keys=True, separators=(",", ":"))


def implementation_command(
    repository: Path,
    implementation: str,
    java: Path,
    grakel_python: Path,
    julia: Path,
    julia_depot: Path,
) -> tuple[list[str], list[str], str, dict[str, str]]:
    if implementation == "grakern":
        jar = (
            repository
            / "benchmark-worker"
            / "target"
            / "scala-3.7.4"
            / "grakern-benchmark-worker.jar"
        )
        return (
            [
                str(java),
                "-jar",
                str(jar),
                "--job",
                "{job_json}",
                "--output",
                "{worker_result_json}",
            ],
            [str(java), "-version"],
            str(jar),
            {},
        )
    if implementation == "grakel":
        worker = repository / "tools" / "grakel" / "benchmark_worker.py"
        return (
            [
                str(grakel_python),
                str(worker),
                "--job",
                "{job_json}",
                "--output",
                "{worker_result_json}",
            ],
            [
                str(grakel_python),
                "-c",
                (
                    "import grakel,numpy,scipy,sklearn,sys;"
                    "print(sys.version);"
                    "print(grakel.__version__,numpy.__version__,"
                    "scipy.__version__,sklearn.__version__)"
                ),
            ],
            str(worker),
            {
                "PYTHONHASHSEED": "0",
                "PYTHONNOUSERSITE": "1",
            },
        )
    worker = repository / "tools" / "graphkernels-jl" / "benchmark_worker.jl"
    project = repository / "tools" / "graphkernels-jl"
    return (
        [
            str(julia),
            "--startup-file=no",
            "--history-file=no",
            f"--project={project}",
            str(worker),
            "--job",
            "{job_json}",
            "--output",
            "{worker_result_json}",
        ],
        [
            str(julia),
            "--startup-file=no",
            f"--project={project}",
            "-e",
            (
                "using GraphKernels,JSON3;"
                'print(VERSION," ",Base.libllvm_version)'
            ),
        ],
        str(worker),
        {"JULIA_DEPOT_PATH": str(julia_depot)},
    )


def selected_rows(
    tier1: dict[str, Any],
    identifiers: set[str] | None,
) -> list[dict[str, Any]]:
    rows = tier1["rows"]
    if identifiers is None:
        return rows
    observed = {row["id"] for row in rows}
    missing = sorted(identifiers - observed)
    if missing:
        raise ValueError(f"unknown Tier-1 rows: {', '.join(missing)}")
    return [row for row in rows if row["id"] in identifiers]


def job_modes(
    coverage_mode: str,
    boundaries: Iterable[str] | None,
    modes: Iterable[str] | None,
) -> list[tuple[str, str]]:
    if coverage_mode == "championship":
        return [
            ("public_end_to_end", "warm"),
            ("prepared_topology", "warm"),
            ("public_end_to_end", "cold"),
        ]
    selected_boundaries = list(boundaries or ["prepared_topology"])
    selected_modes = list(modes or ["warm"])
    result = [
        (boundary, mode)
        for boundary in selected_boundaries
        for mode in selected_modes
        if not (boundary == "prepared_topology" and mode == "cold")
    ]
    if not result:
        raise ValueError("diagnostic selection produced no valid boundary/mode")
    return result


def build_plan(
    *,
    repository: Path,
    tier1: dict[str, Any],
    court_id: str,
    coverage_mode: str,
    row_ids: set[str] | None,
    boundaries: Iterable[str] | None,
    modes: Iterable[str] | None,
    implementations: set[str] | None,
    expensive_rows: set[str],
    expensive_jobs: set[tuple[str, str]],
    timeout_seconds: int,
    java: Path,
    grakel_python: Path,
    julia: Path,
    julia_depot: Path,
    graph4s_source: Path,
    gale_source: Path,
) -> dict[str, Any]:
    court_directory = repository / "benchmarks" / "court"
    thread_policy = tier1["thread_policy"]
    all_row_ids = {row["id"] for row in tier1["rows"]}
    unknown_expensive = expensive_rows - all_row_ids
    if unknown_expensive:
        raise ValueError(
            "unknown expensive rows: "
            + ", ".join(sorted(unknown_expensive))
        )
    eligible_jobs = {
        (row["id"], implementation)
        for row in tier1["rows"]
        for implementation in ["grakern", *row["comparators"]]
    }
    unknown_expensive_jobs = expensive_jobs - eligible_jobs
    if unknown_expensive_jobs:
        raise ValueError(
            "unknown expensive jobs: "
            + ", ".join(
                f"{row}:{implementation}"
                for row, implementation in sorted(unknown_expensive_jobs)
            )
        )
    if coverage_mode == "championship" and any(
        value is not None
        for value in (
            row_ids,
            boundaries,
            modes,
            implementations,
        )
    ):
        raise ValueError(
            "row, boundary, mode, and implementation selectors require "
            "diagnostic_subset"
        )
    jobs: list[dict[str, Any]] = []
    for row in selected_rows(tier1, row_ids):
        eligible = ["grakern", *row["comparators"]]
        if implementations is not None:
            eligible = [
                implementation
                for implementation in eligible
                if implementation in implementations
            ]
        if not eligible:
            raise ValueError(f"row {row['id']} has no selected implementation")
        for boundary, mode in job_modes(
            coverage_mode,
            boundaries,
            modes,
        ):
            for implementation in eligible:
                command, version_command, entrypoint, environment = (
                    implementation_command(
                        repository,
                        implementation,
                        java,
                        grakel_python,
                        julia,
                        julia_depot,
                    )
                )
                policy = (
                    "expensive_row"
                    if mode == "warm"
                    and (
                        row["id"] in expensive_rows
                        or (row["id"], implementation) in expensive_jobs
                    )
                    else "standard"
                )
                job: dict[str, Any] = {
                    "id": (
                        f"{row['id']}.{boundary}.{mode}.{implementation}"
                    ),
                    "tier1_id": row["id"],
                    "boundary": boundary,
                    "mode": mode,
                    "sampling_policy": policy,
                    "implementation": implementation,
                    "language": IMPLEMENTATION_LANGUAGE[implementation],
                    "version": IMPLEMENTATION_VERSION[implementation],
                    "entrypoint": entrypoint,
                    "comparator_role": (
                        "candidate"
                        if implementation == "grakern"
                        else "external_comparator"
                    ),
                    "command": command,
                    "version_command": version_command,
                    "working_directory": str(repository),
                    "timeout_seconds": timeout_seconds,
                    "application_threads": thread_policy[
                        "application_threads"
                    ],
                    "blas_threads": thread_policy["blas_threads"],
                    "environment": environment,
                }
                if implementation != "grakern":
                    job["semantic_translation"] = semantic_translation(
                        row,
                        implementation,
                    )
                jobs.append(job)
    return {
        "schema_version": "grakern.performance-court-plan.v1",
        "court_id": court_id,
        "coverage_mode": coverage_mode,
        "execution_order_seed": 730_205,
        "tier1_manifest": str(court_directory / "tier1.json"),
        "receipt_schema": str(court_directory / "receipt.schema.json"),
        "worker_result_schema": str(
            court_directory / "worker-result.schema.json"
        ),
        "runner_files": [
            str(court_directory / "generate_plan.py"),
            str(court_directory / "tier1.json"),
            str(repository / "tools" / "grakel" / "benchmark_worker.py"),
            str(
                repository
                / "tools"
                / "graphkernels-jl"
                / "benchmark_worker.jl"
            ),
        ],
        "source_components": [
            {
                "id": "grakern",
                "path": str(repository),
                "lock_files": [
                    "build.sbt",
                    "project/build.properties",
                    "project/plugins.sbt",
                    "tools/grakel/performance-requirements.txt",
                    "tools/graphkernels-jl/Project.toml",
                    "tools/graphkernels-jl/Manifest.toml",
                ],
            },
            {
                "id": "graph4s",
                "path": str(graph4s_source),
                "lock_files": [
                    "build.sbt",
                    "project/build.properties",
                    "project/plugins.sbt",
                ],
            },
            {
                "id": "gale",
                "path": str(gale_source),
                "lock_files": [
                    "build.sbt",
                    "project/build.properties",
                    "project/plugins.sbt",
                ],
            },
        ],
        "host_policy": host_policy(coverage_mode),
        "classification_policy": classification_policy(),
        "jobs": jobs,
    }


def executable(value: str | None, fallback: str) -> Path:
    candidate = value or shutil.which(fallback)
    if candidate is None:
        raise ValueError(f"cannot find executable: {fallback}")
    path = Path(candidate).expanduser().absolute()
    if not path.is_file() or not os.access(path, os.X_OK):
        raise ValueError(f"not an executable file: {path}")
    return path


def validate_entrypoints(plan: dict[str, Any]) -> None:
    missing = sorted(
        {
            job["entrypoint"]
            for job in plan["jobs"]
            if not Path(job["entrypoint"]).is_file()
        }
    )
    if missing:
        raise ValueError(
            "missing worker entrypoints: "
            + ", ".join(missing)
            + "; build benchmarkWorker/assembly and prepare comparator runtimes"
        )


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--coverage-mode",
        choices=("championship", "diagnostic_subset"),
        default="championship",
    )
    parser.add_argument(
        "--court-id",
        default="grakern-tier1-baseline",
    )
    parser.add_argument("--tier1-id", action="append")
    parser.add_argument(
        "--boundary",
        action="append",
        choices=("public_end_to_end", "prepared_topology"),
    )
    parser.add_argument(
        "--mode",
        action="append",
        choices=("cold", "warm"),
    )
    parser.add_argument(
        "--implementation",
        action="append",
        choices=tuple(IMPLEMENTATION_LANGUAGE),
    )
    parser.add_argument("--expensive-row", action="append", default=[])
    parser.add_argument(
        "--expensive-job",
        action="append",
        default=[],
        metavar="TIER1_ID:IMPLEMENTATION",
        help=(
            "apply the five-fork expensive policy only to one implementation; "
            "repeat for additional jobs"
        ),
    )
    parser.add_argument("--timeout-seconds", type=int, default=1800)
    parser.add_argument("--repository", type=Path, default=REPOSITORY)
    parser.add_argument("--java")
    parser.add_argument(
        "--grakel-python",
        default="/tmp/grakern-grakel-3.11/bin/python",
    )
    parser.add_argument("--julia")
    parser.add_argument(
        "--julia-depot",
        type=Path,
        default=Path("/tmp/grakern-graphkernels-julia"),
    )
    parser.add_argument("--graph4s-source", type=Path)
    parser.add_argument("--gale-source", type=Path)
    return parser.parse_args()


def parse_expensive_jobs(values: Iterable[str]) -> set[tuple[str, str]]:
    result: set[tuple[str, str]] = set()
    for value in values:
        row, separator, implementation = value.partition(":")
        if (
            not separator
            or not row
            or implementation not in IMPLEMENTATION_LANGUAGE
        ):
            raise ValueError(
                "invalid expensive job "
                f"{value!r}; expected TIER1_ID:"
                + "|".join(IMPLEMENTATION_LANGUAGE)
            )
        result.add((row, implementation))
    return result


def main() -> int:
    arguments = parse_arguments()
    repository = arguments.repository.expanduser().resolve()
    tier1 = read_json(repository / "benchmarks" / "court" / "tier1.json")
    graph4s_revision = revision_from_build(repository, "Graph4s")
    gale_revision = revision_from_build(repository, "Gale")
    plan = build_plan(
        repository=repository,
        tier1=tier1,
        court_id=arguments.court_id,
        coverage_mode=arguments.coverage_mode,
        row_ids=set(arguments.tier1_id) if arguments.tier1_id else None,
        boundaries=arguments.boundary,
        modes=arguments.mode,
        implementations=(
            set(arguments.implementation)
            if arguments.implementation
            else None
        ),
        expensive_rows=set(arguments.expensive_row),
        expensive_jobs=parse_expensive_jobs(arguments.expensive_job),
        timeout_seconds=arguments.timeout_seconds,
        java=executable(arguments.java, "java"),
        grakel_python=executable(arguments.grakel_python, "python3"),
        julia=executable(arguments.julia, "julia"),
        julia_depot=arguments.julia_depot.expanduser().resolve(),
        graph4s_source=find_pinned_checkout(
            repository,
            "graph4s",
            graph4s_revision,
            arguments.graph4s_source,
        ),
        gale_source=find_pinned_checkout(
            repository,
            "gale",
            gale_revision,
            arguments.gale_source,
        ),
    )
    validate_entrypoints(plan)
    schema_path = repository / "benchmarks" / "court" / "court-plan.schema.json"
    Draft202012Validator(read_json(schema_path)).validate(plan)
    output = arguments.output.expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(canonical_json(plan), encoding="utf-8")
    print(f"wrote {len(plan['jobs'])} jobs to {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
