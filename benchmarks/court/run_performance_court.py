#!/usr/bin/env python3
"""Run the correctness-locked Grakern performance court.

The workload workers own language-specific setup and measurement. This
orchestrator owns admission, source and host fingerprints, resumability,
artifacts, receipt construction, and cross-implementation classification.
"""

from __future__ import annotations

import argparse
import contextlib
import fcntl
import hashlib
import json
import math
import os
import platform
import random
import re
import statistics
import subprocess
import sys
import tempfile
import time
from collections.abc import Iterator, Sequence
from pathlib import Path
from typing import Any

try:
    import psutil
except ImportError as error:  # pragma: no cover - exercised by CLI users
    raise SystemExit(
        "psutil is required; run with `uv run --with psutil --with jsonschema`"
    ) from error

try:
    from jsonschema import Draft202012Validator
except ImportError as error:  # pragma: no cover - exercised by CLI users
    raise SystemExit(
        "jsonschema is required; run with `uv run --with psutil --with jsonschema`"
    ) from error


SCHEMA_VERSION = "grakern.performance-court-state.v1"
SCRIPT_PATH = Path(__file__).resolve()
COURT_DIRECTORY = SCRIPT_PATH.parent


class CourtError(RuntimeError):
    """An admission or execution failure that invalidates a court run."""


def canonical_json_bytes(value: Any) -> bytes:
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
        )
        + "\n"
    ).encode("utf-8")


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_bytes(canonical_json_bytes(value))
    os.replace(temporary, path)


def read_json(path: Path) -> Any:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def resolve_from(base: Path, value: str) -> Path:
    path = Path(value)
    if not path.is_absolute():
        path = base / path
    return path.resolve()


def validate(instance: Any, schema_path: Path, label: str) -> None:
    ensure_finite_numbers(instance, label)
    schema = read_json(schema_path)
    validator = Draft202012Validator(schema)
    errors = sorted(
        validator.iter_errors(instance),
        key=lambda error: list(error.absolute_path),
    )
    if errors:
        rendered = []
        for error in errors[:20]:
            location = ".".join(str(item) for item in error.absolute_path)
            rendered.append(f"{location or '<root>'}: {error.message}")
        raise CourtError(f"{label} failed schema validation:\n" + "\n".join(rendered))


def ensure_finite_numbers(value: Any, label: str) -> None:
    if isinstance(value, float):
        if not math.isfinite(value):
            raise CourtError(f"{label} contains a nonfinite number")
    elif isinstance(value, dict):
        for child in value.values():
            ensure_finite_numbers(child, label)
    elif isinstance(value, list):
        for child in value:
            ensure_finite_numbers(child, label)


def git(root: Path, *arguments: str) -> str:
    completed = subprocess.run(
        ["git", "-C", str(root), *arguments],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        raise CourtError(
            f"git {' '.join(arguments)} failed for {root}: {completed.stderr.strip()}"
        )
    return completed.stdout


def source_tree_sha256(root: Path) -> str:
    entries = [
        entry for entry in git(root, "ls-files", "-s", "-z").split("\0") if entry
    ]
    digest = hashlib.sha256()
    parsed: list[tuple[str, str]] = []
    for entry in entries:
        metadata, name = entry.split("\t", 1)
        mode = metadata.split(" ", 1)[0]
        parsed.append((name, mode))
    for name, mode in sorted(parsed):
        path = root / name
        if path.is_symlink():
            data = os.readlink(path).encode("utf-8")
            kind = b"symlink"
        else:
            data = path.read_bytes()
            kind = b"file"
        encoded_name = name.encode("utf-8")
        digest.update(len(encoded_name).to_bytes(8, "big"))
        digest.update(encoded_name)
        digest.update(mode.encode("ascii"))
        digest.update(kind)
        digest.update(len(data).to_bytes(8, "big"))
        digest.update(data)
    return digest.hexdigest()


def lock_sha256(root: Path, lock_files: Sequence[str]) -> str:
    digest = hashlib.sha256()
    for value in sorted(lock_files):
        path = resolve_from(root, value)
        if not path.is_relative_to(root):
            raise CourtError(f"lock file escapes source root {root}: {value}")
        if not path.is_file():
            raise CourtError(f"missing lock file for {root}: {value}")
        relative = os.path.relpath(path, root).encode("utf-8")
        data = path.read_bytes()
        digest.update(len(relative).to_bytes(8, "big"))
        digest.update(relative)
        digest.update(len(data).to_bytes(8, "big"))
        digest.update(data)
    return digest.hexdigest()


def source_snapshot(
    component: dict[str, Any],
    plan_directory: Path,
) -> dict[str, Any]:
    root = resolve_from(plan_directory, component["path"])
    if not (root / ".git").exists():
        git(root, "rev-parse", "--git-dir")
    status = git(
        root,
        "status",
        "--porcelain=v1",
        "--untracked-files=all",
    )
    return {
        "revision": git(root, "rev-parse", "HEAD").strip(),
        "tree_sha256": source_tree_sha256(root),
        "clean": status == "",
        "lock_sha256": lock_sha256(root, component["lock_files"]),
        "_root": str(root),
        "_status": status,
    }


def snapshot_sources(
    plan: dict[str, Any],
    plan_directory: Path,
) -> dict[str, dict[str, Any]]:
    snapshots: dict[str, dict[str, Any]] = {}
    for component in plan["source_components"]:
        identifier = component["id"]
        if identifier in snapshots:
            raise CourtError(f"duplicate source component: {identifier}")
        snapshots[identifier] = source_snapshot(component, plan_directory)
    return snapshots


def receipt_sources(
    snapshots: dict[str, dict[str, Any]],
) -> dict[str, dict[str, Any]]:
    return {
        identifier: {
            key: value for key, value in snapshot.items() if not key.startswith("_")
        }
        for identifier, snapshot in snapshots.items()
    }


def sources_equal(
    before: dict[str, dict[str, Any]],
    after: dict[str, dict[str, Any]],
) -> bool:
    return receipt_sources(before) == receipt_sources(after)


def runner_sha256(
    plan: dict[str, Any],
    plan_directory: Path,
) -> str:
    defaults = [
        SCRIPT_PATH,
        COURT_DIRECTORY / "court-plan.schema.json",
        COURT_DIRECTORY / "worker-result.schema.json",
        COURT_DIRECTORY / "receipt.schema.json",
    ]
    configured = [
        resolve_from(plan_directory, value) for value in plan.get("runner_files", [])
    ]
    paths = sorted(set(defaults + configured), key=str)
    digest = hashlib.sha256()
    for path in paths:
        if not path.is_file():
            raise CourtError(f"missing runner file: {path}")
        name = os.path.relpath(path, plan_directory).encode("utf-8")
        data = path.read_bytes()
        digest.update(len(name).to_bytes(8, "big"))
        digest.update(name)
        digest.update(len(data).to_bytes(8, "big"))
        digest.update(data)
    return digest.hexdigest()


def coefficient_of_variation(values: Sequence[float]) -> float:
    if not values:
        raise CourtError("calibration requires at least one value")
    mean = statistics.fmean(values)
    if mean == 0.0:
        return math.inf
    return statistics.pstdev(values) / mean


def compute_calibration(sample_count: int) -> dict[str, float]:
    durations: list[float] = []
    state = 0x9E3779B97F4A7C15
    for sample in range(sample_count):
        started = time.perf_counter_ns()
        current = state ^ sample
        for _ in range(200_000):
            current = (
                current * 6364136223846793005 + 1442695040888963407
            ) & 0xFFFFFFFFFFFFFFFF
        durations.append(float(time.perf_counter_ns() - started))
        state ^= current
    return {
        "median_ns": statistics.median(durations),
        "coefficient_of_variation": coefficient_of_variation(durations),
    }


def macos_power_mode() -> tuple[bool | None, str]:
    completed = subprocess.run(
        ["pmset", "-g", "custom"],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        return None, completed.stderr.strip() or "pmset failed"
    matches = re.findall(r"\blowpowermode\s+(\d+)", completed.stdout)
    if not matches:
        return None, "pmset did not report lowpowermode"
    return all(value == "0" for value in matches), "pmset lowpowermode"


def linux_power_mode() -> tuple[bool | None, str]:
    governors = list(
        Path("/sys/devices/system/cpu").glob("cpu[0-9]*/cpufreq/scaling_governor")
    )
    if not governors:
        return None, "CPU governor files unavailable"
    values = [path.read_text(encoding="utf-8").strip() for path in governors]
    return all(value == "performance" for value in values), ",".join(
        sorted(set(values))
    )


def performance_mode() -> tuple[bool | None, str]:
    if sys.platform == "darwin":
        return macos_power_mode()
    if sys.platform.startswith("linux"):
        return linux_power_mode()
    return None, f"unsupported platform: {sys.platform}"


def thermal_throttling() -> tuple[bool | None, str]:
    if sys.platform == "darwin":
        completed = subprocess.run(
            ["pmset", "-g", "therm"],
            check=False,
            capture_output=True,
            text=True,
        )
        if completed.returncode != 0:
            return None, completed.stderr.strip() or "pmset therm failed"
        output = completed.stdout.strip()
        lowered = output.lower()
        if "no thermal warning level has been recorded" in lowered:
            return False, output
        limits = [
            int(value)
            for value in re.findall(
                r"(?:CPU_Speed_Limit|Scheduler_Limit)\s*=\s*(\d+)",
                output,
            )
        ]
        if limits:
            return any(value < 100 for value in limits), output
        return None, output or "pmset therm returned no interpretable facts"
    if sys.platform.startswith("linux"):
        counters = list(
            Path("/sys/devices/system/cpu").glob(
                "cpu*/thermal_throttle/*_throttle_count"
            )
        )
        if not counters:
            return None, "thermal throttle counters unavailable"
        values = [int(path.read_text(encoding="utf-8").strip()) for path in counters]
        return any(value > 0 for value in values), str(max(values))
    return None, f"unsupported platform: {sys.platform}"


def power_state() -> tuple[bool | None, str]:
    battery = psutil.sensors_battery()
    if battery is None:
        return True, "no battery reported; treated as mains-powered host"
    return bool(battery.power_plugged), "psutil.sensors_battery"


def host_snapshot(policy: dict[str, Any]) -> dict[str, Any]:
    swap_before = psutil.swap_memory().used
    cpu_percent = psutil.cpu_percent(interval=policy["probe_seconds"])
    swap_after = psutil.swap_memory().used
    virtual_memory = psutil.virtual_memory()
    ac_power, ac_evidence = power_state()
    mode, mode_evidence = performance_mode()
    thermal, thermal_evidence = thermal_throttling()
    calibration = compute_calibration(policy["calibration_samples"])
    return {
        "host_id": platform.node() or "unknown-host",
        "cpu": platform.processor() or platform.machine(),
        "physical_cores": psutil.cpu_count(logical=False)
        or psutil.cpu_count(logical=True)
        or 1,
        "memory_bytes": int(virtual_memory.total),
        "ac_power": ac_power,
        "ac_power_evidence": ac_evidence,
        "performance_mode": mode,
        "performance_mode_evidence": mode_evidence,
        "non_court_cpu_percent": float(cpu_percent),
        "free_memory_bytes": int(virtual_memory.available),
        "swap_used_bytes": int(swap_after),
        "swap_probe_growth_bytes": max(0, int(swap_after - swap_before)),
        "memory_pressure": bool(virtual_memory.percent >= 90.0),
        "thermal_or_power_throttling": thermal,
        "thermal_evidence": thermal_evidence,
        "calibration": calibration,
    }


def combine_host_snapshots(
    before: dict[str, Any],
    after: dict[str, Any],
) -> dict[str, Any]:
    return {
        "host_id": before["host_id"],
        "cpu": before["cpu"],
        "physical_cores": before["physical_cores"],
        "memory_bytes": before["memory_bytes"],
        "ac_power": before["ac_power"]
        if before["ac_power"] == after["ac_power"]
        else None,
        "performance_mode": before["performance_mode"]
        if before["performance_mode"] == after["performance_mode"]
        else None,
        "non_court_cpu_percent": max(
            before["non_court_cpu_percent"],
            after["non_court_cpu_percent"],
        ),
        "free_memory_bytes": min(
            before["free_memory_bytes"],
            after["free_memory_bytes"],
        ),
        "swap_growth_bytes": max(
            before["swap_probe_growth_bytes"],
            after["swap_probe_growth_bytes"],
            after["swap_used_bytes"] - before["swap_used_bytes"],
            0,
        ),
        "memory_pressure": bool(before["memory_pressure"] or after["memory_pressure"]),
        "thermal_or_power_throttling": (
            None
            if before["thermal_or_power_throttling"] is None
            or after["thermal_or_power_throttling"] is None
            else bool(
                before["thermal_or_power_throttling"]
                or after["thermal_or_power_throttling"]
            )
        ),
        "other_court_process": False,
        "calibration_before": before["calibration"],
        "calibration_after": after["calibration"],
        "_evidence": {
            "ac_power_before": before["ac_power_evidence"],
            "ac_power_after": after["ac_power_evidence"],
            "performance_mode_before": before["performance_mode_evidence"],
            "performance_mode_after": after["performance_mode_evidence"],
            "thermal_before": before["thermal_evidence"],
            "thermal_after": after["thermal_evidence"],
        },
    }


def host_checks(
    host: dict[str, Any],
    policy: dict[str, Any],
) -> list[dict[str, Any]]:
    before_median = host["calibration_before"]["median_ns"]
    after_median = host["calibration_after"]["median_ns"]
    drift = abs(after_median - before_median) / before_median
    checks = [
        {
            "id": "host.cpu_quiet",
            "passed": host["non_court_cpu_percent"]
            <= policy["maximum_non_court_cpu_percent"],
            "evidence": (
                f"observed={host['non_court_cpu_percent']:.3f}% "
                f"maximum={policy['maximum_non_court_cpu_percent']:.3f}%"
            ),
        },
        {
            "id": "host.free_memory",
            "passed": host["free_memory_bytes"] >= policy["minimum_free_memory_bytes"],
            "evidence": (
                f"observed={host['free_memory_bytes']} "
                f"minimum={policy['minimum_free_memory_bytes']}"
            ),
        },
        {
            "id": "host.swap_stable",
            "passed": host["swap_growth_bytes"] == 0,
            "evidence": f"growth_bytes={host['swap_growth_bytes']}",
        },
        {
            "id": "host.memory_pressure",
            "passed": not host["memory_pressure"],
            "evidence": f"memory_pressure={host['memory_pressure']}",
        },
        {
            "id": "host.calibration_cv",
            "passed": max(
                host["calibration_before"]["coefficient_of_variation"],
                host["calibration_after"]["coefficient_of_variation"],
            )
            <= policy["maximum_calibration_cv"],
            "evidence": (
                "before="
                f"{host['calibration_before']['coefficient_of_variation']:.6f} "
                "after="
                f"{host['calibration_after']['coefficient_of_variation']:.6f}"
            ),
        },
        {
            "id": "host.calibration_drift",
            "passed": drift <= policy["maximum_calibration_drift"],
            "evidence": (
                f"observed={drift:.6f} "
                f"maximum={policy['maximum_calibration_drift']:.6f}"
            ),
        },
    ]
    if policy["require_ac_power"]:
        checks.append(
            {
                "id": "host.ac_power",
                "passed": host["ac_power"] is True,
                "evidence": (
                    f"value={host['ac_power']} "
                    f"before={host['_evidence']['ac_power_before']} "
                    f"after={host['_evidence']['ac_power_after']}"
                ),
            }
        )
    if policy["require_performance_mode"]:
        checks.append(
            {
                "id": "host.performance_mode",
                "passed": host["performance_mode"] is True,
                "evidence": (
                    f"value={host['performance_mode']} "
                    f"before={host['_evidence']['performance_mode_before']} "
                    f"after={host['_evidence']['performance_mode_after']}"
                ),
            }
        )
    if policy["require_thermal_probe"]:
        checks.append(
            {
                "id": "host.not_throttled",
                "passed": host["thermal_or_power_throttling"] is False,
                "evidence": (
                    f"value={host['thermal_or_power_throttling']} "
                    f"before={host['_evidence']['thermal_before']} "
                    f"after={host['_evidence']['thermal_after']}"
                ),
            }
        )
    return checks


def public_host(host: dict[str, Any]) -> dict[str, Any]:
    return {key: value for key, value in host.items() if not key.startswith("_")}


def checks_pass(checks: Sequence[dict[str, Any]]) -> bool:
    return all(check["passed"] for check in checks)


@contextlib.contextmanager
def exclusive_court_lock(output_root: Path) -> Iterator[None]:
    output_root.mkdir(parents=True, exist_ok=True)
    host_lock_path = (
        Path(tempfile.gettempdir()) / f"grakern-performance-court-{os.getuid()}.lock"
    )
    output_lock_path = output_root / ".court.lock"
    with (
        host_lock_path.open("a+", encoding="utf-8") as host_handle,
        output_lock_path.open("a+", encoding="utf-8") as output_handle,
    ):
        try:
            fcntl.flock(
                host_handle.fileno(),
                fcntl.LOCK_EX | fcntl.LOCK_NB,
            )
        except BlockingIOError as error:
            raise CourtError(
                f"another court owned by uid {os.getuid()} is active"
            ) from error
        try:
            try:
                fcntl.flock(
                    output_handle.fileno(),
                    fcntl.LOCK_EX | fcntl.LOCK_NB,
                )
            except BlockingIOError as error:
                raise CourtError(
                    f"another court owns the output root: {output_root}"
                ) from error
            for handle in (host_handle, output_handle):
                handle.seek(0)
                handle.truncate()
                handle.write(f"{os.getpid()}\n")
                handle.flush()
            try:
                yield
            finally:
                fcntl.flock(
                    output_handle.fileno(),
                    fcntl.LOCK_UN,
                )
        finally:
            fcntl.flock(host_handle.fileno(), fcntl.LOCK_UN)


def ensure_output_outside_sources(
    output_root: Path,
    sources: dict[str, dict[str, Any]],
) -> None:
    for identifier, snapshot in sources.items():
        root = Path(snapshot["_root"])
        if output_root == root or output_root.is_relative_to(root):
            raise CourtError(f"output root must be outside source {identifier}: {root}")


def find_tier1_row(
    tier1: dict[str, Any],
    identifier: str,
) -> dict[str, Any]:
    matches = [row for row in tier1["rows"] if row["id"] == identifier]
    if len(matches) != 1:
        raise CourtError(f"Tier-1 row {identifier!r} resolved {len(matches)} times")
    return matches[0]


def validate_expected_output(row: dict[str, Any]) -> None:
    expected = row.get("expected_output")
    if not isinstance(expected, dict):
        raise CourtError(f"Tier-1 row {row['id']} lacks an admitted expected_output")
    required = {
        "shape",
        "sample_order_sha256",
        "comparison",
        "expected_sha256",
    }
    missing = sorted(required - expected.keys())
    if missing:
        raise CourtError(
            f"Tier-1 row {row['id']} expected_output misses: " + ", ".join(missing)
        )
    shape = expected["shape"]
    if (
        not isinstance(shape, list)
        or len(shape) != 2
        or any(not isinstance(dimension, int) or dimension < 0 for dimension in shape)
    ):
        raise CourtError(f"Tier-1 row {row['id']} has an invalid expected shape")
    for field in ("sample_order_sha256", "expected_sha256"):
        if not isinstance(expected[field], str) or not re.fullmatch(
            r"[a-f0-9]{64}",
            expected[field],
        ):
            raise CourtError(f"Tier-1 row {row['id']} has an invalid {field}")
    comparison = expected["comparison"]
    if comparison not in {"exact_ieee754", "elementwise_tolerance"}:
        raise CourtError(
            f"Tier-1 row {row['id']} has invalid comparison {comparison!r}"
        )
    if comparison == "elementwise_tolerance":
        for field in ("absolute_tolerance", "relative_tolerance"):
            value = expected.get(field)
            if (
                not isinstance(value, (int, float))
                or not math.isfinite(value)
                or value < 0
            ):
                raise CourtError(f"Tier-1 row {row['id']} has invalid {field}")


def validate_plan_references(
    plan: dict[str, Any],
    tier1: dict[str, Any],
) -> None:
    if plan["coverage_mode"] == "championship" and set(tier1["boundaries"]) != {
        "public_end_to_end",
        "prepared_topology",
    }:
        raise CourtError(
            "championship manifest must declare exactly both measured boundaries"
        )
    allowed_comparators = {"grakel", "graphkernels_jl"}
    for row in tier1["rows"]:
        validate_expected_output(row)
        unexpected = sorted(set(row["comparators"]) - allowed_comparators)
        if unexpected:
            raise CourtError(
                f"Tier-1 row {row['id']} has unsupported comparators: "
                + ", ".join(unexpected)
            )
        configuration = tier1["kernel_configurations"][row["kernel"]]
        if configuration.get("output_kind") not in {"gram", "cross"}:
            raise CourtError(
                f"Tier-1 kernel {row['kernel']} lacks an explicit output_kind"
            )
    job_ids: set[str] = set()
    coverage: set[tuple[str, str, str, str]] = set()
    for job in plan["jobs"]:
        if job["id"] in job_ids:
            raise CourtError(f"duplicate job id: {job['id']}")
        job_ids.add(job["id"])
        row = find_tier1_row(tier1, job["tier1_id"])
        if (
            job["implementation"] != "grakern"
            and job["implementation"] not in row["comparators"]
        ):
            raise CourtError(f"job {job['id']} uses an ineligible comparator")
        expected_role = (
            "candidate" if job["implementation"] == "grakern" else "external_comparator"
        )
        if job["comparator_role"] != expected_role:
            raise CourtError(
                f"job {job['id']} has role {job['comparator_role']}; "
                f"expected {expected_role}"
            )
        if expected_role == "external_comparator" and not job.get(
            "semantic_translation"
        ):
            raise CourtError(
                f"external comparator job {job['id']} lacks a semantic translation"
            )
        if expected_role == "candidate" and "semantic_translation" in job:
            raise CourtError(
                f"candidate job {job['id']} must not declare a semantic translation"
            )
        expected_language = {
            "grakern": "scala-jvm",
            "grakel": "python",
            "graphkernels_jl": "julia",
        }[job["implementation"]]
        if job["language"] != expected_language:
            raise CourtError(
                f"job {job['id']} has language {job['language']}; "
                f"expected {expected_language}"
            )
        if plan["coverage_mode"] == "championship":
            thread_policy = tier1["thread_policy"]
            if (
                job["application_threads"] != thread_policy["application_threads"]
                or job["blas_threads"] != thread_policy["blas_threads"]
            ):
                raise CourtError(
                    f"championship job {job['id']} violates the Tier-1 thread policy"
                )
        coverage_key = (
            job["tier1_id"],
            job["boundary"],
            job["mode"],
            job["implementation"],
        )
        if coverage_key in coverage:
            raise CourtError(
                "duplicate row/boundary/mode/implementation job: "
                + "/".join(coverage_key)
            )
        coverage.add(coverage_key)
        command_text = "\0".join(job["command"])
        if "{job_json}" not in command_text:
            raise CourtError(f"job {job['id']} command lacks {{job_json}}")
        if "{worker_result_json}" not in command_text:
            raise CourtError(f"job {job['id']} command lacks {{worker_result_json}}")
    if plan["coverage_mode"] == "championship":
        required: set[tuple[str, str, str, str]] = set()
        for row in tier1["rows"]:
            implementations = ["grakern", *row["comparators"]]
            for boundary in tier1["boundaries"]:
                for implementation in implementations:
                    required.add(
                        (
                            row["id"],
                            boundary,
                            "warm",
                            implementation,
                        )
                    )
            for implementation in implementations:
                required.add(
                    (
                        row["id"],
                        "public_end_to_end",
                        "cold",
                        implementation,
                    )
                )
        missing = sorted(required - coverage)
        if missing:
            rendered = ", ".join("/".join(item) for item in missing[:20])
            raise CourtError(
                f"championship plan misses {len(missing)} jobs: {rendered}"
            )


def load_contract(
    plan_path: Path,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Path]]:
    plan_path = plan_path.resolve()
    plan_directory = plan_path.parent
    plan = read_json(plan_path)
    plan_schema = COURT_DIRECTORY / "court-plan.schema.json"
    validate(plan, plan_schema, "court plan")
    paths = {
        "plan": plan_path,
        "tier1": resolve_from(plan_directory, plan["tier1_manifest"]),
        "receipt_schema": resolve_from(
            plan_directory,
            plan["receipt_schema"],
        ),
        "worker_result_schema": resolve_from(
            plan_directory,
            plan["worker_result_schema"],
        ),
    }
    tier1 = read_json(paths["tier1"])
    ensure_finite_numbers(tier1, "Tier-1 manifest")
    validate_plan_references(plan, tier1)
    Draft202012Validator.check_schema(read_json(paths["receipt_schema"]))
    Draft202012Validator.check_schema(read_json(paths["worker_result_schema"]))
    return plan, tier1, paths


def source_checks(
    before: dict[str, dict[str, Any]],
    after: dict[str, dict[str, Any]],
) -> list[dict[str, Any]]:
    checks: list[dict[str, Any]] = []
    for identifier, snapshot in before.items():
        checks.append(
            {
                "id": f"source.{identifier}.clean_before",
                "passed": snapshot["clean"],
                "evidence": snapshot["_status"] or "clean",
            }
        )
    for identifier, snapshot in after.items():
        checks.append(
            {
                "id": f"source.{identifier}.clean_after",
                "passed": snapshot["clean"],
                "evidence": snapshot["_status"] or "clean",
            }
        )
    checks.append(
        {
            "id": "source.unchanged",
            "passed": sources_equal(before, after),
            "evidence": "before and after fingerprints compared",
        }
    )
    return checks


def command_for_job(
    job: dict[str, Any],
    job_json: Path,
    worker_result_json: Path,
) -> list[str]:
    replacements = {
        "job_json": str(job_json),
        "worker_result_json": str(worker_result_json),
    }
    return [argument.format(**replacements) for argument in job["command"]]


def process_rss_bytes(process: psutil.Process) -> int:
    processes = [process]
    try:
        processes.extend(process.children(recursive=True))
    except psutil.Error:
        pass
    total = 0
    for candidate in processes:
        try:
            total += int(candidate.memory_info().rss)
        except psutil.Error:
            pass
    return total


def run_worker(
    job: dict[str, Any],
    job_directory: Path,
    payload: dict[str, Any],
    plan_directory: Path,
) -> tuple[dict[str, Any], dict[str, Path], int]:
    job_directory.mkdir(parents=True, exist_ok=True)
    job_json = job_directory / "job.json"
    worker_result_json = job_directory / "worker-result.json"
    stdout_path = job_directory / "stdout.log"
    stderr_path = job_directory / "stderr.log"
    write_json(job_json, payload)
    command = command_for_job(job, job_json, worker_result_json)
    environment = os.environ.copy()
    environment.update(job.get("environment", {}))
    threads = str(job["application_threads"])
    blas_threads = str(job["blas_threads"])
    environment.update(
        {
            "GRAKERN_COURT_APPLICATION_THREADS": threads,
            "OMP_NUM_THREADS": blas_threads,
            "OPENBLAS_NUM_THREADS": blas_threads,
            "MKL_NUM_THREADS": blas_threads,
            "VECLIB_MAXIMUM_THREADS": blas_threads,
            "JULIA_NUM_THREADS": threads,
        }
    )
    working_directory = (
        resolve_from(plan_directory, job["working_directory"])
        if "working_directory" in job
        else plan_directory
    )
    peak_rss = 0
    with stdout_path.open("wb") as stdout, stderr_path.open("wb") as stderr:
        process = subprocess.Popen(
            command,
            cwd=working_directory,
            env=environment,
            stdout=stdout,
            stderr=stderr,
        )
        observed = psutil.Process(process.pid)
        deadline = time.monotonic() + job["timeout_seconds"]
        while process.poll() is None:
            peak_rss = max(peak_rss, process_rss_bytes(observed))
            if time.monotonic() > deadline:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
                raise CourtError(
                    f"job {job['id']} exceeded {job['timeout_seconds']} seconds"
                )
            time.sleep(0.01)
        peak_rss = max(peak_rss, process_rss_bytes(observed))
        return_code = process.returncode
    if return_code != 0:
        raise CourtError(
            f"job {job['id']} failed with exit code {return_code}; see {stderr_path}"
        )
    if not worker_result_json.is_file():
        raise CourtError(f"job {job['id']} did not create {worker_result_json}")
    return (
        read_json(worker_result_json),
        {
            "job_json": job_json,
            "worker_result": worker_result_json,
            "stdout": stdout_path,
            "stderr": stderr_path,
        },
        peak_rss,
    )


def runtime_fingerprint(
    job: dict[str, Any],
    plan_directory: Path,
) -> tuple[str, str]:
    environment = os.environ.copy()
    environment.update(job.get("environment", {}))
    completed = subprocess.run(
        job["version_command"],
        cwd=(
            resolve_from(plan_directory, job["working_directory"])
            if "working_directory" in job
            else plan_directory
        ),
        env=environment,
        check=False,
        capture_output=True,
    )
    payload = (
        str(completed.returncode).encode("ascii")
        + b"\0"
        + completed.stdout
        + b"\0"
        + completed.stderr
    )
    evidence = (
        completed.stdout.decode("utf-8", errors="replace")
        + completed.stderr.decode("utf-8", errors="replace")
    ).strip()
    if completed.returncode != 0:
        raise CourtError(f"runtime probe failed for {job['id']}: {evidence}")
    return sha256_bytes(payload), evidence


def validate_worker_contract(
    job: dict[str, Any],
    worker: dict[str, Any],
) -> None:
    runtime = worker["runtime"]
    sampling = worker["sampling"]
    metrics = worker["metrics"]
    proof_by_id = {check["id"]: check for check in worker["proof_checks"]}
    if len(proof_by_id) != len(worker["proof_checks"]):
        raise CourtError(f"worker proof check ids are not unique for {job['id']}")
    required_proofs = {
        "semantic.independent_reference",
        "sampling.independent_process_forks",
        "operation.state_reset",
        "boundary.complete",
    }
    missing_proofs = sorted(required_proofs - proof_by_id.keys())
    if missing_proofs:
        raise CourtError(
            f"worker proof checks missing for {job['id']}: " + ", ".join(missing_proofs)
        )
    if runtime["language"] != job["language"]:
        raise CourtError(
            f"worker language mismatch for {job['id']}: "
            f"{runtime['language']} != {job['language']}"
        )
    expected_environment = {
        **job.get("environment", {}),
        "GRAKERN_COURT_APPLICATION_THREADS": str(job["application_threads"]),
        "OMP_NUM_THREADS": str(job["blas_threads"]),
        "OPENBLAS_NUM_THREADS": str(job["blas_threads"]),
        "MKL_NUM_THREADS": str(job["blas_threads"]),
        "VECLIB_MAXIMUM_THREADS": str(job["blas_threads"]),
        "JULIA_NUM_THREADS": str(job["application_threads"]),
    }
    for name, value in expected_environment.items():
        if runtime["environment"].get(name) != value:
            raise CourtError(
                f"worker runtime environment mismatch for {job['id']}: "
                f"{name}={runtime['environment'].get(name)!r}, "
                f"expected {value!r}"
            )
    if (
        job["implementation"] == "grakern"
        and not metrics["allocated_bytes"]["available"]
    ):
        raise CourtError(f"Grakern allocation measurement unavailable for {job['id']}")
    if not metrics.get("phases"):
        raise CourtError(f"worker phase timings unavailable for {job['id']}")
    if sampling["mode"] != job["mode"]:
        raise CourtError(
            f"worker mode mismatch for {job['id']}: {sampling['mode']} != {job['mode']}"
        )
    if len(sampling["durations_ns"]) != sampling["forks"]:
        raise CourtError(f"worker fork count mismatch for {job['id']}")
    if any(
        len(fork) != sampling["samples_per_fork"] for fork in sampling["durations_ns"]
    ):
        raise CourtError(f"worker sample count mismatch for {job['id']}")
    timed_work = sum(duration for fork in sampling["durations_ns"] for duration in fork)
    if timed_work != sampling["timed_work_ns"]:
        raise CourtError(
            f"worker timed-work mismatch for {job['id']}: "
            f"{timed_work} != {sampling['timed_work_ns']}"
        )
    medians = flattened(
        [
            [duration / sampling["batch_size"] for duration in fork]
            for fork in sampling["durations_ns"]
        ]
    )
    expected_median = round(statistics.median(medians))
    if metrics["wall_clock_median_ns"] != expected_median:
        raise CourtError(
            f"worker median mismatch for {job['id']}: "
            f"{metrics['wall_clock_median_ns']} != {expected_median}"
        )
    if sampling["batch_size"] > 1 and expected_median >= 100_000_000:
        raise CourtError(
            f"worker batched an operation at or above 100 ms for {job['id']}"
        )
    if sampling["mode"] == "warm":
        if sampling["policy"] == "standard":
            minima = {
                "forks": 10,
                "warmups_per_fork": 10,
                "samples_per_fork": 10,
            }
        else:
            minima = {
                "forks": 5,
                "warmups_per_fork": 10,
                "samples_per_fork": 5,
            }
        for field, minimum in minima.items():
            if sampling[field] < minimum:
                raise CourtError(
                    f"worker {field} below policy for {job['id']}: "
                    f"{sampling[field]} < {minimum}"
                )
        if timed_work < 30_000_000_000:
            raise CourtError(f"worker timed work below 30 seconds for {job['id']}")
    else:
        expected = {
            "forks": 10,
            "warmups_per_fork": 0,
            "samples_per_fork": 1,
            "batch_size": 1,
        }
        for field, value in expected.items():
            if sampling[field] != value:
                raise CourtError(
                    f"worker cold {field} must equal {value} for {job['id']}: "
                    f"{sampling[field]}"
                )


def artifact(path: Path, output_root: Path, media_type: str) -> dict[str, str]:
    return {
        "path": os.path.relpath(path, output_root),
        "sha256": sha256_file(path),
        "media_type": media_type,
    }


def verify_artifact(
    descriptor: dict[str, str],
    output_root: Path,
    label: str,
) -> None:
    path = (output_root / descriptor["path"]).resolve()
    if not path.is_relative_to(output_root):
        raise CourtError(f"{label} artifact escapes output root: {path}")
    if not path.is_file():
        raise CourtError(f"{label} artifact is missing: {path}")
    observed = sha256_file(path)
    if observed != descriptor["sha256"]:
        raise CourtError(
            f"{label} artifact drift: "
            f"expected={descriptor['sha256']} observed={observed}"
        )


def verify_draft_artifacts(
    draft: dict[str, Any],
    output_root: Path,
) -> None:
    for name, descriptor in draft["artifacts"].items():
        verify_artifact(
            descriptor,
            output_root,
            f"{draft['receipt_id']}:{name}",
        )


def corpus_sha256(
    tier1: dict[str, Any],
    row: dict[str, Any],
) -> str:
    corpus = tier1["corpora"][row["corpus"]]
    value = corpus.get("content_sha256")
    if not isinstance(value, str) or not re.fullmatch(r"[a-f0-9]{64}", value):
        raise CourtError(f"corpus {row['corpus']} lacks an admitted content_sha256")
    return value


def build_receipt_draft(
    plan: dict[str, Any],
    tier1: dict[str, Any],
    paths: dict[str, Path],
    job: dict[str, Any],
    worker: dict[str, Any],
    worker_paths: dict[str, Path],
    peak_rss: int,
    source_before: dict[str, dict[str, Any]],
    source_after: dict[str, dict[str, Any]],
    host: dict[str, Any],
    diagnostic: bool,
    output_root: Path,
    runner_digest: str,
    runtime_before: tuple[str, str],
    runtime_after: tuple[str, str],
) -> dict[str, Any]:
    row = find_tier1_row(tier1, job["tier1_id"])
    source_evidence = source_checks(source_before, source_after)
    host_evidence = host_checks(host, plan["host_policy"])
    worker_checks = worker["proof_checks"]
    semantic = worker["semantic_output"]
    expected_output = row["expected_output"]
    semantic_checks = [
        {
            "id": "semantic.finite",
            "passed": semantic["finite"],
            "evidence": f"finite={semantic['finite']}",
        },
        {
            "id": "semantic.independent_contract",
            "passed": (
                semantic["shape"] == expected_output["shape"]
                and semantic["sample_order_sha256"]
                == expected_output["sample_order_sha256"]
                and semantic["comparison"] == expected_output["comparison"]
                and semantic["expected_sha256"] == expected_output["expected_sha256"]
                and (
                    semantic["comparison"] == "exact_ieee754"
                    or (
                        semantic["absolute_tolerance"]
                        == expected_output["absolute_tolerance"]
                        and semantic["relative_tolerance"]
                        == expected_output["relative_tolerance"]
                    )
                )
            ),
            "evidence": (
                f"worker_shape={semantic['shape']} "
                f"contract_shape={expected_output['shape']} "
                f"worker_order={semantic['sample_order_sha256']} "
                f"contract_order={expected_output['sample_order_sha256']} "
                f"worker_expected={semantic['expected_sha256']} "
                f"contract_expected={expected_output['expected_sha256']}"
            ),
        },
    ]
    if semantic["comparison"] == "exact_ieee754":
        semantic_checks.append(
            {
                "id": "semantic.exact_checksum",
                "passed": semantic["observed_sha256"] == semantic["expected_sha256"],
                "evidence": (
                    f"observed={semantic['observed_sha256']} "
                    f"expected={semantic['expected_sha256']}"
                ),
            }
        )
    else:
        absolute = semantic.get("maximum_absolute_error")
        relative = semantic.get("maximum_relative_error")
        absolute_tolerance = semantic.get("absolute_tolerance")
        relative_tolerance = semantic.get("relative_tolerance")
        tolerance_available = all(
            isinstance(value, (int, float))
            for value in (
                absolute,
                relative,
                absolute_tolerance,
                relative_tolerance,
            )
        )
        semantic_checks.append(
            {
                "id": "semantic.elementwise_tolerance",
                "passed": bool(
                    tolerance_available
                    and (
                        absolute <= absolute_tolerance or relative <= relative_tolerance
                    )
                ),
                "evidence": (
                    f"maximum_absolute_error={absolute} "
                    f"absolute_tolerance={absolute_tolerance} "
                    f"maximum_relative_error={relative} "
                    f"relative_tolerance={relative_tolerance}"
                ),
            }
        )
    runtime_unchanged = runtime_before[0] == runtime_after[0]
    runtime_checks = [
        {
            "id": "runtime.unchanged",
            "passed": runtime_unchanged,
            "evidence": (
                f"before={runtime_before[0]} after={runtime_after[0]} "
                f"before_version={runtime_before[1]!r} "
                f"after_version={runtime_after[1]!r}"
            ),
        }
    ]
    source_admitted = checks_pass(source_evidence + runtime_checks)
    host_admitted = checks_pass(host_evidence)
    semantic_admitted = checks_pass(worker_checks + semantic_checks)
    if diagnostic:
        status = "diagnostic_only"
    elif not source_admitted:
        status = "blocked_source"
    elif not host_admitted:
        status = "blocked_host"
    elif not semantic_admitted:
        status = "semantic_mismatch"
    else:
        status = "admitted"

    proof_log = worker_paths["worker_result"].parent / "proof.json"
    write_json(
        proof_log,
        {
            "source_checks": source_evidence,
            "runtime_checks": runtime_checks,
            "host_checks": host_evidence,
            "worker_checks": worker_checks,
            "semantic_checks": semantic_checks,
            "host_probe_evidence": host["_evidence"],
        },
    )

    metrics = dict(worker["metrics"])
    metrics["peak_rss_bytes"] = max(
        int(metrics["peak_rss_bytes"]),
        peak_rss,
    )
    metrics["peak_rss_delta_bytes"] = max(
        0,
        metrics["peak_rss_bytes"] - int(metrics["runtime_baseline_rss_bytes"]),
    )
    return {
        "schema_version": "grakern.performance-receipt.v1",
        "receipt_id": f"{plan['court_id']}:{job['id']}",
        "created_at": time.strftime(
            "%Y-%m-%dT%H:%M:%SZ",
            time.gmtime(),
        ),
        "status": status,
        "row": {
            "tier1_id": job["tier1_id"],
            "boundary": job["boundary"],
            "kernel_configuration": tier1["kernel_configurations"][row["kernel"]],
            "corpus_id": row["corpus"],
            "corpus_sha256": corpus_sha256(tier1, row),
            "output_kind": tier1["kernel_configurations"][row["kernel"]]["output_kind"],
            "application_threads": job["application_threads"],
            "blas_threads": job["blas_threads"],
        },
        "implementation": {
            "id": job["implementation"],
            "version": job["version"],
            "entrypoint": job["entrypoint"],
            "comparator_role": job["comparator_role"],
            **(
                {"semantic_translation": job["semantic_translation"]}
                if "semantic_translation" in job
                else {}
            ),
        },
        "source": {
            "before": receipt_sources(source_before),
            "after": receipt_sources(source_after),
            "runner_sha256": runner_digest,
            "workload_manifest_sha256": sha256_file(paths["tier1"]),
            "unchanged": sources_equal(source_before, source_after),
        },
        "runtime": {
            **worker["runtime"],
            "fingerprint_before": runtime_before[0],
            "fingerprint_after": runtime_after[0],
            "unchanged": runtime_unchanged,
        },
        "host": public_host(host),
        "admission": {
            "source_admitted": source_admitted,
            "host_admitted": host_admitted,
            "semantic_admitted": semantic_admitted,
            "checks": source_evidence
            + runtime_checks
            + host_evidence
            + worker_checks
            + semantic_checks,
        },
        "semantic_output": semantic,
        "sampling": worker["sampling"],
        "metrics": metrics,
        "artifacts": {
            "worker_job": artifact(
                worker_paths["job_json"],
                output_root,
                "application/json",
            ),
            "raw_samples": artifact(
                worker_paths["worker_result"],
                output_root,
                "application/json",
            ),
            "proof_log": artifact(
                proof_log,
                output_root,
                "application/json",
            ),
            "stdout": artifact(
                worker_paths["stdout"],
                output_root,
                "text/plain",
            ),
            "stderr": artifact(
                worker_paths["stderr"],
                output_root,
                "text/plain",
            ),
        },
    }


def per_operation_samples(receipt: dict[str, Any]) -> list[list[float]]:
    batch = receipt["sampling"]["batch_size"]
    return [
        [duration / batch for duration in fork]
        for fork in receipt["sampling"]["durations_ns"]
    ]


def flattened(values: Sequence[Sequence[float]]) -> list[float]:
    return [value for group in values for value in group]


def percentile(sorted_values: Sequence[float], probability: float) -> float:
    if not sorted_values:
        raise CourtError("cannot compute percentile of no values")
    index = probability * (len(sorted_values) - 1)
    lower = math.floor(index)
    upper = math.ceil(index)
    if lower == upper:
        return sorted_values[lower]
    fraction = index - lower
    return sorted_values[lower] * (1.0 - fraction) + sorted_values[upper] * fraction


def clustered_bootstrap_ratio(
    candidate: dict[str, Any],
    comparator: dict[str, Any],
    policy: dict[str, Any],
) -> tuple[float, list[float]]:
    candidate_forks = per_operation_samples(candidate)
    comparator_forks = per_operation_samples(comparator)
    if not candidate_forks or not comparator_forks:
        raise CourtError("bootstrap requires candidate and comparator forks")
    point = statistics.median(flattened(candidate_forks)) / statistics.median(
        flattened(comparator_forks)
    )
    random_source = random.Random(policy["bootstrap_seed"])
    ratios: list[float] = []
    for _ in range(policy["bootstrap_resamples"]):
        sampled_candidate = [
            candidate_forks[random_source.randrange(len(candidate_forks))]
            for _ in candidate_forks
        ]
        sampled_comparator = [
            comparator_forks[random_source.randrange(len(comparator_forks))]
            for _ in comparator_forks
        ]
        ratios.append(
            statistics.median(flattened(sampled_candidate))
            / statistics.median(flattened(sampled_comparator))
        )
    ratios.sort()
    tail = (1.0 - policy["confidence_level"]) / 2.0
    return point, [
        percentile(ratios, tail),
        percentile(ratios, 1.0 - tail),
    ]


def classification(
    candidate: dict[str, Any],
    comparators: Sequence[dict[str, Any]],
    policy: dict[str, Any],
) -> dict[str, Any]:
    admitted = [
        receipt
        for receipt in comparators
        if receipt["status"] == "admitted" and receipt["admission"]["semantic_admitted"]
    ]
    if not admitted:
        return {
            "outcome": "uncontested",
            "memory_outcome": "unavailable",
        }
    fastest = min(
        admitted,
        key=lambda receipt: receipt["metrics"]["wall_clock_median_ns"],
    )
    ratio, interval = clustered_bootstrap_ratio(
        candidate,
        fastest,
        policy,
    )
    if (
        ratio <= policy["win_median_ratio"]
        and interval[1] <= policy["win_confidence_upper"]
    ):
        outcome = "win"
    elif ratio >= policy["loss_median_ratio"]:
        outcome = "loss"
    else:
        outcome = "tie"
    best_memory = min(
        receipt["metrics"]["peak_rss_delta_bytes"] for receipt in admitted
    )
    candidate_memory = candidate["metrics"]["peak_rss_delta_bytes"]
    material_memory_loss = (
        candidate_memory > best_memory * policy["memory_ratio"]
        and candidate_memory - best_memory > policy["memory_absolute_bytes"]
    )
    return {
        "outcome": outcome,
        "comparator_receipt_id": fastest["receipt_id"],
        "median_ratio": ratio,
        "confidence_level": policy["confidence_level"],
        "confidence_interval": interval,
        "bootstrap_seed": policy["bootstrap_seed"],
        "bootstrap_resamples": policy["bootstrap_resamples"],
        "memory_outcome": ("material_loss" if material_memory_loss else "pass"),
    }


def group_key(receipt: dict[str, Any]) -> tuple[str, str, str]:
    return (
        receipt["row"]["tier1_id"],
        receipt["row"]["boundary"],
        receipt["sampling"]["mode"],
    )


def finalize_receipts(
    drafts: list[dict[str, Any]],
    policy: dict[str, Any],
    receipt_schema: Path,
    output_root: Path,
) -> list[Path]:
    groups: dict[tuple[str, str, str], list[dict[str, Any]]] = {}
    for receipt in drafts:
        groups.setdefault(group_key(receipt), []).append(receipt)
    paths: list[Path] = []
    for receipt in drafts:
        if (
            receipt["status"] == "admitted"
            and receipt["implementation"]["comparator_role"] == "candidate"
            and receipt["sampling"]["mode"] == "warm"
        ):
            peers = groups[group_key(receipt)]
            receipt["classification"] = classification(
                receipt,
                [
                    peer
                    for peer in peers
                    if peer["implementation"]["comparator_role"]
                    == "external_comparator"
                ],
                policy,
            )
        validate(receipt, receipt_schema, "performance receipt")
        safe_name = receipt["receipt_id"].replace(":", "__")
        path = output_root / "receipts" / f"{safe_name}.json"
        write_json(path, receipt)
        paths.append(path)
    return paths


def state_path(output_root: Path) -> Path:
    return output_root / "court-state.json"


def initialize_state(
    plan_path: Path,
    plan: dict[str, Any],
    plan_sha256: str,
    runner_digest: str,
    source_before: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    return {
        "schema_version": SCHEMA_VERSION,
        "court_id": plan["court_id"],
        "plan_path": str(plan_path),
        "plan_sha256": plan_sha256,
        "runner_sha256": runner_digest,
        "source_baseline": source_before,
        "status": "running",
        "completed_jobs": {},
        "receipts": [],
    }


def load_or_initialize_state(
    plan_path: Path,
    plan: dict[str, Any],
    output_root: Path,
    runner_digest: str,
    source_before: dict[str, dict[str, Any]],
    resume: bool,
) -> dict[str, Any]:
    path = state_path(output_root)
    plan_digest = sha256_file(plan_path)
    if not resume:
        if path.exists():
            raise CourtError(
                f"state already exists; use --resume or a new output root: {path}"
            )
        state = initialize_state(
            plan_path,
            plan,
            plan_digest,
            runner_digest,
            source_before,
        )
        write_json(path, state)
        return state
    if not path.is_file():
        raise CourtError(f"cannot resume without state: {path}")
    state = read_json(path)
    expected = {
        "schema_version": SCHEMA_VERSION,
        "court_id": plan["court_id"],
        "plan_sha256": plan_digest,
        "runner_sha256": runner_digest,
    }
    for key, value in expected.items():
        if state.get(key) != value:
            raise CourtError(
                f"resume mismatch for {key}: state={state.get(key)!r} current={value!r}"
            )
    if receipt_sources(state["source_baseline"]) != receipt_sources(source_before):
        raise CourtError("source fingerprint changed since the court began")
    for job_id, completed in state["completed_jobs"].items():
        draft = output_root / completed["draft_path"]
        if not draft.is_file() or sha256_file(draft) != completed["sha256"]:
            raise CourtError(f"completed job artifact drift: {job_id}")
        verify_draft_artifacts(read_json(draft), output_root)
    state["status"] = "running"
    write_json(path, state)
    return state


def worker_payload(
    plan: dict[str, Any],
    tier1: dict[str, Any],
    job: dict[str, Any],
    paths: dict[str, Path],
) -> dict[str, Any]:
    row = find_tier1_row(tier1, job["tier1_id"])
    return {
        "schema_version": "grakern.performance-worker-job.v1",
        "court_id": plan["court_id"],
        "job": job,
        "tier1_row": row,
        "kernel_configuration": tier1["kernel_configurations"][row["kernel"]],
        "corpus": tier1["corpora"][row["corpus"]],
        "expected_output": row["expected_output"],
        "tier1_manifest": str(paths["tier1"]),
    }


def preflight(
    plan_path: Path,
    output_root: Path,
    diagnostic: bool,
) -> dict[str, Any]:
    plan_path = plan_path.resolve()
    output_root = output_root.resolve()
    plan, tier1, paths = load_contract(plan_path)
    initial_source = snapshot_sources(plan, plan_path.parent)
    ensure_output_outside_sources(output_root, initial_source)
    with exclusive_court_lock(output_root):
        source = snapshot_sources(plan, plan_path.parent)
        runner_digest = runner_sha256(plan, plan_path.parent)
        before = host_snapshot(plan["host_policy"])
        after = host_snapshot(plan["host_policy"])
        host = combine_host_snapshots(before, after)
        source_evidence = source_checks(source, source)
        host_evidence = host_checks(host, plan["host_policy"])
        coverage_admitted = plan["coverage_mode"] == "championship"
        coverage_evidence = [
            {
                "id": "plan.championship_coverage",
                "passed": coverage_admitted,
                "evidence": f"coverage_mode={plan['coverage_mode']}",
            }
        ]
        admitted = checks_pass(source_evidence + host_evidence + coverage_evidence)
        result = {
            "court_id": plan["court_id"],
            "diagnostic": diagnostic,
            "admitted": admitted and not diagnostic,
            "source": receipt_sources(source),
            "runner_sha256": runner_digest,
            "tier1_manifest_sha256": sha256_file(paths["tier1"]),
            "source_checks": source_evidence,
            "host": public_host(host),
            "host_checks": host_evidence,
            "plan_checks": coverage_evidence,
            "job_count": len(plan["jobs"]),
            "tier1_row_count": len(tier1["rows"]),
            "coverage_mode": plan["coverage_mode"],
        }
        write_json(output_root / "preflight.json", result)
        if not admitted and not diagnostic:
            failed = [
                check["id"]
                for check in (source_evidence + host_evidence + coverage_evidence)
                if not check["passed"]
            ]
            raise CourtError("preflight admission failed: " + ", ".join(failed))
        return result


def run_court(
    plan_path: Path,
    output_root: Path,
    resume: bool,
    diagnostic: bool,
) -> list[Path]:
    plan_path = plan_path.resolve()
    output_root = output_root.resolve()
    plan, tier1, paths = load_contract(plan_path)
    if plan["coverage_mode"] != "championship" and not diagnostic:
        raise CourtError("diagnostic_subset plans require --diagnostic")
    initial_source = snapshot_sources(plan, plan_path.parent)
    ensure_output_outside_sources(output_root, initial_source)
    with exclusive_court_lock(output_root):
        source_before = snapshot_sources(plan, plan_path.parent)
        runner_digest = runner_sha256(plan, plan_path.parent)
        state = load_or_initialize_state(
            plan_path,
            plan,
            output_root,
            runner_digest,
            source_before,
            resume,
        )
        initial_host = host_snapshot(plan["host_policy"])
        initial_combined = combine_host_snapshots(
            initial_host,
            initial_host,
        )
        initial_source_checks = source_checks(
            source_before,
            source_before,
        )
        initial_host_checks = host_checks(
            initial_combined,
            plan["host_policy"],
        )
        if not diagnostic and not checks_pass(
            initial_source_checks + initial_host_checks
        ):
            state["status"] = "blocked_preflight"
            write_json(state_path(output_root), state)
            failed = [
                check["id"]
                for check in initial_source_checks + initial_host_checks
                if not check["passed"]
            ]
            raise CourtError(
                "run admission failed before first job: " + ", ".join(failed)
            )

        drafts: list[dict[str, Any]] = []
        for job in plan["jobs"]:
            completed = state["completed_jobs"].get(job["id"])
            if completed is not None:
                drafts.append(read_json(output_root / completed["draft_path"]))
                continue
            before_host = host_snapshot(plan["host_policy"])
            before_sources = snapshot_sources(plan, plan_path.parent)
            before_runtime = runtime_fingerprint(
                job,
                plan_path.parent,
            )
            if not diagnostic:
                quick_host = combine_host_snapshots(
                    before_host,
                    before_host,
                )
                checks = source_checks(source_before, before_sources)
                checks += host_checks(quick_host, plan["host_policy"])
                if not checks_pass(checks):
                    state["status"] = "blocked_before_job"
                    state["blocked_job"] = job["id"]
                    state["failed_checks"] = [
                        check for check in checks if not check["passed"]
                    ]
                    write_json(state_path(output_root), state)
                    raise CourtError(f"admission changed before job {job['id']}")

            job_directory = output_root / "jobs" / job["id"]
            worker, worker_paths, peak_rss = run_worker(
                job,
                job_directory,
                worker_payload(plan, tier1, job, paths),
                plan_path.parent,
            )
            validate(
                worker,
                paths["worker_result_schema"],
                f"worker result {job['id']}",
            )
            if worker["job_id"] != job["id"]:
                raise CourtError(
                    f"worker job id mismatch: {worker['job_id']} != {job['id']}"
                )
            validate_worker_contract(job, worker)
            after_sources = snapshot_sources(plan, plan_path.parent)
            after_runtime = runtime_fingerprint(
                job,
                plan_path.parent,
            )
            after_host = host_snapshot(plan["host_policy"])
            host = combine_host_snapshots(before_host, after_host)
            draft = build_receipt_draft(
                plan,
                tier1,
                paths,
                job,
                worker,
                worker_paths,
                peak_rss,
                before_sources,
                after_sources,
                host,
                diagnostic,
                output_root,
                runner_digest,
                before_runtime,
                after_runtime,
            )
            draft_path = job_directory / "receipt-draft.json"
            write_json(draft_path, draft)
            state["completed_jobs"][job["id"]] = {
                "draft_path": os.path.relpath(draft_path, output_root),
                "sha256": sha256_file(draft_path),
                "status": draft["status"],
            }
            write_json(state_path(output_root), state)
            drafts.append(draft)
            if not diagnostic and draft["status"] != "admitted":
                state["status"] = draft["status"]
                state["blocked_job"] = job["id"]
                write_json(state_path(output_root), state)
                raise CourtError(
                    f"job {job['id']} is not admissible: {draft['status']}"
                )

        final_sources = snapshot_sources(plan, plan_path.parent)
        if not sources_equal(source_before, final_sources):
            state["status"] = "blocked_source"
            write_json(state_path(output_root), state)
            raise CourtError("source changed before receipt finalization")
        for draft in drafts:
            verify_draft_artifacts(draft, output_root)
        receipts = finalize_receipts(
            drafts,
            plan["classification_policy"],
            paths["receipt_schema"],
            output_root,
        )
        state["status"] = "diagnostic_complete" if diagnostic else "complete"
        state["receipts"] = [
            {
                "path": os.path.relpath(path, output_root),
                "sha256": sha256_file(path),
            }
            for path in receipts
        ]
        write_json(state_path(output_root), state)
        return receipts


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the correctness-locked Grakern performance court"
    )
    subcommands = parser.add_subparsers(dest="command", required=True)
    for name in ("preflight", "run"):
        command = subcommands.add_parser(name)
        command.add_argument("plan", type=Path)
        command.add_argument("output_root", type=Path)
        command.add_argument(
            "--diagnostic",
            action="store_true",
            help="record evidence without admitting performance claims",
        )
        if name == "run":
            command.add_argument(
                "--resume",
                action="store_true",
                help="resume only when plan, runner, source, and artifacts match",
            )
    return parser


def main(arguments: Sequence[str] | None = None) -> int:
    parsed = build_parser().parse_args(arguments)
    try:
        if parsed.command == "preflight":
            result = preflight(
                parsed.plan,
                parsed.output_root,
                parsed.diagnostic,
            )
            print(json.dumps(result, indent=2, sort_keys=True))
        else:
            receipts = run_court(
                parsed.plan,
                parsed.output_root,
                parsed.resume,
                parsed.diagnostic,
            )
            for receipt in receipts:
                print(receipt)
        return 0
    except CourtError as error:
        print(f"court error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
