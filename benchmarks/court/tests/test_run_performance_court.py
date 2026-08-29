from __future__ import annotations

import importlib.util
import json
import platform
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator

COURT_DIRECTORY = Path(__file__).resolve().parents[1]
RUNNER_PATH = COURT_DIRECTORY / "run_performance_court.py"
SPEC = importlib.util.spec_from_file_location("grakern_court", RUNNER_PATH)
assert SPEC is not None and SPEC.loader is not None
court = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(court)


def run(command: list[str], working_directory: Path) -> None:
    subprocess.run(
        command,
        cwd=working_directory,
        check=True,
        capture_output=True,
        text=True,
    )


def timed_receipt(
    identifier: str,
    role: str,
    duration: int,
    memory: int,
) -> dict[str, Any]:
    return {
        "receipt_id": identifier,
        "status": "admitted",
        "implementation": {
            "comparator_role": role,
        },
        "admission": {
            "semantic_admitted": True,
        },
        "sampling": {
            "mode": "warm",
            "batch_sizes": [1 for _ in range(10)],
            "durations_ns": [[duration for _ in range(10)] for _ in range(10)],
        },
        "metrics": {
            "wall_clock_median_ns": duration,
            "peak_rss_delta_bytes": memory,
        },
    }


def cold_invocation(
    job: dict[str, Any],
    fork_index: int,
    elapsed_ns: int,
) -> dict[str, Any]:
    checksum = "c" * 64
    worker = {
        "runtime": {
            "language": job["language"],
            "language_version": "fixture",
            "operating_system": "fixture",
            "architecture": "fixture",
            "environment": {
                "GRAKERN_COURT_APPLICATION_THREADS": "1",
                "OMP_NUM_THREADS": "1",
                "OPENBLAS_NUM_THREADS": "1",
                "MKL_NUM_THREADS": "1",
                "VECLIB_MAXIMUM_THREADS": "1",
                "JULIA_NUM_THREADS": "1",
            },
        },
        "semantic_output": {
            "shape": [1, 1],
            "sample_order_sha256": checksum,
            "comparison": "exact_ieee754",
            "observed_sha256": checksum,
            "expected_sha256": checksum,
            "finite": True,
        },
        "sampling": {
            "mode": "cold",
            "policy": "standard",
            "fork_index": fork_index,
            "warmups": 0,
            "samples": 1,
            "calibration_ns": None,
            "batch_size": 1,
            "timed_work_ns": 1,
            "durations_ns": [1],
        },
        "metrics": {
            "wall_clock_median_ns": 1,
            "cpu_time_ns": {
                "available": False,
                "reason_unavailable": "fixture",
            },
            "runtime_baseline_rss_bytes": 1,
            "peak_rss_bytes": 2,
            "peak_rss_delta_bytes": 1,
            "allocated_bytes": {
                "available": True,
                "value": 1,
            },
            "gc_count": {
                "available": False,
                "reason_unavailable": "fixture",
            },
            "phases": {
                "operation": 1,
            },
        },
        "proof_checks": [
            {
                "id": identifier,
                "passed": True,
                "evidence": "fixture",
            }
            for identifier in (
                "semantic.independent_reference",
                "operation.state_reset",
                "boundary.complete",
            )
        ],
    }
    return {
        "fork_index": fork_index,
        "worker": worker,
        "peak_rss": 2,
        "process": {
            "pid": 1000 + fork_index,
            "started_monotonic_ns": 10_000,
            "finished_monotonic_ns": 10_000 + elapsed_ns,
        },
    }


class CourtContractSuite(unittest.TestCase):
    def test_macos_power_mode_accepts_active_automatic_and_high_profiles(
        self,
    ) -> None:
        cases = {
            "lowpowermode 0": (True, "pmset active lowpowermode=0"),
            "lowpowermode 1": (False, "pmset active lowpowermode=1"),
            "powermode 0": (True, "pmset active powermode=0"),
            "powermode 1": (False, "pmset active powermode=1"),
            "powermode 2": (True, "pmset active powermode=2"),
            "standby 1": (None, "pmset did not report an active power mode"),
        }
        for output, expected in cases.items():
            with self.subTest(output=output):
                self.assertEqual(court.interpret_macos_power_mode(output), expected)

    def test_receipt_samples_use_each_forks_calibrated_batch_size(self) -> None:
        receipt = {
            "receipt_id": "varying-batches",
            "sampling": {
                "batch_sizes": [1, 2],
                "durations_ns": [
                    [100_000_000, 110_000_000],
                    [200_000_000, 220_000_000],
                ],
            },
        }

        self.assertEqual(
            court.per_operation_samples(receipt),
            [
                [100_000_000, 110_000_000],
                [100_000_000, 110_000_000],
            ],
        )

    def test_all_schemas_are_valid_draft_2020_12(self) -> None:
        for name in (
            "court-plan.schema.json",
            "receipt.schema.json",
            "worker-result.schema.json",
        ):
            with (COURT_DIRECTORY / name).open(
                "r",
                encoding="utf-8",
            ) as handle:
                Draft202012Validator.check_schema(json.load(handle))

    def test_row_classification_keeps_ties_and_losses_visible(self) -> None:
        policy = {
            "bootstrap_seed": 42,
            "bootstrap_resamples": 200,
            "confidence_level": 0.99,
            "win_median_ratio": 0.9,
            "win_confidence_upper": 0.95,
            "loss_median_ratio": 1.0,
            "memory_ratio": 1.1,
            "memory_absolute_bytes": 64 * 1024 * 1024,
        }
        comparator = timed_receipt(
            "comparator",
            "external_comparator",
            100,
            100_000_000,
        )

        winner = court.classification(
            timed_receipt("winner", "candidate", 80, 100_000_000),
            [comparator],
            policy,
        )
        tie = court.classification(
            timed_receipt("tie", "candidate", 95, 100_000_000),
            [comparator],
            policy,
        )
        loss = court.classification(
            timed_receipt("loss", "candidate", 100, 100_000_000),
            [comparator],
            policy,
        )
        uncontested = court.classification(
            timed_receipt(
                "uncontested",
                "candidate",
                50,
                100_000_000,
            ),
            [],
            policy,
        )
        memory_loser = court.classification(
            timed_receipt(
                "memory-loser",
                "candidate",
                80,
                300_000_000,
            ),
            [comparator],
            policy,
        )

        self.assertEqual(winner["outcome"], "win")
        self.assertEqual(tie["outcome"], "tie")
        self.assertEqual(loss["outcome"], "loss")
        self.assertEqual(uncontested["outcome"], "uncontested")
        self.assertEqual(
            memory_loser["memory_outcome"],
            "material_loss",
        )

    def test_court_lock_is_host_local_not_output_local(self) -> None:
        with (
            tempfile.TemporaryDirectory() as first_temporary,
            tempfile.TemporaryDirectory() as second_temporary,
        ):
            first = Path(first_temporary)
            second = Path(second_temporary)
            with (
                court.exclusive_court_lock(first),
                self.assertRaisesRegex(
                    court.CourtError,
                    "another court",
                ),
                court.exclusive_court_lock(second),
            ):
                self.fail("second court acquired the host lock")

    def test_rotating_schedule_is_deterministic_and_position_balanced(
        self,
    ) -> None:
        jobs = [
            {
                "id": implementation,
                "implementation": implementation,
                "mode": "warm",
                "sampling_policy": "standard",
            }
            for implementation in (
                "grakern",
                "grakel",
                "graphkernels_jl",
            )
        ]
        group = ("row", "public_end_to_end", "warm")
        first = court.rotating_schedule(jobs, group, 17)
        second = court.rotating_schedule(jobs, group, 17)

        self.assertEqual(first, second)
        self.assertEqual(len(first), 10)
        expected = {job["implementation"] for job in jobs}
        positions = {implementation: [0, 0, 0] for implementation in expected}
        for fork in first:
            self.assertEqual(
                {job["implementation"] for job in fork},
                expected,
            )
            for position, job in enumerate(fork):
                positions[job["implementation"]][position] += 1
        for counts in positions.values():
            self.assertLessEqual(max(counts) - min(counts), 1)

    def test_rotating_schedule_supports_per_implementation_expensive_policy(
        self,
    ) -> None:
        jobs = [
            {
                "id": implementation,
                "implementation": implementation,
                "mode": "warm",
                "sampling_policy": (
                    "expensive_row"
                    if implementation == "graphkernels_jl"
                    else "standard"
                ),
            }
            for implementation in (
                "grakern",
                "grakel",
                "graphkernels_jl",
            )
        ]

        schedule = court.rotating_schedule(
            jobs,
            ("row", "public_end_to_end", "warm"),
            17,
        )
        appearances = {
            implementation: sum(
                job["implementation"] == implementation
                for fork in schedule
                for job in fork
            )
            for implementation in (
                "grakern",
                "grakel",
                "graphkernels_jl",
            )
        }

        self.assertEqual(len(schedule), 10)
        self.assertEqual(appearances["grakern"], 10)
        self.assertEqual(appearances["grakel"], 10)
        self.assertEqual(appearances["graphkernels_jl"], 5)
        self.assertTrue(
            all(
                any(job["implementation"] == "graphkernels_jl" for job in fork)
                for fork in schedule[:5]
            )
        )
        self.assertTrue(
            all(
                all(job["implementation"] != "graphkernels_jl" for job in fork)
                for fork in schedule[5:]
            )
        )

    def test_cold_receipt_uses_runner_observed_process_latency(
        self,
    ) -> None:
        job = {
            "id": "cold-fixture",
            "implementation": "grakern",
            "language": "scala-jvm",
            "mode": "cold",
            "sampling_policy": "standard",
            "application_threads": 1,
            "blas_threads": 1,
        }
        elapsed = [100 + fork_index for fork_index in range(10)]
        worker, _ = court.aggregate_workers(
            job,
            [
                cold_invocation(job, fork_index, duration)
                for fork_index, duration in enumerate(elapsed)
            ],
            execution_order_seed=17,
            execution_order_sha256="d" * 64,
        )

        self.assertEqual(
            worker["sampling"]["durations_ns"],
            [[duration] for duration in elapsed],
        )
        self.assertIn(
            "sampling.cold_process_boundary",
            {check["id"] for check in worker["proof_checks"]},
        )

    def test_warm_batch_is_verified_from_untimed_calibration(
        self,
    ) -> None:
        job = {
            "id": "warm-calibration-fixture",
            "implementation": "grakern",
            "language": "scala-jvm",
            "mode": "warm",
            "sampling_policy": "standard",
            "application_threads": 1,
            "blas_threads": 1,
        }
        invocation = cold_invocation(job, fork_index=0, elapsed_ns=1)
        sampling = invocation["worker"]["sampling"]
        sampling.update(
            {
                "mode": "warm",
                "warmups": 10,
                "samples": 30,
                "calibration_ns": 99_000_000,
                "batch_size": 3,
                "timed_work_ns": 9_000_000_000,
                "durations_ns": [300_000_000 for _ in range(30)],
            }
        )
        invocation["worker"]["metrics"]["wall_clock_median_ns"] = 100_000_000

        court.validate_worker_invocation(
            job,
            invocation["worker"],
            fork_index=0,
        )

        sampling["batch_size"] = 2
        invocation["worker"]["metrics"]["wall_clock_median_ns"] = 150_000_000
        with self.assertRaisesRegex(
            court.CourtError,
            "calibrated batch mismatch",
        ):
            court.validate_worker_invocation(
                job,
                invocation["worker"],
                fork_index=0,
            )

    def test_source_snapshot_detects_content_and_dirty_state(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "lock.txt").write_text("lock\n", encoding="utf-8")
            (root / "source.txt").write_text("one\n", encoding="utf-8")
            run(["git", "init", "-q"], root)
            run(["git", "config", "user.email", "court@example.invalid"], root)
            run(["git", "config", "user.name", "Court Test"], root)
            run(["git", "add", "lock.txt", "source.txt"], root)
            run(["git", "commit", "-qm", "fixture"], root)
            component = {
                "id": "fixture",
                "path": str(root),
                "lock_files": ["lock.txt"],
            }

            clean = court.source_snapshot(component, root)
            (root / "source.txt").write_text("two\n", encoding="utf-8")
            dirty = court.source_snapshot(component, root)

            self.assertTrue(clean["clean"])
            self.assertFalse(dirty["clean"])
            self.assertNotEqual(
                clean["tree_sha256"],
                dirty["tree_sha256"],
            )

    def test_diagnostic_run_is_resumable_and_fails_on_source_drift(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as source_temporary:  # noqa: SIM117
            with tempfile.TemporaryDirectory() as output_temporary:
                source = Path(source_temporary)
                output = Path(output_temporary) / "court"
                worker = source / "worker.py"
                worker.write_text(
                    """
import json
import os
import platform
import sys

job_path, result_path = sys.argv[1:3]
job = json.load(open(job_path, encoding="utf-8"))
contract = job["sampling_contract"]
duration = (
    1_200_000_000
    if contract["policy"] == "expensive_row"
    else 100_000_000
)
durations = [duration * 2 for _ in range(contract["samples"])]
checksum = "a" * 64
result = {
    "schema_version": "grakern.performance-worker-result.v1",
    "job_id": job["job"]["id"],
    "runtime": {
        "language": job["job"]["language"],
        "language_version": platform.python_version(),
        "operating_system": platform.platform(),
        "architecture": platform.machine(),
        "environment": {
            name: os.environ[name]
            for name in [
                "GRAKERN_COURT_APPLICATION_THREADS",
                "OMP_NUM_THREADS",
                "OPENBLAS_NUM_THREADS",
                "MKL_NUM_THREADS",
                "VECLIB_MAXIMUM_THREADS",
                "JULIA_NUM_THREADS"
            ]
        }
    },
    "semantic_output": {
        "shape": [1, 1],
        "sample_order_sha256": checksum,
        "comparison": "exact_ieee754",
        "observed_sha256": checksum,
        "expected_sha256": checksum,
        "finite": True
    },
    "sampling": {
        "mode": "warm",
        "policy": contract["policy"],
        "fork_index": job["execution"]["fork_index"],
        "warmups": contract["warmups"],
        "samples": contract["samples"],
        "calibration_ns": max(1, duration),
        "batch_size": 2,
        "timed_work_ns": sum(durations),
        "durations_ns": durations
    },
    "metrics": {
        "wall_clock_median_ns": duration,
        "cpu_time_ns": {
            "available": False,
            "reason_unavailable": "fixture"
        },
        "runtime_baseline_rss_bytes": 0,
        "peak_rss_bytes": 1,
        "peak_rss_delta_bytes": 1,
        "allocated_bytes": {
            "available": True,
            "value": 1
        },
        "gc_count": {
            "available": False,
            "reason_unavailable": "fixture"
        },
        "phases": {
            "operation": duration
        }
    },
    "proof_checks": [
        {
            "id": proof_id,
            "passed": True,
            "evidence": "fixture"
        }
        for proof_id in [
            "semantic.independent_reference",
            "operation.state_reset",
            "boundary.complete"
        ]
    ]
}
json.dump(result, open(result_path, "w", encoding="utf-8"))
""".lstrip(),
                    encoding="utf-8",
                )
                (source / "lock.txt").write_text(
                    "lock\n",
                    encoding="utf-8",
                )
                content_sha = "b" * 64
                tier1 = {
                    "schema_version": "fixture",
                    "boundaries": [
                        "public_end_to_end",
                        "prepared_topology",
                    ],
                    "corpora": {
                        "fixture": {
                            "content_sha256": content_sha,
                        }
                    },
                    "kernel_configurations": {
                        "fixture": {
                            "kernel": "wl_subtree",
                            "output_kind": "gram",
                        }
                    },
                    "rows": [
                        {
                            "id": "fixture-row",
                            "kernel": "fixture",
                            "corpus": "fixture",
                            "comparators": ["grakel"],
                            "expected_output": {
                                "shape": [1, 1],
                                "sample_order_sha256": "a" * 64,
                                "comparison": "exact_ieee754",
                                "expected_sha256": "a" * 64,
                            },
                        }
                    ],
                }
                (source / "tier1.json").write_text(
                    json.dumps(tier1),
                    encoding="utf-8",
                )
                plan = {
                    "schema_version": "grakern.performance-court-plan.v1",
                    "court_id": "fixture-court",
                    "coverage_mode": "diagnostic_subset",
                    "execution_order_seed": 7,
                    "tier1_manifest": "tier1.json",
                    "receipt_schema": str(COURT_DIRECTORY / "receipt.schema.json"),
                    "worker_result_schema": str(
                        COURT_DIRECTORY / "worker-result.schema.json"
                    ),
                    "source_components": [
                        {
                            "id": "fixture",
                            "path": str(source),
                            "lock_files": ["lock.txt"],
                        }
                    ],
                    "host_policy": {
                        "probe_seconds": 0.001,
                        "maximum_non_court_cpu_percent": 100,
                        "minimum_free_memory_bytes": 0,
                        "maximum_calibration_cv": 100,
                        "maximum_calibration_drift": 100,
                        "calibration_samples": 3,
                        "require_ac_power": False,
                        "require_performance_mode": False,
                        "require_thermal_probe": False,
                    },
                    "classification_policy": {
                        "bootstrap_seed": 42,
                        "bootstrap_resamples": 20000,
                        "confidence_level": 0.99,
                        "win_median_ratio": 0.9,
                        "win_confidence_upper": 0.95,
                        "loss_median_ratio": 1.0,
                        "memory_ratio": 1.1,
                        "memory_absolute_bytes": 67108864,
                    },
                    "jobs": [
                        {
                            "id": "fixture-job",
                            "tier1_id": "fixture-row",
                            "boundary": "public_end_to_end",
                            "mode": "warm",
                            "sampling_policy": "expensive_row",
                            "implementation": "grakern",
                            "language": "scala-jvm",
                            "version": platform.python_version(),
                            "entrypoint": "fixture",
                            "comparator_role": "candidate",
                            "command": [
                                sys.executable,
                                str(worker),
                                "{job_json}",
                                "{worker_result_json}",
                            ],
                            "version_command": [
                                sys.executable,
                                "--version",
                            ],
                            "working_directory": str(source),
                            "timeout_seconds": 30,
                            "application_threads": 1,
                            "blas_threads": 1,
                        },
                        {
                            "id": "fixture-grakel-job",
                            "tier1_id": "fixture-row",
                            "boundary": "public_end_to_end",
                            "mode": "warm",
                            "sampling_policy": "standard",
                            "implementation": "grakel",
                            "language": "python",
                            "version": platform.python_version(),
                            "entrypoint": "fixture",
                            "comparator_role": "external_comparator",
                            "semantic_translation": "identity",
                            "command": [
                                sys.executable,
                                str(worker),
                                "{job_json}",
                                "{worker_result_json}",
                            ],
                            "version_command": [
                                sys.executable,
                                "--version",
                            ],
                            "working_directory": str(source),
                            "timeout_seconds": 30,
                            "application_threads": 1,
                            "blas_threads": 1,
                        },
                    ],
                }
                plan_path = source / "plan.json"
                plan_path.write_text(
                    json.dumps(plan),
                    encoding="utf-8",
                )
                run(["git", "init", "-q"], source)
                run(
                    [
                        "git",
                        "config",
                        "user.email",
                        "court@example.invalid",
                    ],
                    source,
                )
                run(
                    ["git", "config", "user.name", "Court Test"],
                    source,
                )
                run(["git", "add", "."], source)
                run(["git", "commit", "-qm", "fixture"], source)

                first = court.run_court(
                    plan_path,
                    output,
                    resume=False,
                    diagnostic=True,
                )
                second = court.run_court(
                    plan_path,
                    output,
                    resume=True,
                    diagnostic=True,
                )

                self.assertEqual(first, second)
                receipts = [court.read_json(path) for path in first]
                receipt = next(
                    candidate
                    for candidate in receipts
                    if candidate["implementation"]["id"] == "grakern"
                )
                self.assertEqual(receipt["status"], "diagnostic_only")
                self.assertEqual(
                    receipt["semantic_output"]["observed_sha256"],
                    "a" * 64,
                )
                contract_check = next(
                    check
                    for check in receipt["admission"]["checks"]
                    if check["id"] == "semantic.independent_contract"
                )
                self.assertTrue(contract_check["passed"])
                manifests = {
                    candidate["implementation"]["id"]: court.read_json(
                        output / candidate["artifacts"]["worker_job"]["path"]
                    )
                    for candidate in receipts
                }
                schedules = {
                    json.dumps(manifest["group_schedule"])
                    for manifest in manifests.values()
                }
                self.assertEqual(len(schedules), 1)
                group_schedule = next(iter(manifests.values()))["group_schedule"]
                for fork_index, declared_order in enumerate(group_schedule):
                    records = []
                    for implementation, manifest in manifests.items():
                        invocation = next(
                            (
                                candidate
                                for candidate in manifest["invocations"]
                                if candidate["fork_index"] == fork_index
                            ),
                            None,
                        )
                        if invocation is not None:
                            records.append((implementation, invocation))
                    actual_order = [
                        implementation
                        for implementation, _ in sorted(
                            records,
                            key=lambda item: item[1]["process"]["started_monotonic_ns"],
                        )
                    ]
                    self.assertEqual(actual_order, declared_order)
                raw_samples = output / receipt["artifacts"]["raw_samples"]["path"]
                original_raw_samples = raw_samples.read_bytes()
                raw_samples.write_text(
                    "{}\n",
                    encoding="utf-8",
                )
                with self.assertRaisesRegex(
                    court.CourtError,
                    "artifact drift",
                ):
                    court.run_court(
                        plan_path,
                        output,
                        resume=True,
                        diagnostic=True,
                    )
                raw_samples.write_bytes(original_raw_samples)
                invocation_manifest_path = (
                    output / receipt["artifacts"]["worker_job"]["path"]
                )
                invocation_manifest = court.read_json(invocation_manifest_path)
                invocation_result = (
                    invocation_manifest_path.parent
                    / invocation_manifest["invocations"][0]["artifacts"][
                        "worker_result"
                    ]["path"]
                ).resolve()
                original_invocation_result = invocation_result.read_bytes()
                invocation_result.write_text(
                    "{}\n",
                    encoding="utf-8",
                )
                with self.assertRaisesRegex(
                    court.CourtError,
                    "artifact drift",
                ):
                    court.run_court(
                        plan_path,
                        output,
                        resume=True,
                        diagnostic=True,
                    )
                invocation_result.write_bytes(original_invocation_result)
                (source / "lock.txt").write_text(
                    "drift\n",
                    encoding="utf-8",
                )
                with self.assertRaises(court.CourtError):
                    court.run_court(
                        plan_path,
                        output,
                        resume=True,
                        diagnostic=True,
                    )


if __name__ == "__main__":
    unittest.main()
