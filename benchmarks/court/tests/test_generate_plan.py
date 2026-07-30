from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

from jsonschema import Draft202012Validator

COURT_DIRECTORY = Path(__file__).resolve().parents[1]
REPOSITORY = COURT_DIRECTORY.parents[1]
GENERATOR_PATH = COURT_DIRECTORY / "generate_plan.py"
SPEC = importlib.util.spec_from_file_location(
    "grakern_court_plan",
    GENERATOR_PATH,
)
assert SPEC is not None and SPEC.loader is not None
generator = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(generator)


class PerformancePlanSuite(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.tier1 = json.loads(
            (COURT_DIRECTORY / "tier1.json").read_text(encoding="utf-8")
        )
        cls.schema = json.loads(
            (COURT_DIRECTORY / "court-plan.schema.json").read_text(
                encoding="utf-8"
            )
        )

    def plan(self, **overrides: object) -> dict[str, object]:
        arguments: dict[str, object] = {
            "repository": REPOSITORY,
            "tier1": self.tier1,
            "court_id": "test-court",
            "coverage_mode": "championship",
            "row_ids": None,
            "boundaries": None,
            "modes": None,
            "implementations": None,
            "expensive_rows": set(),
            "expensive_jobs": set(),
            "timeout_seconds": 1800,
            "java": Path("/runtime/java"),
            "grakel_python": Path("/runtime/python"),
            "julia": Path("/runtime/julia"),
            "julia_depot": Path("/runtime/julia-depot"),
            "graph4s_source": Path("/source/graph4s"),
            "gale_source": Path("/source/gale"),
        }
        arguments.update(overrides)
        return generator.build_plan(**arguments)

    def test_championship_plan_has_closed_complete_coverage(self) -> None:
        plan = self.plan()
        Draft202012Validator(self.schema).validate(plan)

        expected = sum(
            3 * (1 + len(row["comparators"]))
            for row in self.tier1["rows"]
        )
        self.assertEqual(len(plan["jobs"]), expected)
        coverage = {
            (
                job["tier1_id"],
                job["boundary"],
                job["mode"],
                job["implementation"],
            )
            for job in plan["jobs"]
        }
        for row in self.tier1["rows"]:
            for implementation in ["grakern", *row["comparators"]]:
                for boundary, mode in (
                    ("public_end_to_end", "warm"),
                    ("prepared_topology", "warm"),
                    ("public_end_to_end", "cold"),
                ):
                    self.assertIn(
                        (
                            row["id"],
                            boundary,
                            mode,
                            implementation,
                        ),
                        coverage,
                    )

    def test_diagnostic_selection_never_emits_cold_prepared_job(self) -> None:
        plan = self.plan(
            coverage_mode="diagnostic_subset",
            row_ids={"t1-wl-subtree-mutag"},
            boundaries=["prepared_topology"],
            modes=["cold", "warm"],
            implementations={"grakern", "grakel"},
        )
        Draft202012Validator(self.schema).validate(plan)

        self.assertEqual(
            {
                (
                    job["boundary"],
                    job["mode"],
                    job["implementation"],
                )
                for job in plan["jobs"]
            },
            {
                ("prepared_topology", "warm", "grakern"),
                ("prepared_topology", "warm", "grakel"),
            },
        )

    def test_external_translation_is_canonical_and_candidate_has_none(
        self,
    ) -> None:
        plan = self.plan(
            coverage_mode="diagnostic_subset",
            row_ids={"t1-shortest-path-mutag"},
            boundaries=["public_end_to_end"],
            modes=["warm"],
        )
        jobs = {job["implementation"]: job for job in plan["jobs"]}

        self.assertNotIn("semantic_translation", jobs["grakern"])
        self.assertEqual(
            jobs["grakel"]["semantic_translation"],
            (
                '{"divide_raw_output_by":4,'
                '"reason":"ordered-pair feature counts"}'
            ),
        )
        self.assertEqual(
            jobs["graphkernels_jl"]["semantic_translation"],
            (
                '{"divide_raw_output_by":8,'
                '"reason":"ordered-pair counts and additive constant '
                'endpoint scores"}'
            ),
        )

    def test_expensive_policy_applies_to_warm_race_group_only(self) -> None:
        plan = self.plan(
            expensive_rows={"t1-shortest-path-mutag"},
        )
        jobs = [
            job
            for job in plan["jobs"]
            if job["tier1_id"] == "t1-shortest-path-mutag"
        ]
        self.assertTrue(
            all(
                job["sampling_policy"] == "expensive_row"
                for job in jobs
                if job["mode"] == "warm"
            )
        )
        self.assertTrue(
            all(
                job["sampling_policy"] == "standard"
                for job in jobs
                if job["mode"] == "cold"
            )
        )

    def test_expensive_job_does_not_reduce_faster_comparators(self) -> None:
        plan = self.plan(
            expensive_jobs={
                ("t1-shortest-path-sparse", "graphkernels_jl"),
            },
        )
        jobs = [
            job
            for job in plan["jobs"]
            if job["tier1_id"] == "t1-shortest-path-sparse"
            and job["mode"] == "warm"
        ]

        self.assertTrue(
            all(
                job["sampling_policy"] == "expensive_row"
                for job in jobs
                if job["implementation"] == "graphkernels_jl"
            )
        )
        self.assertTrue(
            all(
                job["sampling_policy"] == "standard"
                for job in jobs
                if job["implementation"] != "graphkernels_jl"
            )
        )

    def test_executable_keeps_virtual_environment_symlink(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            target = directory / "base-python"
            target.write_text("", encoding="utf-8")
            target.chmod(0o755)
            link = directory / "venv-python"
            link.symlink_to(target)

            self.assertEqual(
                generator.executable(str(link), "unused"),
                link.absolute(),
            )

    def test_championship_rejects_partial_selectors(self) -> None:
        with self.assertRaisesRegex(
            ValueError,
            "selectors require diagnostic_subset",
        ):
            self.plan(row_ids={"t1-wl-subtree-mutag"})

    def test_unknown_expensive_row_fails_closed(self) -> None:
        with self.assertRaisesRegex(
            ValueError,
            "unknown expensive rows",
        ):
            self.plan(expensive_rows={"not-a-tier1-row"})

    def test_unknown_expensive_job_fails_closed(self) -> None:
        with self.assertRaisesRegex(
            ValueError,
            "unknown expensive jobs",
        ):
            self.plan(
                expensive_jobs={
                    ("t1-wl-subtree-mutag", "graphkernels_jl"),
                }
            )

    def test_expensive_job_cli_parser_is_strict(self) -> None:
        self.assertEqual(
            generator.parse_expensive_jobs(
                ["t1-shortest-path-sparse:graphkernels_jl"]
            ),
            {("t1-shortest-path-sparse", "graphkernels_jl")},
        )
        with self.assertRaisesRegex(ValueError, "invalid expensive job"):
            generator.parse_expensive_jobs(["missing-separator"])


if __name__ == "__main__":
    unittest.main()
