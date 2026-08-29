from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "check_baseline_update.py"
SPEC = importlib.util.spec_from_file_location("check_baseline_update", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
baseline_guard = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(baseline_guard)


class BaselineUpdateSuite(unittest.TestCase):
    def test_non_baseline_change_needs_no_receipt(self) -> None:
        self.assertFalse(baseline_guard.missing_receipt({"engine-jvm/Foo.scala"}))

    def test_baseline_change_without_report_fails(self) -> None:
        self.assertTrue(
            baseline_guard.missing_receipt(
                {"benchmarks/court/baselines/query-overlay.json"}
            )
        )

    def test_baseline_change_with_report_passes(self) -> None:
        self.assertFalse(
            baseline_guard.missing_receipt(
                {
                    "benchmarks/court/baselines/query-overlay.json",
                    "benchmarks/court/reports/2026-08-28-query-overlay.md",
                }
            )
        )


if __name__ == "__main__":
    unittest.main()
