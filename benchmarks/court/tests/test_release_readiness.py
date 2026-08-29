from __future__ import annotations

import importlib.util
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "tools" / "check_release_readiness.py"
SPEC = importlib.util.spec_from_file_location("check_release_readiness", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ReleaseReadinessTest(unittest.TestCase):
    def test_current_source_pin_receipt_is_blocked(self) -> None:
        receipt = MODULE.load_properties(ROOT / "release" / "0.1.0-M1.properties")
        blockers = MODULE.release_blockers(receipt)
        self.assertEqual(len(blockers), 3)
        self.assertTrue(any("graph4s.resolution" in blocker for blocker in blockers))
        self.assertTrue(any("gale.resolution" in blocker for blocker in blockers))

    def test_exact_artifact_closure_is_ready(self) -> None:
        receipt = {
            "release.readiness": "candidate-ready",
            "graph4s.resolution": "artifact-release",
            "gale.resolution": "artifact-candidate",
        }
        self.assertEqual(MODULE.release_blockers(receipt), [])


if __name__ == "__main__":
    unittest.main()
