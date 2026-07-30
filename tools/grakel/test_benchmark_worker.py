from __future__ import annotations

import json
import unittest
from pathlib import Path

import benchmark_worker
import jsonschema

REPOSITORY = Path(__file__).resolve().parents[2]
TIER1_PATH = REPOSITORY / "benchmarks" / "court" / "tier1.json"


class GraKeLBenchmarkWorkerSuite(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.tier1 = json.loads(TIER1_PATH.read_text(encoding="utf-8"))

    def payload(self, row: dict[str, object], boundary: str) -> dict[str, object]:
        kernel_id = row["kernel"]
        corpus_id = row["corpus"]
        assert isinstance(kernel_id, str)
        assert isinstance(corpus_id, str)
        return {
            "job": {
                "boundary": boundary,
                "id": f"test-{row['id']}-{boundary}",
            },
            "kernel_configuration": self.tier1["kernel_configurations"][kernel_id],
            "corpus": self.tier1["corpora"][corpus_id],
            "expected_output": row["expected_output"],
        }

    def test_standard_batching_targets_120_ms_without_batching_boundary(
        self,
    ) -> None:
        self.assertEqual(benchmark_worker.choose_batch_size(1_000_000, "warm"), 120)
        self.assertEqual(
            benchmark_worker.choose_batch_size(99_000_000, "warm"),
            2,
        )
        self.assertEqual(
            benchmark_worker.choose_batch_size(100_000_000, "warm"),
            1,
        )

    def test_every_eligible_row_matches_the_independent_oracle(self) -> None:
        for row in self.tier1["rows"]:
            if "grakel" not in row["comparators"]:
                continue
            for boundary in ("public_end_to_end", "prepared_topology"):
                with self.subTest(row=row["id"], boundary=boundary):
                    payload = self.payload(row, boundary)
                    context = benchmark_worker.load_context(payload)
                    observed, _, rows, columns = benchmark_worker.evaluate_once(context)
                    expected = row["expected_output"]
                    self.assertEqual(
                        list(observed.shape),
                        expected["shape"],
                    )
                    self.assertEqual(
                        benchmark_worker.sample_order_sha256(rows, columns),
                        expected["sample_order_sha256"],
                    )
                    self.assertEqual(
                        benchmark_worker.sha256_bytes(
                            benchmark_worker.matrix_bytes(observed)
                        ),
                        expected["expected_sha256"],
                    )

    def test_shortest_path_translation_is_explicit(self) -> None:
        configuration = {
            "kernel": "categorical_shortest_path",
        }
        self.assertEqual(
            benchmark_worker.translation_divisor(configuration),
            4.0,
        )

    def test_worker_result_conforms_to_the_court_protocol(self) -> None:
        row = self.tier1["rows"][0]
        payload = self.payload(row, "prepared_topology")
        payload["job"]["environment"] = {}
        payload["execution"] = {"fork_index": 0}
        payload["sampling_contract"] = {
            "mode": "cold",
            "policy": "standard",
            "warmups": 0,
            "samples": 1,
        }
        result = benchmark_worker.run_worker(payload)
        schema = json.loads(
            (
                REPOSITORY / "benchmarks" / "court" / "worker-result.schema.json"
            ).read_text(encoding="utf-8")
        )
        jsonschema.Draft202012Validator(schema).validate(result)
        self.assertTrue(all(check["passed"] for check in result["proof_checks"]))


if __name__ == "__main__":
    unittest.main()
