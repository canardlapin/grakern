from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path

import jsonschema

WORKLOAD_DIRECTORY = Path(__file__).resolve().parents[1]
GENERATOR_PATH = WORKLOAD_DIRECTORY / "generate_workloads.py"
SPEC = importlib.util.spec_from_file_location(
    "grakern_workloads",
    GENERATOR_PATH,
)
assert SPEC is not None and SPEC.loader is not None
workloads = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(workloads)


class WorkloadOracleSuite(unittest.TestCase):
    def setUp(self) -> None:
        self.pair = workloads.graph(
            "pair",
            [0, 0],
            {(0, 1)},
        )
        self.path = workloads.graph(
            "path",
            [0, 0, 0],
            {(0, 1), (1, 2)},
        )
        self.corpus = {
            "corpus_id": "fixture",
            "provenance": {},
            "query_graphs": [],
            "schema_version": workloads.CORPUS_SCHEMA,
            "training_graphs": [self.pair, self.path],
        }

    def test_wl_subtree_matches_hand_computed_counts(self) -> None:
        _, _, matrix = workloads.expected_matrix(
            {
                "kernel": "wl_subtree",
                "rounds": 1,
            },
            self.corpus,
        )
        self.assertEqual(
            matrix,
            [
                [8.0, 10.0],
                [10.0, 14.0],
            ],
        )

    def test_wl_assignment_matches_hand_computed_intersections(
        self,
    ) -> None:
        _, _, matrix = workloads.expected_matrix(
            {
                "kernel": "wl_optimal_assignment",
                "rounds": 1,
            },
            self.corpus,
        )
        self.assertEqual(
            matrix,
            [
                [4.0, 4.0],
                [4.0, 6.0],
            ],
        )

    def test_shortest_path_matches_unordered_pair_histograms(
        self,
    ) -> None:
        _, _, matrix = workloads.expected_matrix(
            {
                "kernel": "categorical_shortest_path",
            },
            self.corpus,
        )
        self.assertEqual(
            matrix,
            [
                [1.0, 2.0],
                [2.0, 5.0],
            ],
        )

    def test_matrix_checksum_is_big_endian_binary64(self) -> None:
        payload = workloads.matrix_bytes([[1.0, -0.0]])
        self.assertEqual(
            payload.hex(),
            "3ff00000000000000000000000000000",
        )

    def test_synthetic_generation_is_reproducible(self) -> None:
        spec = {
            "generator_schema": "fixture",
            "graph_count": 8,
            "kind": "generated",
            "node_label_cardinality": 3,
            "seed": 42,
            "topology": {
                "connected": True,
                "target_mean_degree": 4,
            },
            "vertex_count": {
                "maximum": 10,
                "minimum": 8,
            },
        }
        first = workloads.synthetic_corpus("fixture", spec)
        second = workloads.synthetic_corpus("fixture", spec)
        self.assertEqual(first, second)
        workloads.validate_corpus(first)

    def test_materialized_corpora_cover_the_declared_regimes(
        self,
    ) -> None:
        schema = json.loads(
            (WORKLOAD_DIRECTORY / "corpus.schema.json").read_text(encoding="utf-8")
        )
        jsonschema.Draft202012Validator.check_schema(schema)
        validator = jsonschema.Draft202012Validator(schema)
        corpora = {
            path.stem: json.loads(path.read_text(encoding="utf-8"))
            for path in (WORKLOAD_DIRECTORY / "corpora").glob("*.json")
        }
        tier1 = json.loads(workloads.TIER1_PATH.read_text(encoding="utf-8"))
        for corpus_id, contract in tier1["corpora"].items():
            corpus_path = workloads.REPOSITORY / contract["path"]
            self.assertEqual(
                workloads.sha256_bytes(corpus_path.read_bytes()),
                contract["content_sha256"],
            )
            self.assertEqual(
                corpora[corpus_id]["corpus_id"],
                corpus_id,
            )
        self.assertEqual(
            len(corpora["mutag-188"]["training_graphs"]),
            188,
        )
        sparse = corpora["synthetic-sparse-label8"]["training_graphs"]
        dense = corpora["synthetic-dense-label2"]["training_graphs"]
        multiplicity = corpora["synthetic-multiplicity"]["training_graphs"]
        regular = corpora["synthetic-regular-uniform"]["training_graphs"]
        novelty = corpora["synthetic-query-novelty"]
        self.assertEqual(len(sparse), 256)
        self.assertEqual(len(dense), 192)
        self.assertEqual(len(multiplicity), 256)
        self.assertEqual(len(regular), 256)
        self.assertEqual(len(novelty["training_graphs"]), 256)
        self.assertEqual(len(novelty["query_graphs"]), 64)
        self.assertTrue(all(len(sample["node_labels"]) == 32 for sample in regular))
        self.assertTrue(all(set(sample["node_labels"]) == {0} for sample in regular))
        for sample in regular:
            degrees = [0] * len(sample["node_labels"])
            for left, right in sample["edges"]:
                degrees[left] += 1
                degrees[right] += 1
            self.assertEqual(set(degrees), {4})
        sparse_density = sum(len(sample["edges"]) for sample in sparse) / sum(
            len(sample["node_labels"]) for sample in sparse
        )
        dense_density = sum(len(sample["edges"]) for sample in dense) / sum(
            len(sample["node_labels"]) for sample in dense
        )
        self.assertGreater(dense_density, sparse_density)
        self.assertTrue(
            all(
                set(sample["node_labels"]) <= set(range(8))
                for sample in novelty["training_graphs"]
            )
        )
        self.assertTrue(
            all(
                any(label >= 8 for label in sample["node_labels"])
                for sample in novelty["query_graphs"]
            )
        )
        for corpus in corpora.values():
            validator.validate(corpus)
            workloads.validate_corpus(corpus)
            for sample in corpus["training_graphs"] + corpus["query_graphs"]:
                self.assertEqual(
                    len(
                        workloads.connected_components(
                            len(sample["node_labels"]),
                            {tuple(value) for value in sample["edges"]},
                        )
                    ),
                    1,
                )

    def test_materialized_expected_outputs_match_the_manifest(
        self,
    ) -> None:
        schema = json.loads(
            (WORKLOAD_DIRECTORY / "expected-output.schema.json").read_text(
                encoding="utf-8"
            )
        )
        jsonschema.Draft202012Validator.check_schema(schema)
        validator = jsonschema.Draft202012Validator(schema)
        tier1 = json.loads(workloads.TIER1_PATH.read_text(encoding="utf-8"))
        for row in tier1["rows"]:
            expected = row["expected_output"]
            metadata_path = workloads.REPOSITORY / expected["artifact"]
            metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
            validator.validate(metadata)
            binary_path = workloads.REPOSITORY / metadata["binary"]["path"]
            payload = binary_path.read_bytes()
            self.assertEqual(
                len(payload),
                expected["shape"][0] * expected["shape"][1] * 8,
            )
            self.assertEqual(
                workloads.sha256_bytes(payload),
                expected["expected_sha256"],
            )
            self.assertEqual(
                metadata["binary"]["sha256"],
                expected["expected_sha256"],
            )
            self.assertEqual(
                metadata["expected_sha256"],
                expected["expected_sha256"],
            )
            self.assertEqual(
                metadata["sample_order_sha256"],
                expected["sample_order_sha256"],
            )
            self.assertEqual(
                workloads.sample_order_sha256(
                    metadata["rows"],
                    metadata["columns"],
                ),
                expected["sample_order_sha256"],
            )
            self.assertEqual(
                len(metadata["rows"]),
                expected["shape"][0],
            )
            self.assertEqual(
                len(metadata["columns"]),
                expected["shape"][1],
            )


if __name__ == "__main__":
    unittest.main()
