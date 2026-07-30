#!/usr/bin/env python3
"""Generate or verify GraKeL compatibility fixtures."""

from __future__ import annotations

import argparse
import importlib.metadata
import json
import platform
from pathlib import Path
from typing import Any

import grakel
import numpy as np
from grakel import GraphKernel


REPOSITORY = Path(__file__).resolve().parents[2]
DEFAULT_OUTPUT = (
    REPOSITORY
    / "engine-jvm"
    / "src"
    / "test"
    / "resources"
    / "grakel"
    / "0.1.10"
    / "wl_pair_path.json"
)

TRAIN_SAMPLES = [
    {
        "key": "pair",
        "edges": [[0, 1]],
        "node_labels": ["C", "C"],
    },
    {
        "key": "path",
        "edges": [[0, 1], [1, 2]],
        "node_labels": ["C", "O", "C"],
    },
]

QUERY_SAMPLES = [
    {
        "key": "single",
        "edges": [],
        "node_labels": ["C"],
    },
    {
        "key": "triangle",
        "edges": [[0, 1], [1, 2], [0, 2]],
        "node_labels": ["C", "C", "O"],
    },
]


def as_grakel_graph(sample: dict[str, Any]) -> tuple[dict[int, list[int]], dict[int, str]]:
    labels = {
        index: value
        for index, value in enumerate(sample["node_labels"])
    }
    adjacency = {index: [] for index in labels}
    for left, right in sample["edges"]:
        adjacency[left].append(right)
        adjacency[right].append(left)
    return adjacency, labels


def flattened_views(
    kernel: GraphKernel,
    training: list[Any],
    query: list[Any],
) -> tuple[list[float], list[float]]:
    gram = np.asarray(kernel.fit_transform(training), dtype=float)
    cross = np.asarray(kernel.transform(query), dtype=float)
    return gram.reshape(-1).tolist(), cross.reshape(-1).tolist()


def build_fixture() -> dict[str, Any]:
    training = [as_grakel_graph(sample) for sample in TRAIN_SAMPLES]
    query = [as_grakel_graph(sample) for sample in QUERY_SAMPLES]
    rounds = 2
    subtree_spec = [
        {"name": "weisfeiler_lehman", "n_iter": rounds},
        {"name": "subtree_wl"},
    ]
    assignment_spec = {
        "name": "weisfeiler_lehman_optimal_assignment",
        "n_iter": rounds,
    }
    subtree_gram, subtree_cross = flattened_views(
        GraphKernel(kernel=subtree_spec, normalize=False),
        training,
        query,
    )
    normalized_subtree_gram, normalized_subtree_cross = flattened_views(
        GraphKernel(kernel=subtree_spec, normalize=True),
        training,
        query,
    )
    assignment_gram, assignment_cross = flattened_views(
        GraphKernel(kernel=assignment_spec, normalize=False),
        training,
        query,
    )
    normalized_assignment_gram, normalized_assignment_cross = flattened_views(
        GraphKernel(kernel=assignment_spec, normalize=True),
        training,
        query,
    )
    return {
        "comparison_policy": {
            "absolute_tolerance": 1.0e-12,
            "authority": "compatibility evidence, not semantic oracle",
            "relative_tolerance": 1.0e-12,
        },
        "cross_shape": [len(QUERY_SAMPLES), len(TRAIN_SAMPLES)],
        "exclusions": [
            {
                "reason": (
                    "GraKeL normalization does not retain the complete "
                    "query-only WL feature norm required by Grakern"
                ),
                "view": "wl_subtree_normalized_cross",
            },
            {
                "reason": (
                    "GraKeL transform-time handling of query-only WL colours "
                    "does not preserve Grakern's hierarchy-ancestor contract"
                ),
                "view": "wl_optimal_assignment_cross",
            },
            {
                "reason": (
                    "inherits the excluded unnormalized optimal-assignment "
                    "cross semantics"
                ),
                "view": "wl_optimal_assignment_normalized_cross",
            },
        ],
        "gram_shape": [2, 2],
        "oracle": {
            "distribution": "GraKeL",
            "distribution_version": importlib.metadata.version("GraKeL"),
            "module_version": grakel.__version__,
            "numpy_version": np.__version__,
            "python_version": ".".join(platform.python_version_tuple()[:2]),
        },
        "query_samples": QUERY_SAMPLES,
        "schema_version": "grakern.grakel-fixture.v2",
        "semantics": {
            "edge_labels": False,
            "normalization": ["false", "true"],
            "rounds": rounds,
            "query_order": [sample["key"] for sample in QUERY_SAMPLES],
            "sample_order": [sample["key"] for sample in TRAIN_SAMPLES],
            "split": {
                "query": [sample["key"] for sample in QUERY_SAMPLES],
                "train": [sample["key"] for sample in TRAIN_SAMPLES],
            },
            "wl_optimal_assignment": {
                "name": "weisfeiler_lehman_optimal_assignment",
                "n_iter": rounds,
            },
            "wl_subtree": [
                {"name": "weisfeiler_lehman", "n_iter": rounds},
                {"name": "subtree_wl"},
            ],
        },
        "samples": TRAIN_SAMPLES,
        "wl_optimal_assignment_cross_flat": assignment_cross,
        "wl_optimal_assignment_gram_flat": assignment_gram,
        "wl_optimal_assignment_normalized_cross_flat": normalized_assignment_cross,
        "wl_optimal_assignment_normalized_gram_flat": normalized_assignment_gram,
        "wl_subtree_cross_flat": subtree_cross,
        "wl_subtree_gram_flat": subtree_gram,
        "wl_subtree_normalized_cross_flat": normalized_subtree_cross,
        "wl_subtree_normalized_gram_flat": normalized_subtree_gram,
    }


def render(fixture: dict[str, Any]) -> str:
    return json.dumps(fixture, indent=2, sort_keys=True) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    action = parser.add_mutually_exclusive_group()
    action.add_argument(
        "--check",
        action="store_true",
        help="fail if the committed fixture differs from GraKeL",
    )
    action.add_argument(
        "--write",
        action="store_true",
        help="replace the fixture with newly generated output",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
    )
    arguments = parser.parse_args()
    output = render(build_fixture())

    if arguments.check:
        committed = arguments.output.read_text(encoding="utf-8")
        if committed != output:
            raise SystemExit(
                f"GraKeL fixture drift: {arguments.output}"
            )
        return 0
    if arguments.write:
        arguments.output.parent.mkdir(parents=True, exist_ok=True)
        arguments.output.write_text(output, encoding="utf-8")
        return 0

    print(output, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
