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

SAMPLES = [
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


def flattened_gram(kernel: GraphKernel, graphs: list[Any]) -> list[float]:
    matrix = np.asarray(kernel.fit_transform(graphs), dtype=float)
    return matrix.reshape(-1).tolist()


def build_fixture() -> dict[str, Any]:
    graphs = [as_grakel_graph(sample) for sample in SAMPLES]
    rounds = 2
    subtree = GraphKernel(
        kernel=[
            {"name": "weisfeiler_lehman", "n_iter": rounds},
            {"name": "subtree_wl"},
        ],
        normalize=False,
    )
    assignment = GraphKernel(
        kernel={
            "name": "weisfeiler_lehman_optimal_assignment",
            "n_iter": rounds,
        },
        normalize=False,
    )
    return {
        "comparison_policy": {
            "absolute_tolerance": 1.0e-12,
            "authority": "compatibility evidence, not semantic oracle",
            "relative_tolerance": 1.0e-12,
        },
        "gram_shape": [2, 2],
        "oracle": {
            "distribution": "GraKeL",
            "distribution_version": importlib.metadata.version("GraKeL"),
            "module_version": grakel.__version__,
            "numpy_version": np.__version__,
            "python_version": ".".join(platform.python_version_tuple()[:2]),
        },
        "samples": SAMPLES,
        "schema_version": "grakern.grakel-fixture.v1",
        "semantics": {
            "edge_labels": False,
            "normalization": False,
            "rounds": rounds,
            "sample_order": [sample["key"] for sample in SAMPLES],
            "split": {
                "query": [],
                "train": [sample["key"] for sample in SAMPLES],
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
        "wl_optimal_assignment_gram_flat": flattened_gram(
            assignment,
            graphs,
        ),
        "wl_subtree_gram_flat": flattened_gram(
            subtree,
            graphs,
        ),
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
