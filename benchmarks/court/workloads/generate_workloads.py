#!/usr/bin/env python3
"""Materialize Grakern's neutral Tier-1 corpora and independent oracles."""

from __future__ import annotations

import argparse
import copy
import hashlib
import io
import json
import math
import random
import struct
import urllib.request
import zipfile
from collections import Counter, deque
from collections.abc import Iterable, Sequence
from itertools import pairwise
from pathlib import Path
from typing import Any

REPOSITORY = Path(__file__).resolve().parents[3]
TIER1_PATH = REPOSITORY / "benchmarks" / "court" / "tier1.json"
WORKLOAD_DIRECTORY = REPOSITORY / "benchmarks" / "court" / "workloads"
MUTAG_URL = "https://www.chrsmrrs.com/graphkerneldatasets/MUTAG.zip"
MUTAG_SHA256 = "c419bdc853c367d2d83da4973c45100954ae15e10f5ae2cddde6ca431f8207f6"
CORPUS_SCHEMA = "grakern.neutral-graph-corpus.v1"
EXPECTED_SCHEMA = "grakern.independent-kernel-output.v1"


def canonical_json_bytes(value: Any) -> bytes:
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
        )
        + "\n"
    ).encode()


def pretty_json_bytes(value: Any) -> bytes:
    return (
        json.dumps(
            value,
            indent=2,
            sort_keys=True,
            ensure_ascii=False,
        )
        + "\n"
    ).encode()


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def relative(path: Path) -> str:
    return path.relative_to(REPOSITORY).as_posix()


def edge(left: int, right: int) -> tuple[int, int]:
    if left == right:
        raise ValueError("neutral corpora are loopless")
    return (left, right) if left < right else (right, left)


def validate_graph(graph: dict[str, Any]) -> None:
    labels = graph["node_labels"]
    size = len(labels)
    observed: set[tuple[int, int]] = set()
    for raw_left, raw_right in graph["edges"]:
        if not (0 <= raw_left < size and 0 <= raw_right < size):
            raise ValueError(f"edge endpoint outside graph {graph['key']}")
        candidate = edge(raw_left, raw_right)
        if candidate in observed:
            raise ValueError(f"duplicate edge in graph {graph['key']}")
        observed.add(candidate)
    if graph["edges"] != [list(value) for value in sorted(observed)]:
        raise ValueError(f"edges are not canonical in graph {graph['key']}")


def validate_corpus(corpus: dict[str, Any]) -> None:
    if corpus["schema_version"] != CORPUS_SCHEMA:
        raise ValueError("unknown neutral-corpus schema")
    keys: set[str] = set()
    for split in ("training_graphs", "query_graphs"):
        for graph in corpus[split]:
            validate_graph(graph)
            if graph["key"] in keys:
                raise ValueError(f"duplicate sample key {graph['key']}")
            keys.add(graph["key"])


def graph(
    key: str,
    labels: Sequence[int],
    edges: Iterable[tuple[int, int]],
    *,
    class_label: int | None = None,
) -> dict[str, Any]:
    result: dict[str, Any] = {
        "edges": [list(value) for value in sorted(set(edges))],
        "key": key,
        "node_labels": list(labels),
    }
    if class_label is not None:
        result["class_label"] = class_label
    validate_graph(result)
    return result


def sparse_graphs(
    rng: random.Random,
    count: int,
    minimum_vertices: int,
    maximum_vertices: int,
    mean_degree: float,
    label_cardinality: int,
    key_prefix: str,
) -> list[dict[str, Any]]:
    result = []
    for graph_index in range(count):
        size = rng.randint(minimum_vertices, maximum_vertices)
        order = list(range(size))
        rng.shuffle(order)
        edges = {
            edge(order[index], order[rng.randrange(index)]) for index in range(1, size)
        }
        target = max(size - 1, round(size * mean_degree / 2.0))
        while len(edges) < target:
            left = rng.randrange(size)
            right = rng.randrange(size)
            if left != right:
                edges.add(edge(left, right))
        labels = [rng.randrange(label_cardinality) for _ in range(size)]
        result.append(
            graph(
                f"{key_prefix}-{graph_index:04d}",
                labels,
                edges,
            )
        )
    return result


def dense_graphs(
    rng: random.Random,
    count: int,
    minimum_vertices: int,
    maximum_vertices: int,
    probability: float,
    label_cardinality: int,
    key_prefix: str,
) -> list[dict[str, Any]]:
    result = []
    for graph_index in range(count):
        size = rng.randint(minimum_vertices, maximum_vertices)
        edges = {
            (left, right)
            for left in range(size)
            for right in range(left + 1, size)
            if rng.random() < probability
        }
        components = connected_components(size, edges)
        for left_component, right_component in pairwise(components):
            edges.add(edge(left_component[0], right_component[0]))
        labels = [rng.randrange(label_cardinality) for _ in range(size)]
        result.append(
            graph(
                f"{key_prefix}-{graph_index:04d}",
                labels,
                edges,
            )
        )
    return result


def regular_graphs(
    rng: random.Random,
    count: int,
    size: int,
    degree: int,
    key_prefix: str,
) -> list[dict[str, Any]]:
    if degree != 4 or size < 6:
        raise ValueError("v1 regular generator supports degree four")
    result = []
    for graph_index in range(count):
        order = list(range(size))
        rng.shuffle(order)
        edges = {
            edge(order[index], order[(index + offset) % size])
            for index in range(size)
            for offset in (1, 2)
        }
        result.append(
            graph(
                f"{key_prefix}-{graph_index:04d}",
                [0] * size,
                edges,
            )
        )
    return result


def connected_components(
    size: int,
    edges: Iterable[tuple[int, int]],
) -> list[list[int]]:
    adjacency = [[] for _ in range(size)]
    for left, right in edges:
        adjacency[left].append(right)
        adjacency[right].append(left)
    remaining = set(range(size))
    components = []
    while remaining:
        start = min(remaining)
        queue = [start]
        remaining.remove(start)
        component = []
        while queue:
            current = queue.pop()
            component.append(current)
            for neighbor in adjacency[current]:
                if neighbor in remaining:
                    remaining.remove(neighbor)
                    queue.append(neighbor)
        components.append(sorted(component))
    return components


def query_novelty_corpus(spec: dict[str, Any]) -> dict[str, Any]:
    rng = random.Random(spec["seed"])
    size = spec["vertex_count"]
    topology = spec["topology"]
    training = sparse_graphs(
        rng,
        spec["training_graph_count"],
        size["minimum"],
        size["maximum"],
        topology["target_mean_degree"],
        spec["training_node_label_cardinality"],
        "query-novelty-train",
    )
    query = sparse_graphs(
        rng,
        spec["query_graph_count"],
        size["minimum"],
        size["maximum"],
        topology["target_mean_degree"],
        spec["training_node_label_cardinality"],
        "query-novelty-query",
    )
    base_cardinality = spec["training_node_label_cardinality"]
    for sample in query:
        vertex_order = list(range(len(sample["node_labels"])))
        rng.shuffle(vertex_order)
        novel_count = round(len(vertex_order) * spec["query_novel_label_rate"])
        for index, vertex in enumerate(vertex_order[:novel_count]):
            sample["node_labels"][vertex] = base_cardinality + index % 4
    return {
        "corpus_id": "synthetic-query-novelty",
        "provenance": {
            "generator": "query_novelty_corpus",
            "generator_schema": spec["generator_schema"],
            "seed": spec["seed"],
        },
        "query_graphs": query,
        "schema_version": CORPUS_SCHEMA,
        "training_graphs": training,
    }


def synthetic_corpus(
    identifier: str,
    spec: dict[str, Any],
) -> dict[str, Any]:
    if spec["kind"] == "generated_train_query":
        return query_novelty_corpus(spec)
    rng = random.Random(spec["seed"])
    size = spec["vertex_count"]
    topology = spec["topology"]
    if "regular_degree" in topology:
        samples = regular_graphs(
            rng,
            spec["graph_count"],
            size["minimum"],
            topology["regular_degree"],
            identifier,
        )
    elif "edge_probability" in topology:
        samples = dense_graphs(
            rng,
            spec["graph_count"],
            size["minimum"],
            size["maximum"],
            topology["edge_probability"],
            spec["node_label_cardinality"],
            identifier,
        )
    else:
        samples = sparse_graphs(
            rng,
            spec["graph_count"],
            size["minimum"],
            size["maximum"],
            topology["target_mean_degree"],
            spec["node_label_cardinality"],
            identifier,
        )
    return {
        "corpus_id": identifier,
        "provenance": {
            "generator": "synthetic_corpus",
            "generator_schema": spec["generator_schema"],
            "seed": spec["seed"],
        },
        "query_graphs": [],
        "schema_version": CORPUS_SCHEMA,
        "training_graphs": samples,
    }


def csv_integers(archive: zipfile.ZipFile, name: str) -> list[int]:
    return [
        int(line.strip())
        for line in archive.read(name).decode().splitlines()
        if line.strip()
    ]


def mutag_corpus(archive_bytes: bytes) -> dict[str, Any]:
    if sha256_bytes(archive_bytes) != MUTAG_SHA256:
        raise ValueError("MUTAG archive checksum mismatch")
    with zipfile.ZipFile(io.BytesIO(archive_bytes)) as archive:
        indicators = csv_integers(
            archive,
            "MUTAG/MUTAG_graph_indicator.txt",
        )
        node_labels = csv_integers(
            archive,
            "MUTAG/MUTAG_node_labels.txt",
        )
        graph_labels = csv_integers(
            archive,
            "MUTAG/MUTAG_graph_labels.txt",
        )
        raw_edges = [
            tuple(int(value.strip()) for value in line.split(","))
            for line in archive.read("MUTAG/MUTAG_A.txt").decode().splitlines()
            if line.strip()
        ]
    if len(indicators) != len(node_labels):
        raise ValueError("MUTAG node columns have different lengths")
    graph_nodes: list[list[int]] = [[] for _ in range(len(graph_labels))]
    for global_index, graph_id in enumerate(indicators, start=1):
        graph_nodes[graph_id - 1].append(global_index)
    graph_edges: list[set[tuple[int, int]]] = [set() for _ in graph_nodes]
    local_indices = [
        {global_index: local_index for local_index, global_index in enumerate(nodes)}
        for nodes in graph_nodes
    ]
    for global_left, global_right in raw_edges:
        left_graph = indicators[global_left - 1] - 1
        right_graph = indicators[global_right - 1] - 1
        if left_graph != right_graph:
            raise ValueError("MUTAG edge crosses graph boundaries")
        graph_edges[left_graph].add(
            edge(
                local_indices[left_graph][global_left],
                local_indices[left_graph][global_right],
            )
        )
    samples = []
    for graph_index, nodes in enumerate(graph_nodes):
        samples.append(
            graph(
                f"mutag-{graph_index:03d}",
                [node_labels[node - 1] for node in nodes],
                graph_edges[graph_index],
                class_label=graph_labels[graph_index],
            )
        )
    return {
        "corpus_id": "mutag-188",
        "provenance": {
            "archive_sha256": MUTAG_SHA256,
            "format": "TU Dortmund text",
            "source": MUTAG_URL,
        },
        "query_graphs": [],
        "schema_version": CORPUS_SCHEMA,
        "training_graphs": samples,
    }


def adjacency(sample: dict[str, Any]) -> list[list[int]]:
    result = [[] for _ in sample["node_labels"]]
    for left, right in sample["edges"]:
        result[left].append(right)
        result[right].append(left)
    return [sorted(neighbors) for neighbors in result]


def wl_rows(
    samples: Sequence[dict[str, Any]],
    rounds: int,
) -> list[list[Counter[Any]]]:
    interner: dict[Any, int] = {}

    def intern(signature: Any) -> int:
        observed = interner.get(signature)
        if observed is not None:
            return observed
        identifier = len(interner)
        interner[signature] = identifier
        return identifier

    graph_state: list[tuple[list[list[int]], list[Counter[int]]]] = []
    current_colours: list[list[int]] = []
    for sample in samples:
        neighbors = adjacency(sample)
        colours = [intern(("node", label)) for label in sample["node_labels"]]
        graph_state.append((neighbors, [Counter(colours)]))
        current_colours.append(colours)
    for _ in range(rounds):
        next_colours: list[list[int]] = []
        for graph_index, (neighbors, levels) in enumerate(graph_state):
            previous = current_colours[graph_index]
            refined = [
                intern(
                    (
                        previous[vertex],
                        tuple(
                            sorted(
                                (0, previous[neighbor])
                                for neighbor in neighbors[vertex]
                            )
                        ),
                    )
                )
                for vertex in range(len(previous))
            ]
            levels.append(Counter(refined))
            next_colours.append(refined)
        current_colours = next_colours
    return [levels for _, levels in graph_state]


def counter_dot(
    left: Counter[Any],
    right: Counter[Any],
) -> int:
    if len(left) > len(right):
        left, right = right, left
    return sum(count * right.get(feature, 0) for feature, count in left.items())


def counter_intersection(
    left: Counter[Any],
    right: Counter[Any],
) -> int:
    if len(left) > len(right):
        left, right = right, left
    return sum(min(count, right.get(feature, 0)) for feature, count in left.items())


def wl_kernel(
    left: list[Counter[Any]],
    right: list[Counter[Any]],
    assignment: bool,
) -> int:
    operation = counter_intersection if assignment else counter_dot
    return sum(
        operation(left_level, right_level)
        for left_level, right_level in zip(left, right, strict=True)
    )


def shortest_path_row(sample: dict[str, Any]) -> Counter[int]:
    neighbors = adjacency(sample)
    result: Counter[int] = Counter()
    for source in range(len(neighbors)):
        distances = [-1] * len(neighbors)
        distances[source] = 0
        queue = deque([source])
        while queue:
            current = queue.popleft()
            for neighbor in neighbors[current]:
                if distances[neighbor] < 0:
                    distances[neighbor] = distances[current] + 1
                    queue.append(neighbor)
        for target in range(source + 1, len(neighbors)):
            if distances[target] > 0:
                result[distances[target]] += 1
    return result


def pairwise_matrix(
    rows: Sequence[Any],
    columns: Sequence[Any],
    kernel: Any,
) -> list[list[float]]:
    return [[float(kernel(row, column)) for column in columns] for row in rows]


def expected_matrix(
    configuration: dict[str, Any],
    corpus: dict[str, Any],
) -> tuple[list[str], list[str], list[list[float]]]:
    training = corpus["training_graphs"]
    query = corpus["query_graphs"]
    kernel = configuration["kernel"]
    if kernel == "wl_subtree":
        training_rows = wl_rows(training, configuration["rounds"])
        matrix = pairwise_matrix(
            training_rows,
            training_rows,
            lambda left, right: wl_kernel(left, right, False),
        )
        return (
            [sample["key"] for sample in training],
            [sample["key"] for sample in training],
            matrix,
        )
    if kernel == "wl_optimal_assignment":
        training_rows = wl_rows(training, configuration["rounds"])
        matrix = pairwise_matrix(
            training_rows,
            training_rows,
            lambda left, right: wl_kernel(left, right, True),
        )
        return (
            [sample["key"] for sample in training],
            [sample["key"] for sample in training],
            matrix,
        )
    if kernel == "wl_subtree_cross":
        all_rows = wl_rows(
            [*training, *query],
            configuration["rounds"],
        )
        training_rows = all_rows[: len(training)]
        query_rows = all_rows[len(training) :]
        matrix = pairwise_matrix(
            query_rows,
            training_rows,
            lambda left, right: wl_kernel(left, right, False),
        )
        return (
            [sample["key"] for sample in query],
            [sample["key"] for sample in training],
            matrix,
        )
    if kernel == "categorical_shortest_path":
        training_rows = [shortest_path_row(sample) for sample in training]
        matrix = pairwise_matrix(
            training_rows,
            training_rows,
            counter_dot,
        )
        return (
            [sample["key"] for sample in training],
            [sample["key"] for sample in training],
            matrix,
        )
    raise ValueError(f"unsupported Tier-1 kernel {kernel}")


def matrix_bytes(matrix: Sequence[Sequence[float]]) -> bytes:
    result = bytearray()
    width: int | None = None
    for row in matrix:
        if width is None:
            width = len(row)
        elif len(row) != width:
            raise ValueError("matrix is not rectangular")
        for value in row:
            if not math.isfinite(value):
                raise ValueError("oracle matrix contains a nonfinite value")
            canonical = 0.0 if value == 0.0 else value
            if canonical < 0.0 or canonical > 2**53:
                raise ValueError("exact Tier-1 output is outside its domain")
            result.extend(struct.pack(">d", canonical))
    return bytes(result)


def sample_order_sha256(
    rows: Sequence[str],
    columns: Sequence[str],
) -> str:
    return sha256_bytes(
        canonical_json_bytes(
            {
                "columns": list(columns),
                "rows": list(rows),
            }
        )
    )


def semantic_alignment(
    row: dict[str, Any],
    configuration: dict[str, Any],
) -> dict[str, Any]:
    kernel = configuration["kernel"]
    result: dict[str, Any] = {
        "grakern": {
            "translation": "identity",
        }
    }
    if "grakel" in row["comparators"]:
        if kernel == "wl_subtree":
            grakel = {
                "kernel": [
                    {
                        "name": "weisfeiler_lehman",
                        "n_iter": configuration["rounds"],
                    },
                    {"name": "subtree_wl"},
                ],
                "normalize": False,
                "translation": "identity",
            }
        elif kernel == "wl_optimal_assignment":
            grakel = {
                "kernel": {
                    "name": "weisfeiler_lehman_optimal_assignment",
                    "n_iter": configuration["rounds"],
                },
                "normalize": False,
                "translation": "identity",
            }
        elif kernel == "wl_subtree_cross":
            grakel = {
                "kernel": [
                    {
                        "name": "weisfeiler_lehman",
                        "n_iter": configuration["rounds"],
                    },
                    {"name": "subtree_wl"},
                ],
                "normalize": False,
                "translation": "identity",
            }
        else:
            grakel = {
                "algorithm_type": "floyd_warshall",
                "kernel": "shortest_path",
                "normalize": False,
                "translation": {
                    "divide_raw_output_by": 4,
                    "reason": "ordered-pair feature counts",
                },
                "with_labels": False,
            }
        result["grakel"] = grakel
    if "graphkernels_jl" in row["comparators"]:
        if kernel == "wl_subtree":
            result["graphkernels_jl"] = {
                "base_kernel": "exact vertex histogram",
                "num_iterations": configuration["rounds"] + 1,
                "scope": "uniform-labeled regular graphs only",
                "translation": "identity",
            }
        else:
            result["graphkernels_jl"] = {
                "dist_key": None,
                "kernel": "ShortestPathGraphKernel",
                "translation": {
                    "divide_raw_output_by": 8,
                    "reason": (
                        "ordered-pair counts and additive constant endpoint scores"
                    ),
                },
                "vertex_kernel": "ConstVertexKernel(1.0)",
            }
    return result


def build_artifacts(
    tier1: dict[str, Any],
    mutag_archive: bytes,
) -> tuple[dict[Path, bytes], bytes]:
    updated = copy.deepcopy(tier1)
    corpora: dict[str, dict[str, Any]] = {}
    artifacts: dict[Path, bytes] = {}
    for identifier, spec in updated["corpora"].items():
        corpus = (
            mutag_corpus(mutag_archive)
            if identifier == "mutag-188"
            else synthetic_corpus(identifier, spec)
        )
        validate_corpus(corpus)
        payload = canonical_json_bytes(corpus)
        path = WORKLOAD_DIRECTORY / "corpora" / f"{identifier}.json"
        artifacts[path] = payload
        spec["content_sha256"] = sha256_bytes(payload)
        spec["path"] = relative(path)
        spec.pop("content_digest_required", None)
        corpora[identifier] = corpus

    for row in updated["rows"]:
        configuration = updated["kernel_configurations"][row["kernel"]]
        rows, columns, matrix = expected_matrix(
            configuration,
            corpora[row["corpus"]],
        )
        raw = matrix_bytes(matrix)
        binary_path = WORKLOAD_DIRECTORY / "expected" / f"{row['id']}.f64be"
        metadata_path = WORKLOAD_DIRECTORY / "expected" / f"{row['id']}.json"
        expected_sha256 = sha256_bytes(raw)
        order_sha256 = sample_order_sha256(rows, columns)
        shape = [len(rows), len(columns)]
        metadata = {
            "binary": {
                "byte_order": "big",
                "format": "row-major IEEE-754 binary64",
                "path": relative(binary_path),
                "sha256": expected_sha256,
            },
            "columns": columns,
            "comparison": "exact_ieee754",
            "expected_sha256": expected_sha256,
            "independent_oracle": ("benchmarks/court/workloads/generate_workloads.py"),
            "row_id": row["id"],
            "rows": rows,
            "sample_order_sha256": order_sha256,
            "schema_version": EXPECTED_SCHEMA,
            "shape": shape,
        }
        artifacts[binary_path] = raw
        artifacts[metadata_path] = pretty_json_bytes(metadata)
        row["expected_output"] = {
            "artifact": relative(metadata_path),
            "comparison": "exact_ieee754",
            "expected_sha256": expected_sha256,
            "sample_order_sha256": order_sha256,
            "shape": shape,
        }
        row["semantic_alignment"] = semantic_alignment(
            row,
            configuration,
        )
    return artifacts, pretty_json_bytes(updated)


def read_archive(path: Path | None) -> bytes:
    if path is not None:
        return path.read_bytes()
    with urllib.request.urlopen(MUTAG_URL) as response:
        return response.read()


def compare_or_write(
    artifacts: dict[Path, bytes],
    tier1_bytes: bytes,
    check: bool,
) -> None:
    expected = {**artifacts, TIER1_PATH: tier1_bytes}
    if check:
        drift = [
            path
            for path, payload in expected.items()
            if not path.is_file() or path.read_bytes() != payload
        ]
        if drift:
            rendered = "\n".join(str(path) for path in drift)
            raise SystemExit(f"workload artifact drift:\n{rendered}")
        return
    for path, payload in expected.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary = path.with_suffix(path.suffix + ".tmp")
        temporary.write_bytes(payload)
        temporary.replace(path)


def main(arguments: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Materialize deterministic neutral corpora and independent "
            "Tier-1 graph-kernel outputs"
        )
    )
    parser.add_argument(
        "--mutag-archive",
        type=Path,
        help=(
            "pinned MUTAG.zip; omit to download the source URL recorded in tier1.json"
        ),
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if a committed corpus, oracle, or tier1.json differs",
    )
    parsed = parser.parse_args(arguments)
    tier1 = json.loads(TIER1_PATH.read_text(encoding="utf-8"))
    artifacts, tier1_bytes = build_artifacts(
        tier1,
        read_archive(parsed.mutag_archive),
    )
    compare_or_write(artifacts, tier1_bytes, parsed.check)
    print(
        json.dumps(
            {
                "artifacts": len(artifacts) + 1,
                "mode": "check" if parsed.check else "write",
                "mutag_sha256": MUTAG_SHA256,
                "tier1_rows": len(tier1["rows"]),
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
