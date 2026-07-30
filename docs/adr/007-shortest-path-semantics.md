# ADR 007: Shortest-path decomposition semantics

## Status

Accepted for the E6 shortest-path vertical slice.

## Decision

The canonical unweighted shortest-path decomposition emits one pattern for each
reachable unordered pair of distinct vertices:

```text
ShortestPathPart(
  endpoints = UndirectedEndpoints(nodeKey(u), nodeKey(v)),
  distance = positive hop count
)
```

Each graph-local vertex pair occurs exactly once. Self-pairs and disconnected
pairs do not emit a pattern. Equal projected endpoint values do not collapse
different vertex pairs: their occurrences contribute multiplicity to the same
pattern.

Endpoint order is not semantic. `UndirectedEndpoints(a, b)` and
`UndirectedEndpoints(b, a)` have the same universal and Cats equality and hash.
No global ordering is required of node keys.

`HopDistance` is a strictly positive integer. The unweighted recipe therefore
does not use floating-point values or a sentinel for disconnection. A later
weighted recipe will use validated nonnegative finite edge lengths and will
define zero-length paths independently; it must not overload `HopDistance`.

Vertex identity belongs to `ShortestPathWitness`, where it supports provenance.
The feature identity consists only of endpoint descriptors and distance.

The E6 reference implementation materializes Floyd-Warshall distances. This is
not the production lowering. Its purpose is to provide a simple independent
denotation against which the optimized per-source BFS implementation is tested.

## External compatibility

GraKeL 0.1.10 and GraphKernels.jl enumerate ordered off-diagonal vertex pairs for
their shortest-path implementations. For an undirected graph this duplicates
each reachable pair. Plain count-feature kernels therefore differ from this ADR
by a predictable occurrence convention (and, without normalization, a
corresponding scale).

GraphKernels.jl also adds its two endpoint-kernel scores for equal distances,
rather than multiplying endpoint component kernels. That behavior is not the
canonical decomposition denotation adopted here. If supported for parity, it
must be an explicitly named compatibility recipe and is not a semantic oracle.

Parity tests may translate these declared conventions. They must not silently
change the canonical grakern feature identity.

## Consequences

- Vertex reindexing cannot change kernel values.
- Collision-prone hashes may accelerate feature lookup but cannot define path
  identity.
- Categorical feature extraction can count exact
  `(unordered endpoint keys, hop distance)` patterns.
- Optimized BFS, future weighted Dijkstra, and alternative lowerings must agree
  with an independent scalar/reference interpretation on their shared domain.
