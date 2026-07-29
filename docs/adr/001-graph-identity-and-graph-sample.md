# ADR-001: Graph identity and `GraphSample`

Status: Accepted
Date: 2026-07-29

## Context

Graph kernels compare graph structure. A node's payload is not its identity.
Two carbon atoms may have equal `Atom` values while remaining distinct vertices,
and two samples may both use the local identifier `AtomId(1)`.

Grakern needs one graph boundary for the first release. Supporting an open
`GraphSource[G]` adapter before one complete kernel exists would force the
library to standardize graph capabilities that it has not yet exercised.
Graph4s already supplies immutable, finite, simple, loopless graphs and
graph-scoped data fields.

## Decision

The first public graph input is:

```scala
GraphSample[V, N, E]
```

`V` supplies graph-local identity. `N` and `E` are total node and edge values.
Equality of `N` or `E` never changes graph topology.

The preferred constructor receives one `graph4s.Graph[V]` and derives both
fields from that topology:

```scala
GraphSample.build(key, topology)(
  nodeValue = vertex => ...,
  edgeValue = edge => ...
)
```

Construction validates these v0.1 requirements:

- the topology is finite, simple, loopless, and undirected;
- every graph vertex has one node value;
- every graph edge has one edge value;
- the sample key is metadata, not proof of graph content identity.

Different samples may reuse `V` values. Packing assigns disjoint corpus offsets
and records the owning sample for every packed vertex and edge.

`PackedGraphCorpus[N, E]` is private engine state. It may use graph offsets,
CSR adjacency, local integer ordinals, and projected primitive columns. It is
not a second public graph model, and packed ordinals never become semantic
vertex identifiers.

## Rejected alternatives

`UGraph[N, E]` is rejected because equal node values could be mistaken for
equal vertices.

Independent topology and field arguments are rejected as the preferred
constructor because the library would have to reconstruct whether the fields
belong to the same topology.

A generic `GraphSource[G]` is deferred. It may be introduced after the graph4s
slice identifies the smallest capability contract that external adapters need.

## Consequences

Vertex reindexing must preserve every kernel value. Query and training samples
may reuse local identifiers without collision. Public errors identify the
sample key and original graph-local vertex or edge. Only `grakern-graph4s`
depends on graph4s; `grakern-core` remains graph-independent.
