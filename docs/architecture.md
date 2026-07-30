# Architecture walking slice

Grakern keeps mathematical meaning separate from finite execution:

```text
GraphSample
  -> exact decomposition or refinement trace
  -> lawful kernel program
  -> closed lowering
  -> exact sparse features
  -> Gram, cross, operator, provenance
```

## Paired WL

`WLRefinement` owns one session-local plan identity. A combined subtree and
optimal-assignment program refers to that same node twice. The compiler prepares
one exact signature DAG and lowers its two consumers independently:

- subtree coordinates are `(round, exact signature)` counts;
- OA coordinates are `(hierarchy node, multiplicity threshold)` indicators.

The blocks are scaled by the square root of their kernel weights and
concatenated. Row normalization is diagonal scaling. No product distribution,
sum reassociation, or schedule-dependent reduction is permitted.

Query refinement extends an immutable training interner. Shared coordinates
enter the frozen training CSR; query-only coordinates enter
`novelSquaredNorm`. This preserves the complete normalized query diagonal
without inventing cross terms.

## Decomposition and histograms

`Decomposition[A, P]` is a fused finite nonnegative emitter. `map`, `flatMap`,
`filter`, and `reweight` do not require a materialized pattern vector.
Composition is fallible because multiplication can overflow a finite `Double`.

Vertex and edge histograms are the first independent consumers. They emit exact
keys and graph-scoped witnesses, aggregate them into the same generic sparse
feature contract used by WL, and reuse the same Gale Gram/operator lowering.

## Determinism

Parallel Gram execution partitions upper-triangle rows into fixed tiles.
Workers compute independent cells, and every cell retains the original sparse
feature order. Results are assembled in tile order. Changing parallelism or
tile size therefore changes scheduling without reassociating a floating-point
sum; the acceptance court checks bit equality.

## Evidence hierarchy

1. Safe constructors carry the mathematical kernel claims.
2. Independent scalar, LCA, Hungarian, intersection, and threshold
   implementations detect implementation defects.
3. Metamorphic tests cover vertex reindexing, adversarial hashes, immutable
   query overlays, and schedule changes.
4. GraKeL and GraphKernels.jl fixtures detect compatibility drift only on
   explicitly aligned configurations.

The GraphKernels.jl source audit found a neighbor-refinement mismatch, so that
court is restricted to uniform regular graphs where its implementation and
Grakern's exact WL denotation coincide. GraKeL query normalization and OA
transform behavior are likewise excluded where they drop semantics Grakern
deliberately retains.
