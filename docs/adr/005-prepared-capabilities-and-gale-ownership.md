# ADR-005: Prepared capabilities and Gale ownership

Status: Accepted
Date: 2026-07-29

## Context

Some kernels have exact finite features. Others only have a pairwise evaluator
or an implicit operator. Feature availability can depend on the corpus,
lowering, approximation policy, and memory budget, so it is not a permanent
type parameter of `Kernel[A]`.

Prepared results also combine numerical payloads with kernel-specific identity,
alignment, and provenance. Those responsibilities need one owner.

## Decision

An immutable prepared kernel offers these views:

```text
gram
cross(query)
operator
features
capabilities
planReport
```

`features` returns a typed unavailable result when the selected exact plan has
no exact finite feature realization.

An operator is semantically available for every finite training corpus. The
plan may realize it from explicit features, a materialized Gram matrix, or
blocked pairwise evaluation. The capability report identifies the realization
and its cost.

Training features are:

```scala
FeatureMatrix[Coordinate, Metadata](
  values: gale.CSR,
  rows: SampleIndex,
  columns: FeatureDictionary[Coordinate],
  metadata: FeatureMetadata[Metadata]
)
```

Query features align to the frozen training dictionary:

```scala
AlignedFeatures[Coordinate](
  shared: gale.CSR,
  novelSquaredNorm: gale.DVec
)
```

`shared` computes train-query inner products. `novelSquaredNorm` retains exact
norm contributions from query-only coordinates. A query-only feature therefore
changes the query diagonal and normalized cross values without creating a
nonexistent training cross term.

Gale owns `DMat`, `DVec`, `CSR`, and `DoubleLinearOperator`. Grakern owns sample
ordering, feature identity, kernel identity, centering and normalization state,
query alignment, provenance, audit events, approximation metadata, and plan
explanations.

Centering is bound to the prepared reference corpus. A centered prepared value
stores the training means and applies those same statistics to query cross
matrices.

## Rejected alternatives

`Kernel[A, Explicit]` is rejected because the available realization is a
planning result, not a mathematical property.

A raw Gale matrix as the entire prepared API is rejected because it cannot
carry ordered sample identity, normalization state, or query alignment.

Dropping unseen query features from the query norm is rejected because it gives
incorrect normalized cross values.

Ravel is not a dependency. Grakern needs matrices and sparse features, which
Gale owns; it does not initially need a general n-dimensional array.

## Consequences

The JVM engine depends on Gale. Core, graph semantics, and standard recipes do
not. A later Alder adapter constructs Alder-owned training and prediction views
instead of declaring Gale to be every library's interchange model.
