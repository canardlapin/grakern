# ADR-004: Numerical domain, normalization, and decomposition weights

Status: Accepted
Date: 2026-07-29

## Context

The mathematical kernel algebra uses real numbers. Runtime computation uses
finite `Double` values, fixed expression trees, and finite reductions.
Pretending that floating-point addition forms the same exact semiring permits
rewrites that change results and makes reproducibility depend on scheduling.

Graph decompositions also need a precise coefficient contract. Occurrence-count
kernels need nonnegative weights; signed linear combinations are useful but are
not finite measures.

## Decision

The public validated values are:

- `FiniteDouble`;
- `NonNegativeFinite`;
- `PositiveFinite`.

They reject `NaN`, positive infinity, and negative infinity. Nonnegative
constructors canonicalize negative zero to positive zero. Checked arithmetic
returns an error when a finite operation overflows.

Friendly APIs may accept ordinary numeric projections. `prepare` validates
their values once while packing and returns `Either[PrepareError, Prepared]`.
Errors identify the sample, graph-local item, projection descriptor, and
offending value.

For a diagonal `d = k(x, x)` and configured tolerance `epsilon`:

- `d < -epsilon` is an invalid negative diagonal;
- `abs(d) <= epsilon` is zero and records an audit event;
- `d > epsilon` is normalized normally.

If either input has zero feature norm, the normalized kernel value is zero.
This includes the zero-vector object's self-kernel. Grakern does not clip
normalized values to `[-1, 1]`; a material Cauchy-Schwarz violation is reported.

Kernel expressions preserve construction order. The compiler does not
distribute products, reassociate sums, or reorder user-visible components.
Parallel execution uses fixed partitions and a deterministic reduction tree.

The v0.1 decomposition contract is:

```scala
trait Decomposition[-A, P]:
  def fold[B](a: A, initial: B)(
    step: (B, P, NonNegativeFinite) => B
  ): Either[DecompositionError, B]
```

`map`, `flatMap`, `filter`, and `reweight` preserve nonnegative weights. A
materialized measure exists only as an independent test implementation.
Kernel convolution is a compiler lift whose preparation propagates any
decomposition error.

The result is fallible because the product of two finite `Double` weights need
not itself be finite. This correction was made when the E5 walking slice
extracted the executable contract; silently saturating or throwing during
`flatMap` would contradict the checked-arithmetic policy above.

## Rejected alternatives

A Cats `CommutativeSemiring[Kernel[A]]` instance is not published in v0.1.
Approximate equality is not transitive and cannot repair machine-level semiring
laws.

Silently replacing invalid values with zero or clipping normalized values is
rejected because those repairs can conceal a broken evaluator and can destroy
positive semidefiniteness.

Signed decomposition weights are deferred to a separate
`FiniteLinearCombination[P]` abstraction. Calling them a measure would make the
v0.1 contract false.

## Consequences

Combinatorial kernels can use exact equality in tests. Numeric primitives use a
simple independent reference evaluator and declared tolerances. Deterministic
mode is reproducible for one engine schema, not a claim of exact real
arithmetic.
