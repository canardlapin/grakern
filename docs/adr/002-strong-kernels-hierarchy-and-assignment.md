# ADR-002: Strong kernels, hierarchies, bags, and optimal assignment

Status: Accepted
Date: 2026-07-29

## Context

An optimal-assignment score is not positive semidefinite for every base kernel.
It is a valid kernel when the nonnegative base kernel satisfies:

```text
k(x, y) >= min(k(x, z), k(z, y))
```

A finite weighted hierarchy certifies this property and provides a fast
histogram-intersection evaluator. It is a computational realization, not the
definition of every strong kernel.

## Decision

`StrongKernel[A]` is a semantic subtype of `Kernel[A]`. It records
nonnegativity and the strong-kernel law. The following operations preserve it:

- pullback with a total projection;
- multiplication by a nonnegative finite constant.

Sums, products, normalization, and induced Gaussian kernels return `Kernel[A]`.

`Kernel.delta[A]` is strong when `A` has lawful equality. It does not require
enumerating every possible `A`.

A validated finite hierarchy contains:

- a finite nonempty node set;
- exactly one root;
- parent links that are members of the node set;
- no cycles and complete reachability from the root;
- finite nonnegative heights that do not decrease toward a leaf;
- an injective association from items to hierarchy leaves.

The root height may be positive. For node `v`, the incremental height is its
height minus its parent's height, or its full height at the root. The induced
base kernel is the height of the least common ancestor.

`FiniteBag[A]` retains duplicate items and integer multiplicities. Unequal bags
are padded semantically with an external object whose feature vector is zero.
The dummy is not required to be a hierarchy leaf. Therefore a positive root
contributes only for real matched items:

```text
height(root) * min(leftBag.size, rightBag.size)
```

`optimalAssignment(base: StrongKernel[A])` always returns a kernel. The planner
uses hierarchy intersection when it has a finite hierarchy realization and a
padded Hungarian oracle otherwise.

For a hierarchy realization, the pairwise value is:

```text
sum(deltaHeight(v) * min(leftCount(v), rightCount(v)))
```

Raw count histograms are not linear feature vectors. Exact features use the
threshold expansion:

```text
phi(v, t)(bag) = sqrt(deltaHeight(v)) when count(v) >= t, otherwise 0
```

The dot product of these features equals histogram intersection.

## Rejected alternatives

`StrongKernel[A] = Hierarchy[A]` is rejected because delta, pullbacks, and
assumed strong evaluators need not carry a globally enumerable hierarchy.

Forcing every root height to zero is rejected because a positive common
baseline among real items is valid.

Exposing raw hierarchy counts as Gale CSR features is rejected because their
dot product does not compute histogram intersection.

## Consequences

The public API prevents an ordinary kernel from entering the lawful
optimal-assignment constructor. Unsafe assumed strong kernels remain possible
but may use the slow oracle. WL uses a synthetic zero-height root as a property
of its recipe, not as a global hierarchy rule.
