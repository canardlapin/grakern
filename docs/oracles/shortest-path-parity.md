# Shortest-path compatibility court

Grakern's shortest-path semantic oracle is its independent Floyd-Warshall
interpreter, not another graph-kernel package. External packages are valuable
compatibility and regression witnesses only after their conventions have been
made explicit.

## Canonical Grakern convention

For an undirected graph, Grakern emits each reachable unordered pair of
distinct vertices exactly once. The categorical kernel is the dot product of
the resulting exact `(unordered endpoints, distance)` count vectors.

## GraKeL 0.1.10

The pinned GraKeL fixture uses unit distances, no endpoint labels, and
Floyd-Warshall. GraKeL emits both orders of every reachable pair. On the
uniform-label fixture this doubles every count coordinate, so its raw Gram
matrix is four times Grakern's. Normalization removes that common scale.

The generator executes GraKeL itself and records both raw and normalized
matrices. The Scala court applies the declared factor rather than changing
Grakern's pair identity.

## GraphKernels.jl 0.2.0

The Julia fixture pins source revision
`f04797de0b86a39172b6adbcfcee35ec4b39f91b`. Its implementation also emits
ordered pairs. In addition, it combines the two endpoint-kernel evaluations by
addition:

```text
vertexKernel(u, u') + vertexKernel(v, v')
```

Grakern's lawful soft shortest-path kernel uses the tensor-symmetrized product
of endpoint kernels. The Julia behavior is therefore recorded as an external
compatibility convention, not copied into the canonical calculus.

With a constant-one endpoint kernel, ordered-pair duplication contributes a
factor of four and endpoint addition contributes a factor of two. The raw
GraphKernels.jl Gram is consequently divided by eight in the aligned
categorical fixture.

## Reproducing the fixtures

```bash
uv run \
  --python 3.11 \
  --with grakel==0.1.10 \
  --with numpy==1.26.4 \
  python tools/grakel/generate_fixtures.py --check

JULIA_DEPOT_PATH=/tmp/grakern-graphkernels-julia \
  julia --project=tools/graphkernels-jl \
  tools/graphkernels-jl/generate_fixtures.jl --check
```

Fixture drift is a prompt for source review. It is not permission to redefine
Grakern until the independent denotation, external source behavior, and
translation have all been reconciled.
