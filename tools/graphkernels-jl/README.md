# GraphKernels.jl compatibility fixtures

This court is independent compatibility evidence, not Grakern's semantic
oracle. It pins GraphKernels.jl revision
`f04797de0b86a39172b6adbcfcee35ec4b39f91b` (package version 0.2.0).

The source audit found that the pinned WL refinement reads the current
vertex's previous value for every neighbor, rather than each neighbor's
previous value. Broad WL parity would therefore certify different semantics.
The committed fixture is intentionally limited to uniform-labeled regular
graphs, where that implementation and conventional exact WL refinement agree.
It uses a small exact vertex-histogram base kernel implemented through
GraphKernels.jl's documented preprocessing interface.

Instantiate and verify the pinned environment:

```bash
JULIA_DEPOT_PATH=/tmp/grakern-graphkernels-julia \
  julia --project=tools/graphkernels-jl \
  -e 'using Pkg; Pkg.instantiate()'

JULIA_DEPOT_PATH=/tmp/grakern-graphkernels-julia \
  julia --project=tools/graphkernels-jl \
  tools/graphkernels-jl/generate_fixtures.jl --check
```

Use `--write` only after reviewing intentional oracle drift. Ordinary Scala
tests consume the fixture without requiring Julia. Shortest-path parity is
deferred until Grakern's E6 path semantics are frozen.
