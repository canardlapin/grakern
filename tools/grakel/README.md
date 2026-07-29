# GraKeL compatibility fixtures

These fixtures detect compatibility drift for configurations that Grakern and
GraKeL define in the same way. They do not define Grakern's kernel semantics.
The independent scalar and feature interpreters remain the semantic reference.

The first fixture covers unnormalized WL subtree and WL optimal assignment on
two categorical, undirected graphs. It records the graph encoding, sample
order, iteration count, normalization policy, oracle versions, matrices, and
comparison tolerances.

Verify the committed fixture with the pinned environment:

```bash
uv run \
  --python 3.11 \
  --with grakel==0.1.10 \
  --with numpy==1.26.4 \
  python tools/grakel/generate_fixtures.py --check
```

Use `--write` only when deliberately accepting reviewed oracle drift. Ordinary
Scala tests read the committed JSON resource and do not require Python,
NumPy, or GraKeL.
