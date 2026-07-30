# GraKeL compatibility fixtures

These fixtures detect compatibility drift for configurations that Grakern and
GraKeL define in the same way. They do not define Grakern's kernel semantics.
The independent scalar and feature interpreters remain the semantic reference.

The fixture covers normalized and unnormalized WL subtree and WL optimal
assignment on categorical, undirected training and query graphs. It records
the graph encoding, row and column order, split, iteration count,
normalization policy, oracle versions, matrices, and comparison tolerances.

GraKeL parity is deliberately narrower than Grakern's semantic court. In
particular, it does not decide PSD validity, exact signature identity,
query-only feature norms, or hierarchy ancestor semantics. Those contracts are
tested against Grakern's independent reference implementations.

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
