# Sparse WL preparation optimization receipt

## Status

This is local diagnostic evidence, not an admitted championship result.

- Court: `grakern-sparse-primitive-interner-xmx160`
- Base revision: `0329c43c88a0b71e9aa4456723bb16bac2fa3841` plus the uncommitted implementation
- Graph4s revision: `ea5d2d762f85f5a0f97ee188deb5fac0ef2bcbaf`
- Gale revision: `d55fe2f97196a76ab7879e1a12f1e92403aeba06`
- Host: `BUC-GW02.local`, Apple arm64, 36 GiB RAM, macOS 14.3
- Host warning: non-court CPU reached 100%; diagnostic host thresholds were deliberately permissive
- Coverage: sparse categorical WL subtree, five rounds, prepared-topology boundary, warm mode
- Sampling: 10 fresh process forks per implementation, 10 warmups and 30 samples per fork
- Semantics: all forks reproduced exact digest `7bb6dc5fa5e072c8f05af1688f71b1e9e940c1677d2d1905b9d66a0ea85a9813`
- Plan SHA-256: `8c745aca66d54dda5117a235be2c883e9eeb45d8ee96a9f80cf8e3133c8c4cc9`

The raw evidence remains under
`/tmp/grakern-sparse-primitive-interner-xmx160-court` on the measurement host.
The Grakern and GraKeL receipt SHA-256 values are respectively
`c755ac07323a9f4fe331ae0e5e670f3f96d538c8ead1f50f463598979ff492db` and
`ac55aaa192972cac9878fcb8a51654a610c82e4b7aa1451b098a90fc75b267e4`.

## Result

| Measure | Grakern | GraKeL | Diagnostic decision |
| --- | ---: | ---: | --- |
| Complete-operation median | 69.742 ms | 80.510 ms | Grakern 13.4% faster |
| Peak RSS delta | 202.719 MB | 151.732 MB | not material |
| Peak RSS | 332.399 MB | 278.807 MB | recorded |
| Median allocation/operation | 72.615 MB | unavailable | recorded |
| Exact output digest | `7bb6…9813` | `7bb6…9813` | pass |

The frozen court calls a memory difference material only when both the ratio exceeds 1.1 and the
absolute difference exceeds 67,108,864 bytes. The RSS-delta difference here is 50,986,976 bytes,
so the row has no material memory loss even though Grakern uses more memory. The checkout and host
are inadmissible, so this table is a before-release diagnostic rather than a victory claim.

## Changes under measurement

- The standard module emits one durable dictionary plus canonical CSR-like rows. Engine
  preparation consumes those rows directly instead of re-expanding coordinate-bearing entries.
- WL hashing uses primitive loops rather than a boxing `foldLeft`.
- Preparation-local interning uses primitive open-addressed hash chains and freezes once into a
  compact CSR-like immutable index. Deliberate collision tests still resolve exact signatures.
- Graph incidences and local vertex maps are cached or built once instead of reconstructed on each
  refinement pass.
- Feature-posting Gram evaluation uses primitive count, offset, row, and value arrays.
- The measured JVM contract is `-Xmx160m -XX:+UseParallelGC -XX:ParallelGCThreads=1`.

Against the 2026-07-30 diagnostic baseline for the same prepared sparse row, Grakern fell from
2,556.778 ms to 69.742 ms. Peak RSS delta fell from 1,150.7 MiB to approximately 193.3 MiB. These
are cross-checkout diagnostic comparisons; the exact same-session GraKeL comparison above is the
decision evidence for the current implementation.

## Reproduction

Build the worker, generate the two-implementation diagnostic plan, and run it outside the source
checkout:

```text
sbt benchmarkWorker/assembly

uv run --with jsonschema python benchmarks/court/generate_plan.py \
  --coverage-mode diagnostic_subset \
  --court-id grakern-sparse-primitive-interner-xmx160 \
  --tier1-id t1-wl-subtree-sparse \
  --boundary prepared_topology \
  --mode warm \
  --implementation grakern \
  --implementation grakel \
  --output /tmp/grakern-sparse-primitive-interner-xmx160-plan.json

uv run --with psutil --with jsonschema \
  python benchmarks/court/run_performance_court.py run --diagnostic \
  /tmp/grakern-sparse-primitive-interner-xmx160-plan.json \
  /tmp/grakern-sparse-primitive-interner-xmx160-court
```

An admitted result still requires an immutable clean source snapshot, the strict quiescent-host
policy, and the complete Tier-1 championship card.
