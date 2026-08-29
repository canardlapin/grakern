# Final paired WL optimization diagnostic

Date: 2026-08-28  
Courts: `grakern-sparse-final-runtime-diagnostic`, `grakern-cross-serial80-diagnostic`, and
`grakern-all-tier1-serial80-diagnostic`  
Base revision: `0329c43c88a0b71e9aa4456723bb16bac2fa3841` plus the unchanged working-tree implementation  
Boundary: prepared topology, warm mode  
Paired sampling: 10 independent process forks, 30 samples per fork, rotating implementation order

## Result

The final paired cross-kernel receipts passed the independent exact-checksum, boundary,
operation-reset, and host checks. The GraKern source fingerprint was identical before and after the
complete court. The receipts are nevertheless `diagnostic_only`: the implementation was measured
from a dirty working tree, so `source_admitted` is false and these results cannot close the
championship gate.

| Tier-1 row | GraKern median | GraKeL median | GraKern/GraKeL | 99% clustered-bootstrap interval | Diagnostic speed outcome | Peak RSS delta, GraKern / GraKeL |
| --- | ---: | ---: | ---: | ---: | --- | ---: |
| WL subtree, sparse (final runtime) | 35.102 ms | 58.470 ms | 0.600 | [0.575, 0.670] | win | 124.3 / 148.6 MiB |
| WL subtree, novel cross (final runtime) | 49.467 ms | 74.958 ms | 0.660 | [0.623, 0.717] | win | 138.3 / 78.1 MiB |

The labels apply the fixed court policy (`median <= 0.9` and 99% upper bound `<= 0.95` for a win)
as a diagnostic interpretation only. Both final-runtime rows clear the point-estimate and confidence
thresholds. Sparse also uses 24.3 MiB less peak RSS delta than GraKeL. Novel cross has a 60.2 MiB RSS
excess, below the court's 64 MiB absolute materiality threshold, so the simultaneous ratio condition
cannot classify it as a material memory loss.

The final generated Java contract is `-Xmx80m -Xss256k -XX:+UseSerialGC`. A separate
five-fork diagnostic applied that exact contract to all ten Tier-1 Grakern rows. Every receipt was
`diagnostic_only`, semantically admitted, source-stable during execution, and exact; the dense,
optimal-assignment multiplicity, shortest-path, and novel-query workloads all completed without an
out-of-memory failure. That matrix is runtime-safety evidence, not a comparator championship.

## Reproduction

```text
sbt -Dsbt.supershell=false benchmarkWorker/assembly

uv run --with psutil --with jsonschema python benchmarks/court/generate_plan.py \
  --output /tmp/grakern-cross-serial80-plan.json \
  --coverage-mode diagnostic_subset \
  --court-id grakern-cross-serial80-diagnostic \
  --tier1-id t1-wl-subtree-sparse \
  --tier1-id t1-wl-subtree-cross-novel \
  --boundary prepared_topology \
  --mode warm \
  --implementation grakern \
  --implementation grakel

uv run --with psutil --with jsonschema \
  python benchmarks/court/run_performance_court.py run --diagnostic \
  /tmp/grakern-cross-serial80-plan.json \
  /tmp/grakern-cross-serial80-court
```

The sparse and cross receipts are under `/tmp/grakern-sparse-final-runtime-court/receipts` and
`/tmp/grakern-cross-serial80-court/receipts`; the all-row runtime-safety receipts are under
`/tmp/grakern-all-tier1-serial80-court/receipts`. An admitted result still requires a clean immutable
candidate, strict host policy, and the complete Tier-1 championship card across public/prepared and
cold/warm boundaries.
