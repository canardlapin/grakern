# Tier-1 warm diagnostic v3

## Status

This is diagnostic evidence, not a championship result.

- Court: `grakern-tier1-warm-diagnostic-v3`
- Grakern revision: `2289342cb25c3f295b732b5d70d9076e0ee0c3ff`
- Graph4s revision: `ea5d2d762f85f5a0f97ee188deb5fac0ef2bcbaf`
- Gale revision: `d55fe2f97196a76ab7879e1a12f1e92403aeba06`
- Host: `BUC-GW02.local`, Apple arm64, 36 GiB RAM, AC power
- Coverage: 10 Tier-1 rows, public and prepared boundaries, warm mode
- Completion: 48 of 48 implementation jobs
- Semantics: every worker reproduced its independent expected output
- Plan SHA-256: `16900a878f063eadfc9d309988e7e4531849d7a15f84204fc4d0ac91bfa3c4b5`
- Court-state SHA-256: `1a13fb1847335331389c838afc6672853dc68a748deba65e6cb17f1854583f4c`
- Receipt-set digest: `79b793ae7dfaf99a47014a701e68a59dd4a67d913920554fec83227294a3af1d`

The raw evidence is under
`/tmp/grakern-tier1-warm-diagnostic-v3` on the measurement host. Each receipt
is `diagnostic_only`. The outcomes below apply the frozen court policy to
copies of the exact receipts for diagnosis; they are not written back as
admitted classifications.

Standard jobs used 10 fresh process forks, 10 warmups and 30 samples per fork.
Only predeclared operations above two seconds used the reduced 5-fork,
5-sample policy. Every warm implementation accumulated at least 30 seconds of
timed work. Fork-local untimed calibration durations and batch counts are
retained in the receipts.

## Complete league table

Times are complete-operation medians in milliseconds. `Public` includes graph
construction; `Prepared` starts from the comparator's native prepared
topology. The ratio and 99% clustered-bootstrap interval compare Grakern with
the fastest external comparator. Memory is Grakern/best-comparator peak RSS
delta in MiB.

| Row | Boundary | Grakern | GraKeL | GraphKernels.jl | Ratio [99% interval] | Speed | Memory MiB | Memory |
| --- | --- | ---: | ---: | ---: | ---: | --- | ---: | --- |
| WL subtree, MUTAG | Public | 125.412 | 20.215 | — | 6.204 [6.077, 6.593] | loss | 543.2 / 65.7 | material loss |
| WL subtree, MUTAG | Prepared | 120.564 | 17.939 | — | 6.721 [6.627, 6.833] | loss | 479.1 / 60.4 | material loss |
| WL subtree, sparse | Public | 2872.106 | 64.555 | — | 44.491 [41.272, 51.712] | loss | 1075.5 / 143.2 | material loss |
| WL subtree, sparse | Prepared | 2556.778 | 59.114 | — | 43.252 [42.748, 45.944] | loss | 1150.7 / 136.0 | material loss |
| WL subtree, dense | Public | 1196.380 | 53.823 | — | 22.228 [21.334, 23.696] | loss | 806.8 / 101.0 | material loss |
| WL subtree, dense | Prepared | 1238.972 | 51.891 | — | 23.876 [22.743, 26.071] | loss | 760.4 / 98.6 | material loss |
| WL subtree, regular | Public | 45.431 | 35.679 | 29.990 | 1.515 [1.484, 1.658] | loss | 1714.3 / 114.7 | material loss |
| WL subtree, regular | Prepared | 35.922 | 30.747 | 16.862 | 2.130 [2.089, 2.193] | loss | 1832.1 / 96.2 | material loss |
| WL subtree, novel cross | Public | 2489.737 | 76.648 | — | 32.483 [32.311, 32.795] | loss | 1218.3 / 90.0 | material loss |
| WL subtree, novel cross | Prepared | 2471.864 | 69.823 | — | 35.402 [35.145, 35.780] | loss | 1028.1 / 77.9 | material loss |
| WL OA, MUTAG | Public | 167.846 | 204.304 | — | 0.822 [0.789, 0.868] | win | 656.1 / 80.7 | material loss |
| WL OA, MUTAG | Prepared | 164.444 | 200.015 | — | 0.822 [0.786, 0.852] | win | 663.5 / 72.7 | material loss |
| WL OA, multiplicity | Public | 941.543 | 1584.937 | — | 0.594 [0.566, 0.637] | win | 1539.4 / 219.1 | material loss |
| WL OA, multiplicity | Prepared | 912.856 | 1563.616 | — | 0.584 [0.565, 0.617] | win | 1370.6 / 203.9 | material loss |
| Shortest path, MUTAG | Public | 21.882 | 91.187 | 344.136 | 0.240 [0.227, 0.257] | win | 1187.6 / 14.1 | material loss |
| Shortest path, MUTAG | Prepared | 18.577 | 88.201 | 332.070 | 0.211 [0.205, 0.215] | win | 655.9 / 9.3 | material loss |
| Shortest path, sparse | Public | 46.358 | 362.768 | 12688.343 | 0.128 [0.125, 0.131] | win | 889.0 / 25.6 | material loss |
| Shortest path, sparse | Prepared | 34.870 | 358.671 | 12414.444 | 0.097 [0.096, 0.099] | win | 823.6 / 16.1 | material loss |
| Shortest path, regular | Public | 52.996 | 352.285 | 5014.281 | 0.150 [0.147, 0.153] | win | 1056.4 / 24.3 | material loss |
| Shortest path, regular | Prepared | 42.984 | 340.280 | 4909.331 | 0.126 [0.125, 0.128] | win | 818.5 / 15.0 | material loss |

Diagnostic scorecard:

- Speed: 10 wins, 10 losses, no ties.
- Peak memory: 20 material losses.
- WL subtree: loss at every row and boundary.
- WL optimal assignment: win at every row and boundary.
- Categorical shortest path: win at every row and boundary.

GraphKernels.jl is an exact comparator for the regular-uniform WL row and the
three shortest-path rows. Its current package does not supply matched Tier-1
semantics for WL OA, the other WL subtree corpora, or WL cross; those cells are
unsupported rather than missing results.

## Bottleneck ownership

### Public graph boundary and Graph4s

Grakern's measured public graph-construction phase is 2.1–13.3 ms. The severe
WL losses remain at the prepared boundary:

- sparse WL: 2556.8 ms versus 59.1 ms;
- dense WL: 1239.0 ms versus 51.9 ms;
- novel cross: 2471.9 ms versus 69.8 ms.

Graph4s construction, identity handling, and packing are therefore not the
primary speed bottleneck in this baseline. We control Graph4s and should
optimize it if later profiles move ownership there, but the present loss is in
Grakern's engine.

### WL subtree

A 39-second JFR recording of prepared sparse WL contained 2,710 execution
samples and 3,700 allocation samples.

- `scala.collection.Iterator.indexWhere`: 71.55% of execution samples.
- `scala.collection.immutable.Vector.from`: 12.66%.
- vector iteration and immutable map lookup: another 7.94%.
- `ExactFeatureRow.Indexed.find`: 1.96%.
- Gale COO sorting: 0.22%.

The hot `indexWhere` path is owned by
`WLSubtreeFeatures.fromTrace` and `buildProvenance`: both repeatedly scan and
rebuild immutable vectors while counting colours or appending witnesses.
Allocation pressure is dominated by boxed doubles, generated
`ExactFeatureRow.Indexed.find` functions, boxed integers, vector iterators,
and immutable provenance growth.

Primary owner: `grakern-engine-jvm` feature extraction and provenance.

### WL optimal assignment

A 35-second JFR recording of prepared multiplicity OA contained 1,994
execution samples, 10,234 allocation samples, and 122 collections.

- `ExactFeatureRow.Indexed.find`: 32.15% of execution samples.
- generic hashing plus `WLColour.hashCode`: 32.19%.
- immutable map lookup and `Option.getOrElse`: 10.33%.
- hierarchy threshold feature generation: 2.36%.
- Gale COO sorting: 0.35%.

Allocation pressure is 45.17% boxed doubles, 14.77% vector iterators, 13.99%
boxed integers, then arrays, `Some`, and immutable map nodes.

Grakern already wins OA speed. The next OA work is allocation and peak-memory
reduction without regressing its threshold-feature semantics.

Primary owner: `grakern-engine-jvm` exact feature indexing, hashing, and boxed
threshold rows.

### Shortest path

A five-second JFR recording of prepared sparse shortest path contained 263
execution samples and 1,593 allocation samples.

- boxed equality: 29.66% of execution samples;
- unweighted distance block/BFS: 16.73%;
- vector building and iteration: 12.54%;
- `collectFirst` and `indexWhere`: 8.74%;
- Gale CSR construction: 0.38%.

Allocation pressure is spread across vector/object arrays, boxed doubles,
endpoint descriptors, witnesses, occurrences, integers, and provenance.

Grakern wins shortest-path speed by 4.2–10.3 times against the fastest
comparator, so BFS/APSP is not a speed blocker. The object-rich witness and
provenance representation remains a memory blocker.

Primary owner: `grakern-engine-jvm` occurrence/provenance materialization and
boxed categorical feature handling.

### Gale and numerical materialization

Gale appears only as COO sorting or CSR construction in the representative
profiles, at 0.22–0.38% of execution samples. No Tier-1 speed loss is presently
owned by Gale. Gram materialization is covered by the measured kernel-and-matrix
phase. The operator view is not exercised by this Tier-1 card, so this baseline
makes no operator-performance claim.

### Allocation and peak memory

Every Grakern row has a material peak-RSS loss. Candidate peak deltas range
from 479 MiB to 1,832 MiB, while the best comparator ranges from 9 MiB to
219 MiB. The JFR profiles consistently identify boxed numerics, immutable
vectors/maps, per-occurrence case classes, closures, and provenance as the
allocation owners.

The receipt's allocation counter covers the complete timed sampling loop and
is not normalized across differing batch counts. It is useful as raw audit
evidence but must not be compared as a per-operation rate until the court
records a normalized allocation metric.

## Optimization order

1. Replace quadratic WL counting and provenance accumulation with
   collision-resolving mutable builders followed by one immutable freeze.
2. Replace closure-heavy `ExactFeatureRow.Indexed.find` and boxed generic
   feature lookup in hot Gram paths.
3. Separate numerical feature payloads from optional witness provenance so
   ordinary Gram/operator preparation does not retain full occurrence graphs.
4. Re-run the exact Tier-1 diagnostic after each change and reject any checksum
   or query-novelty regression.
5. Open Graph4s or Gale optimization only when a fresh profile assigns a
   material share to those owned repositories.

