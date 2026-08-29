# Incremental WL query-overlay scaling receipt

Date: 2026-08-28  
Base checkout: `0329c43c88a0b71e9aa4456723bb16bac2fa3841` plus the uncommitted query-overlay implementation  
Host: Apple arm64, 14 reported processors, macOS/Darwin 23.3.0  
Runtime: OpenJDK 25.0.1, sbt 1.11.7, Scala 3.7.4

## Boundary

`QueryOverlayBench` prepares 5,000 undirected path graphs with 20 vertices each, one WL round,
and 15,000 exact feature columns. It then warms 250 single-query calls and times 10,000 further
20-vertex queries through `WLCompiledKernel.query`. Each query has three shared feature coordinates
and one nonzero training cross value. Preparation and the one-time training postings index are
outside the timed/allocation boundary. A checksum consumes every sparse result.

Reproduce from the repository root:

```text
sbt -Dsbt.supershell=false 'benchmarkWorker/scalafmtCheckAll' \
  'benchmarkWorker/runMain grakern.benchmark.QueryOverlayBench'
```

## Final receipt

| Measure | Result |
| --- | ---: |
| Prepared corpus | 5,000 graphs |
| Timed queries | 10,000 |
| Prepared feature columns | 15,000 |
| Preparation | 407,250,375 ns |
| Timed query loop | 490,006,083 ns |
| Mean time/query | 49,000.6 ns |
| Thread allocation | 226,099,048 bytes |
| Mean allocation/query | 22,609.9 bytes |
| Maximum observed single-query allocation delta | 636,408 bytes |
| Heap-pool peak minus start-of-loop occupancy | 226,492,416 bytes |
| Checksum | 14,560,000 |

The maximum single-query delta is a worst observed allocation-counter interval and includes JVM
activity on the executing thread; the mean is the stable sizing measure. The heap-pool peak is a
whole-process occupancy high-water mark, not retained memory attributable to one query.

## Defect found by the scaling gate

The first run took 1,686,302,000 ns and allocated 18,456,655,152 thread bytes: 168,630 ns and
1,845,666 bytes per query. The row helper was eagerly allocating one empty postings buffer per
feature column even when reading a single query row. Making postings lazy and training-only,
right-sizing query counters, and using a sparse small-postings accumulator reduced the final
measured mean allocation by 81.6x and mean time by 3.4x under the same 5,000/10,000 workload.

The public sparse-row path now constructs storage from CSR row entries only. A regression test uses
a one-row, one-million-column CSR with one stored entry and observes a one-entry `SparseFeatureRow`;
there is no length-F row or postings allocation. Query-aligned rows also use a row-only CSR view,
so reading one result cannot accidentally construct the training-oriented column-postings index.
Round histograms size their scratch to query vertices rather than the training dictionary. Cross
evaluation walks only the query's sorted shared coordinates and the corresponding immutable
training postings; for at most 64 posting contributions it uses a small sparse accumulator, then
falls back to a row-sized dense accumulator for larger postings sets.

Those changes reduced the final allocation receipt from 77,777.0 to 22,609.9 bytes per query. The
single timed loop is an allocation ratchet, not a latency comparison: its 49,000.6 ns/query point
estimate was collected on a non-quiescent development host and is not admitted court evidence.

## Correctness and state evidence

- Random directed-graph properties compare single-query sparse cross values with both the batch API
  and a full source-plus-query recompilation to `1e-12`.
- The single-query and batch APIs agree bit-for-bit where they share the prepared representation.
- 1,000 repeated queries preserve the training coordinate vector, semantic fingerprint, Gram entry,
  and sparse training row.
- Query-only features remain absent from the training dictionary and contribute only to
  `novelSquaredNorm`.

This is a local working-tree receipt. It is not hosted-CI or released-artifact evidence.
