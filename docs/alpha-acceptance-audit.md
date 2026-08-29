# Alpha acceptance audit

Date: 2026-08-28  
Planned milestone: `0.1.0-M1`  
Verdict: **not accepted; external evidence and dependency closure remain blocked**

This audit records the implemented alpha surface and the evidence available in the current working
tree. It is not a release receipt. Local tests, source-tree examples, and diagnostic performance
receipts cannot substitute for a clean immutable candidate, hosted CI, published artifacts, or a
clean external consumer.

## Module ownership and publication boundary

| Module | Responsibility | Planned alpha publication |
| --- | --- | --- |
| `grakern-core` | Kernel algebra, graph identity, plans, metadata, and public failures | JVM, Scala.js, Scala Native |
| `grakern-graph4s` | Graph4s graph-sample adaptation and directed/labelled incidence semantics | JVM, Scala.js, Scala Native |
| `grakern-standard` | Exact WL, shortest-path, assignment, reification, and portable feature emission | JVM, Scala.js, Scala Native |
| `grakern-laws` | Independent references, generators, laws, and reusable test support | JVM, Scala.js, Scala Native |
| `grakern-engine-jvm` | Packed corpora, exact feature matrices, planning, Gram/cross execution, and prepared centering | JVM only |
| `grakern-architecture-probes` | Dependency, identity, and release-receipt checks | Not published |
| `grakern-examples-jvm` | Executable first-user workflows | Not published |
| `grakern-benchmark-worker` | Correctness-locked performance and allocation workers | Not published |

The intended Maven coordinates and platform suffixes are enumerated in the
[release manifest](release-manifest.md). Graph4s owns graph representation and adaptation; Gale
owns numerical decomposition capabilities where a plan explicitly requires them. Grakern does not
silently replace either dependency with a same-shaped fallback.

## Local verification receipt

The following command completed on the current implementation with formatting, compilation, and
tests green across the declared platforms:

```text
sbt -Dsbt.supershell=false scalafmtAll scalafmtCheckAll compileAll testAll
```

| Suite | Passing tests per platform |
| --- | ---: |
| Core | 44 JVM, 44 Scala.js, 44 Scala Native |
| Graph4s adapter | 11 JVM, 11 Scala.js, 11 Scala Native |
| Standard kernels | 48 JVM, 48 Scala.js, 48 Scala Native |
| Laws | 119 JVM, 119 Scala.js, 119 Scala Native |
| JVM engine | 151 |
| Architecture probes | 11 |
| Executable examples | 2 |
| Benchmark worker | 16 |

The law suite includes independent one-round WL histograms, directed arc-reversal fixtures where
1-WL is expected to distinguish the graphs, multiplicity and self-loop regressions, exact reify
round trips, and portable-emitter parity. JVM engine tests cover sparse and dense feature views,
query/batch/recompile agreement, query-only/full-plan bit equality, centering against a fixed
training reference, planner selection, and mutation resistance.

This is focused local evidence from an uncommitted tree. It must be rerun on the exact integrated
candidate and in the clean-clone workflow before acceptance.

## Reader and API evidence

The [getting-started guide](guide/getting-started.md) provides a runnable first kernel; the
[WL prepared workflow](guide/wl-prepared-workflow.md) covers training, query-only compilation,
cross evaluation, centering, provenance, and failure modes. The
[shortest-path workflow](guide/shortest-path-workflow.md) and
[identity/planning guide](guide/identity-plans-and-failures.md) make graph identity, requested
views, and unsupported capabilities explicit. `grakern-examples-jvm` compiles the same public
surface exercised by the guides.

The alpha surface intentionally excludes the E8 approximation, graphlet, random-walk, tiled-output,
GPU, Alder/I/O/effect, and expanded directed-attributed work, as well as the E9 commute-time family.
Those catalogue promises remain separate Motes and must not appear in a `0.1.0-M1` release claim.

## Performance evidence

The query-overlay allocation ratchet prepares 5,000 training graphs and 15,000 exact feature
columns, then executes 10,000 single-query calls. The current diagnostic receipt measures 22,609.9
thread-allocated bytes per query and a 636,408-byte worst allocation-counter interval, below the
committed 100,000-byte mean and 1,000,000-byte peak ceilings. Exact output is consumed by checksum.

The final paired prepared/warm diagnostics classify both previously unresolved rows as wins. Sparse
measures 35.102 ms against GraKeL's 58.470 ms, ratio 0.600 with a 99% clustered-bootstrap interval
of 0.575 to 0.670, while using less peak RSS delta. Novel query measures 49.467 ms against 74.958
ms, ratio 0.660 with an interval of 0.623 to 0.717. Its RSS excess is 60.2 MiB, below the court's
64 MiB absolute materiality threshold. A separate diagnostic completed all ten Grakern Tier-1 rows
exactly under the generated `-Xmx80m -Xss256k -XX:+UseSerialGC` runtime contract.

These receipts are `diagnostic_only` because the implementation tree is dirty. The complete 72-job
public/prepared, cold/warm, cross-library championship has not run on a clean immutable candidate and
quiescent host. Therefore the performance gate remains open.

## Dependency and release evidence

[`release/0.1.0-M1.properties`](../release/0.1.0-M1.properties) records exact Graph4s and Gale
source revisions, and `ReleaseReceiptSuite` fails when those pins drift from `build.sbt`. The release
preflight currently reports three blockers:

1. `release.readiness=dependency-blocked`;
2. `graph4s.resolution=source-pin`;
3. `gale.resolution=source-pin`.

Exact source pins make the development build reproducible, but they are not an artifact-only Maven
closure. No tag, publication, workspace-catalog promotion, or downstream migration is authorized by
this audit.

## Stop conditions

Alpha acceptance remains blocked until all of the following are true on the same immutable
candidate:

1. the complete performance championship admits every Tier-1 row and the performance gate closes;
2. the continuous ratchet is based on those admitted receipts;
3. Graph4s and Gale resolve as immutable artifacts and generated POMs contain no source, filesystem,
   or snapshot dependency;
4. clean-clone hosted CI passes the full JVM/Scala.js/Scala Native gate;
5. published artifacts resolve from a clean external consumer; and
6. the final audit is regenerated from that exact candidate without expanding the alpha catalogue.
