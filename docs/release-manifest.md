# Grakern 0.1.0-M1 release manifest

`0.1.0-M1` is the planned first immutable Grakern milestone. It is currently
**dependency-blocked** and has not been tagged or published. This manifest describes the intended
artifact boundary; it is not a publication receipt.

The current [alpha acceptance audit](alpha-acceptance-audit.md) brings together the local API,
law, documentation, performance, and dependency evidence and records the remaining stop conditions.

## Admitted artifacts

The intended organization is `io.github.canardlapin`. Every listed artifact is provisional until
the milestone is published and resolved by a clean external consumer.

| Module | Maven artifact variants | Supported platform |
| --- | --- | --- |
| `grakern-core` | `_3`, `_sjs1_3`, `_native0.5_3` | JVM, Scala.js, Scala Native |
| `grakern-graph4s` | `_3`, `_sjs1_3`, `_native0.5_3` | JVM, Scala.js, Scala Native |
| `grakern-standard` | `_3`, `_sjs1_3`, `_native0.5_3` | JVM, Scala.js, Scala Native |
| `grakern-laws` | `_3`, `_sjs1_3`, `_native0.5_3` | JVM, Scala.js, Scala Native |
| `grakern-engine-jvm` | `_3` | JVM |

`grakern-architecture-probes`, `grakern-examples-jvm`, and `grakern-benchmark-worker` are excluded
and publish-skipped.

## Exact source dependency receipt

The canonical machine-readable receipt is
[`release/0.1.0-M1.properties`](../release/0.1.0-M1.properties). It records the exact Graph4s and
Gale revisions used by a clean source build. `ReleaseReceiptSuite` checks those revisions against
`build.sbt`, so a pin change cannot silently leave the receipt stale.

These full-SHA source pins make development builds reproducible, but they do not constitute a
publishable dependency closure. A Storymodel4s source consumer records the exact Grakern revision;
the transitive Graph4s and Gale revisions are then obtained from this receipt for that candidate.

## Blocking release gates

Before the readiness field may change to `candidate-ready` and a tag may be created:

1. Graph4s and Gale must be available at exact immutable artifact versions.
2. Grakern must replace both production `ProjectRef` dependencies with those artifacts.
3. Generated POMs for every admitted module must contain the complete artifact-only closure and
   no snapshot, Git-source, or filesystem dependency.
4. The full clean-clone gate must pass on one clean, pushed candidate SHA.
5. The artifacts must be rehearsed, published under explicit owner authority, and resolved from a
   clean external consumer before the workspace publication state is promoted.

The tag workflow fails closed while the receipt says `dependency-blocked` or names a `source-pin`.
Local tests, `publishLocal`, and the diagnostic performance court do not clear these gates.
