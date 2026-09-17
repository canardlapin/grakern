# Grakern

[User guide](docs/guide/index.md) ·
[Executable example](examples-jvm/src/main/scala/grakern/examples/AlphaWalkthrough.scala) ·
[Design decisions](docs/adr) ·
[Changelog](CHANGELOG.md)

Grakern is a Scala library for measuring similarity between labelled graphs. A labelled graph
might be a molecule whose nodes are atoms and edges are bonds, or any network where nodes and
edges carry data. Grakern can produce a pairwise similarity matrix for a collection of graphs and
then score new graphs against the same frozen feature space.

A graph kernel gives a similarity score to a pair of graphs. Grakern currently provides exact
Weisfeiler–Lehman (WL) and shortest-path kernels, together with a JVM engine that prepares sparse
features, training Gram matrices, and aligned values for new graphs.

> **Status:** Grakern is pre-release, source-only software on the `0.1` development line. Its APIs
> may change, and no Maven artifacts have been published. The planned `0.1.0-M1` release remains
> blocked on published Graph4s and Gale dependencies.

## Quick start

Clone the repository, then run the checked example with sbt:

```bash
sbt -batch \
  'examplesJVM/test' \
  'examplesJVM/runMain grakern.examples.AlphaExampleMain'
```

It prints:

```text
WL train=0.3055555555555556, query=0.1666666666666667, novelNorm=0.8333333333333335, features=23
Shortest path train=2.0, features=2, provenance=2
```

The example builds small labelled graphs, prepares a normalized WL kernel, computes training and
query similarities, and inspects the resulting feature and provenance information. Its
[complete source](examples-jvm/src/main/scala/grakern/examples/AlphaWalkthrough.scala) uses only
the public API and is executed by the example test.

Production bytecode targets JDK 17. The repository CI uses Temurin 21; running the Scala Native
tests also requires a native toolchain.

## What Grakern can do

- Build graph samples from graph4s or directly from Grakern's labelled-neighbourhood protocol.
- Compare graphs with exact WL subtree kernels or a WL optimal-assignment kernel, and combine or
  normalize those measures.
- Prepare sparse feature matrices, Gram matrices, and matrix-vector operators on the JVM.
- Score new graphs without rebuilding or altering the training representation, including correct
  normalization when a query contains previously unseen features.
- Build kernels from categorical or soft shortest paths and from node or edge histograms.
- Inspect sample identity, feature metadata, provenance, numerical audit events, and an explanation
  of the execution plan.

The portable graph and kernel APIs run on the JVM, Scala.js, and Scala Native. Matrix preparation
and the Gale-backed numerical views are currently JVM-only.

## Modules

| Module | Use it for | Platforms |
|---|---|---|
| `grakern-core` | Kernel types, graph identity, and the labelled-neighbourhood input protocol | JVM, Scala.js, Native |
| `grakern-graph4s` | Adapting graph4s graphs into Grakern samples | JVM, Scala.js, Native |
| `grakern-standard` | WL, shortest-path, assignment, and histogram kernel recipes | JVM, Scala.js, Native |
| `grakern-engine-jvm` | Preparing features, matrices, operators, queries, centering, and reports | JVM |
| `grakern-laws` | Reusable laws, generators, and reference checks for kernel implementations | JVM, Scala.js, Native |

These are intended artifact boundaries, not currently published coordinates. Grakern owns graph
sample identity, feature alignment, provenance, and plan reporting. Graph4s supplies the graph
representation, and Gale supplies JVM numerical containers and operators. Ravel is not a
dependency.

## Fit and current limits

Grakern is a good fit when you need more than opaque similarity scores: it keeps the kernel recipe,
input order, feature mapping, and execution plan available for inspection. The current
implementation is aimed at exact, in-memory graph kernels rather than a general estimator or model
training framework.

Histogram query alignment, directed shortest-path semantics, commute-time kernels, approximation,
GPU execution, and a public lowering extension API are not part of the current surface. External
GraKeL and GraphKernels.jl fixtures provide compatibility checks; they do not define Grakern's
semantics. See the [alpha acceptance audit](docs/alpha-acceptance-audit.md) and
[release manifest](docs/release-manifest.md) for the evidence and remaining release gates.

## Documentation

- [Run the executable examples](docs/guide/getting-started.md) — prerequisites, commands, and
  expected output.
- [Prepare and query a WL kernel](docs/guide/wl-prepared-workflow.md) — graph samples, shared
  refinement, sparse features, queries, and centering.
- [Use shortest-path kernels](docs/guide/shortest-path-workflow.md) — categorical paths, soft path
  lengths, provenance, and explanations.
- [Interpret identity, plans, and failures](docs/guide/identity-plans-and-failures.md) — durable
  fingerprints, requested outputs, and explicit failure modes.
- [Architecture](docs/architecture.md) and [ADRs](docs/adr) — module ownership and mathematical or
  numerical guarantees.

## Developing

Run the complete supported-platform source gate with:

```bash
sbt -batch scalafmtCheckAll compileAll testAll
```

Ordinary tests use committed external-oracle fixtures and require neither Python nor Julia. Their
regeneration tools and provenance are under [`tools/grakel`](tools/grakel) and
[`tools/graphkernels-jl`](tools/graphkernels-jl).

## License

Grakern is available under the [Apache License 2.0](LICENSE).
