# Grakern

Grakern is a graph-kernel calculus for Scala, not a catalogue of unrelated
estimators. Its current walking slice proves that one exact Weisfeiler–Lehman
refinement can feed both subtree and lawful optimal-assignment kernels, then
compile to sparse Gale features, a Gram matrix, an operator, and query-aligned
cross values without repeating refinement work.

The semantic core is:

```text
Kernel + Decomposition + Refinement + Hierarchy + Compiler
```

## Current executable slice

- lawful `Kernel`, `StrongKernel`, hierarchy, and padded optimal assignment;
- a topology-neutral `LabelledNeighbourhood` protocol in core (ADR-009):
  vertices, total node values, and labelled incidences with orientation
  relative to the vertex asked; `NeighbourhoodSample` builds validated
  undirected, directed, and multi-edge inputs, and `GraphSample` is the
  graph4s adapter (vertex identity separate from node values);
- orientation-aware WL refinement: reversing a directed edge changes the
  kernel, undirected inputs are bit-identical to the pre-protocol engine on
  every committed parity fixture;
- exact WL signatures with collision resolution, parent links, immutable query
  overlays, subtree features, and threshold-expanded OA features;
- normalized direct-sum WL preparation with Gale CSR, Gram, operator, cross,
  novel-query norm retention, provenance, and plan reports;
- decomposition-backed vertex and edge histograms with exact sparse features
  and graph-scoped witnesses;
- deterministic parallel Gram tiles whose scheduling does not change a
  floating-point result bit;
- independent scalar/Hungarian differential tests plus pinned GraKeL and
  GraphKernels.jl compatibility fixtures.

## Example

The portable program is ordinary immutable Scala:

```scala
val refinement =
  WLRefinement.create(
    rounds = 2,
    nodeKey = Projection.of[Atom, String](_.element),
    edgeKey = Projection.of[Bond, Int](_.order),
    codecs = WLCodecs[String, Int](None, None)
  ).toOption.get

val kernel =
  (
    refinement.subtree.weighted(subtreeWeight) +
      refinement.optimalAssignment.weighted(assignmentWeight)
  ).normalized
```

On the JVM engine:

```scala
val prepared = WLCompiler.plan(trainingSamples, kernel).run().toOption.get
val trainK = prepared.gram
val testK = prepared.cross(querySamples)
val phi = prepared.features
val operator = prepared.operator
println(prepared.planReport.explain)
```

`architecture-probes/WalkingSliceExamples.scala` compiles the portable example
against the production API. The engine suites compile and execute the
preparation example.

## Modules and platform boundary

| Module | Role | Platforms |
|---|---|---|
| `grakern-core` | kernel claims, decomposition, hierarchy, identity, the labelled-neighbourhood protocol | JVM, Scala.js, Native |
| `grakern-graph4s` | graph4s adapter of the protocol (undirected simple graphs, total fields) | JVM, Scala.js, Native |
| `grakern-standard` | WL and histogram recipes over the protocol (no graph4s dependency) | JVM, Scala.js, Native |
| `grakern-laws` | portable semantic law support | JVM, Scala.js, Native |
| `grakern-engine-jvm` | packed execution, Gale features/matrices/operators | JVM |

Gale owns numerical containers. Grakern owns feature and sample identity,
alignment, provenance, numerical audits, and plan explanation. Ravel is not a
dependency.

The six accepted decisions live in [`docs/adr`](docs/adr). A compact execution
walkthrough is in
[`docs/architecture.md`](docs/architecture.md).

## Verification

```bash
sbt -batch scalafmtCheckAll compileAll testAll
```

Ordinary tests read committed external-oracle fixtures and require neither
Python nor Julia. Their pinned regeneration instructions live under
`tools/grakel` and `tools/graphkernels-jl`.

## Scope

The JVM compiler remains deliberately closed. Histogram query alignment,
shortest path, commute-time kernels, centering, approximation, and a public
lowering SPI are later milestones. External parity is compatibility evidence,
never the semantic definition of a Grakern kernel.
