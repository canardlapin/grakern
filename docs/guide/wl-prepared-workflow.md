# Prepare and query a WL kernel

The ordinary WL workflow builds graph samples, defines one shared refinement, composes a kernel,
and prepares it against an ordered training corpus. Preparation produces several views of one
exact feature realization; it does not change the kernel's denotation.

## Build graph samples without merging data

`GraphSample` adapts a finite graph4s graph to Grakern's `LabelledNeighbourhood` protocol. Vertex
IDs identify vertices inside a sample; `Atom` and `Bond` values supply the data consumed by kernel
projections.

The executable example uses `GraphSample.fromEdgeList`:

```scala
GraphSample.fromEdgeList(
  SampleKey("carbonyl"),
  Vector(0, 1),
  Vector(GraphSampleEdge(0, 1, Bond(2)))
)(vertex => Atom(Vector("C", "O")(vertex)))
```

This constructor returns a typed error for duplicate vertices, unknown endpoints, self-loops, and
parallel or reversed duplicate edges. graph4s is deliberately a simple undirected adapter. Use
`NeighbourhoodSample.undirected` or `.directed` when loops, parallel relations, or orientation are
part of the input, and `Reify.relationsAsVertices` when relation values should become WL-visible
nodes.

## Share one refinement between WL components

The refinement decides how node and edge values enter WL signatures. The subtree and
optimal-assignment components below share the same refinement value, so the compiler executes
refinement once:

```scala
val kernel =
  (
    refinement.subtree.weighted(subtreeWeight) +
      refinement.optimalAssignment.weighted(assignmentWeight)
  ).normalized
```

Weights use `NonNegativeFinite`. The type rules prevent a negative or non-finite scale from being
smuggled into a lawful kernel. Optimal assignment is backed by the strong WL hierarchy and is
emitted as exact multiplicity-threshold features; it is not an arbitrary-kernel OA shortcut.

## Prepare one set of numerical views

On the JVM, preparation is explicit:

```scala
val prepared =
  WLCompiler
    .plan(training, kernel, WLPlanOptions(corpusContent = Some(corpusFingerprint)))
    .run()
```

On success, the prepared value owns:

| View | Meaning | Identity/order |
|---|---|---|
| `features` | Exact sparse feature matrix | Rows follow the ordered training keys; columns have a frozen dictionary |
| `gram` | Pairwise training kernel matrix | Both axes use the same training order |
| `operator` | Matrix-vector action | Acts on that training order |
| `planReport` | Realization, costs, capabilities, and identity blockers | Describes this preparation |
| `auditEvents` | Explicit numerical boundary events | Empty unless a policy boundary was exercised |

`prepared.kernel(i, j)` reads the same exact row dot product represented by `gram`. The plan report
also records that shared subtree/OA refinement ran once.

## Align queries to the frozen dictionary

Use `cross` for a batch and `query` for one sparse row:

```scala
val batch = prepared.cross(Vector(querySample))
val one = prepared.query(querySample)
```

Training columns remain frozen. A query feature already seen in training is placed in the matching
column; a query-only feature contributes to `novelSquaredNorm`. This retained norm is necessary for
correct normalized kernels even though a novel feature has zero dot product with every training
row. Query compilation uses an immutable overlay and does not mutate training signatures or
fingerprints.

When an application needs query or cross values but not a training Gram matrix or operator, request
that output explicitly:

```scala
val queryPrepared =
  WLCompiler
    .queryPlan(training, kernel, WLPlanOptions(corpusContent = Some(corpusFingerprint)))
    .runForQueries()

val batch = queryPrepared.flatMap(_.cross(Vector(querySample)))
```

`queryPlan` records `WLRequestedOutput.Query` in artifact identity and `runForQueries` constructs
the same exact frozen feature basis and training traces used by the all-views path. It deliberately
does not construct a training Gram matrix, operator, or all-views plan report. Calling
`runForQueries` on an ordinary `WLCompiler.plan` fails with an explicit requested-output mismatch,
so a cache cannot silently confuse the two artifacts. The executable alpha walkthrough runs both
paths and checks that their cross values are bit-identical.

## Center against one reference corpus

Centering is a prepared operation:

```scala
val centered = prepared.centered
val centeredQuery = centered.flatMap(_.cross(Vector(querySample)))
```

The result freezes feature means, kernel row means, and the grand mean from the ordered training
corpus. It rejects empty references and cross matrices with a different reference identity or
shape. It is intentionally not exposed as a context-free `WLKernel`, because changing the
reference corpus changes the centering operation.

Next, read [identity, plans, and failures](identity-plans-and-failures.md) before caching a result.
