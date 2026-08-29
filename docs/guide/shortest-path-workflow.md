# Use shortest-path features and provenance

Categorical shortest-path kernels compare reachable unordered vertex pairs by endpoint labels and
positive hop distance. The semantic recipe uses an independent Floyd-Warshall reference; the JVM
planner prepares each graph with unweighted BFS and can lower the same denotation to exact sparse
features, direct pairwise evaluation, or both for validation.

Create the recipe from a node projection and run the default exact-feature plan:

```scala
val recipe =
  UnweightedShortestPaths.parts[Atom, Bond, String](
    Projection.named(atomProjectionId, StableVersion.initial)(_.element)
  )

val prepared = ShortestPathPlanner.categorical(training, recipe).run()
```

The default prepared value exposes `features`, `gram`, `operator`, `provenance`, `explain`, and a
`planReport`. Provenance identifies the graph-local endpoint pair behind each emitted feature;
`explain(left, right)` reports the shared feature contributions to one Gram cell.

Lowering controls capabilities, not denotation:

| Lowering | Exact features | Provenance | Gram/operator | Purpose |
|---|---:|---:|---:|---|
| `ExactFeatures` | yes | yes | yes | Ordinary inspectable workflow |
| `Pairwise` | no | no | yes | Direct exact evaluation without a feature matrix |
| `ValidateBoth` | yes | yes | yes | Execute both routes and reject a difference beyond tolerance |

Requests for features or provenance under `Pairwise` return
`ShortestPathFeatureUnavailable.PairwiseLoweringSelected` or
`ShortestPathProvenanceUnavailable.PairwiseLoweringSelected`. The report repeats the selected
realization and its capability flags, so downstream code does not have to infer them from class
names.

The current categorical recipe traverses every incidence in both directions. It therefore has
undirected shortest-path semantics even when the input protocol carries orientations. Directed
shortest paths are not part of the alpha recipe.

The complete compiled workflow is `AlphaWalkthrough.shortestPath` in
[`AlphaWalkthrough.scala`](../../examples-jvm/src/main/scala/grakern/examples/AlphaWalkthrough.scala).
Next, read [identity, plans, and failures](identity-plans-and-failures.md) to decide whether a
prepared result is safe to persist or compare.
