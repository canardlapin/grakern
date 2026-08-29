package grakern.examples

import cats.kernel.Hash
import grakern.core.*
import grakern.engine.*
import grakern.graph4s.{GraphSample, GraphSampleEdge}
import grakern.standard.shortestpath.UnweightedShortestPaths
import grakern.standard.wl.*

object AlphaWalkthrough:
  final case class Atom(element: String)
  final case class Bond(order: Int)

  final case class WLResult(
      trainingSimilarity: Double,
      querySimilarity: Double,
      queryOnlySimilarity: Double,
      queryNovelSquaredNorm: Double,
      centeredQuerySimilarity: Double,
      featureCount: Int,
      semanticIdentityDurable: Boolean,
      artifactIdentityDurable: Boolean,
      planHeading: String
  )

  final case class ShortestPathResult(
      trainingSimilarity: Double,
      featureCount: Int,
      provenanceEntries: Int,
      sharedContributions: Int,
      planHeading: String
  )

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private def stableId(value: String): StableId =
    StableId.from(value).fold(error => throw new IllegalArgumentException(error.toString), identity)

  private val version = StableVersion.initial

  private def graph(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int, Int)]
  ): Either[String, GraphSample[Int, Atom, Bond]] =
    GraphSample
      .fromEdgeList(
        SampleKey(key),
        labels.indices,
        edges.map { case (left, right, order) =>
          GraphSampleEdge(left, right, Bond(order))
        }
      )(vertex => Atom(labels(vertex)))
      .left
      .map(_.toString)

  def trainingSamples: Either[String, Vector[GraphSample[Int, Atom, Bond]]] =
    for
      pair <- graph("carbonyl", Vector("C", "O"), Vector((0, 1, 2)))
      chain <- graph(
        "ether",
        Vector("C", "O", "C"),
        Vector((0, 1, 1), (1, 2, 1))
      )
    yield Vector(pair, chain)

  def querySample: Either[String, GraphSample[Int, Atom, Bond]] =
    graph(
      "amine-query",
      Vector("C", "N"),
      Vector((0, 1, 1))
    )

  private def refinement: Either[String, WLRefinement[Atom, Bond, String, Int]] =
    WLRefinement
      .create(
        rounds = 2,
        nodeKey = Projection.named[Atom, String](
          stableId("examples.atom-element"),
          version
        )(_.element),
        edgeKey = Projection.named[Bond, Int](
          stableId("examples.bond-order"),
          version
        )(_.order),
        codecs = WLCodecs(
          Some(
            StableCodec.named[String](
              stableId("examples.atom-element-codec"),
              version
            )(identity)
          ),
          Some(
            StableCodec.named[Int](
              stableId("examples.bond-order-codec"),
              version
            )(_.toString)
          )
        )
      )
      .left
      .map(_.toString)

  def wl: Either[String, WLResult] =
    for
      training <- trainingSamples
      query <- querySample
      shared <- refinement
      corpus <- CorpusContentFingerprint
        .from("examples.alpha-training.v1")
        .left
        .map(_.toString)
      subtreeWeight <- NonNegativeFinite.from(0.75).left.map(_.toString)
      assignmentWeight <- NonNegativeFinite.from(0.25).left.map(_.toString)
      kernel =
        (
          shared.subtree.weighted(subtreeWeight) +
            shared.optimalAssignment.weighted(assignmentWeight)
        ).normalized
      prepared <- WLCompiler
        .plan(
          training,
          kernel,
          WLPlanOptions(corpusContent = Some(corpus))
        )
        .run()
        .left
        .map(_.toString)
      aligned <- prepared.cross(Vector(query)).left.map(_.toString)
      queryPrepared <- WLCompiler
        .queryPlan(
          training,
          kernel,
          WLPlanOptions(corpusContent = Some(corpus))
        )
        .runForQueries()
        .left
        .map(_.toString)
      queryOnly <- queryPrepared.cross(Vector(query)).left.map(_.toString)
      centered <- prepared.centered.left.map(_.toString)
      centeredQuery <- centered.cross(Vector(query)).left.map(_.toString)
    yield WLResult(
      trainingSimilarity = prepared.kernel(0, 1).get,
      querySimilarity = aligned.cross.values(0, 0),
      queryOnlySimilarity = queryOnly.cross.values(0, 0),
      queryNovelSquaredNorm = aligned.features.novelSquaredNorm(0),
      centeredQuerySimilarity = centeredQuery.cross.values(0, 0),
      featureCount = prepared.features.columns.size,
      semanticIdentityDurable = prepared.semanticFingerprint match
        case Fingerprintability.Durable(_)   => true
        case Fingerprintability.Ephemeral(_) => false,
      artifactIdentityDurable = prepared.artifactFingerprint match
        case ArtifactFingerprintability.Durable(_)   => true
        case ArtifactFingerprintability.Ephemeral(_) => false,
      planHeading = prepared.planReport.explain.linesIterator.next()
    )

  def shortestPath: Either[String, ShortestPathResult] =
    for
      training <- trainingSamples
      recipe = UnweightedShortestPaths.parts[Atom, Bond, String](
        Projection.named[Atom, String](
          stableId("examples.shortest-path.atom-element"),
          version
        )(_.element)
      )
      prepared <- ShortestPathPlanner.categorical(training, recipe).run().left.map(_.toString)
      features <- prepared.features.left.map(_.toString)
      provenance <- prepared.provenance.left.map(_.toString)
      contributions <- prepared
        .explain(0, 1)
        .left
        .map(_.toString)
        .map(_.getOrElse(Vector.empty))
    yield ShortestPathResult(
      trainingSimilarity = prepared.kernel(0, 1).get,
      featureCount = features.columns.size,
      provenanceEntries = provenance.size,
      sharedContributions = contributions.size,
      planHeading = prepared.planReport.explain.linesIterator.next()
    )

object AlphaExampleMain:
  def main(_args: Array[String]): Unit =
    val result =
      for
        wl <- AlphaWalkthrough.wl
        shortestPath <- AlphaWalkthrough.shortestPath
      yield wl -> shortestPath
    result.fold(
      error => throw new IllegalStateException(error),
      { case (wl, shortestPath) =>
        println(
          s"WL train=${wl.trainingSimilarity}, query=${wl.querySimilarity}, novelNorm=${wl.queryNovelSquaredNorm}, features=${wl.featureCount}"
        )
        println(
          s"Shortest path train=${shortestPath.trainingSimilarity}, features=${shortestPath.featureCount}, provenance=${shortestPath.provenanceEntries}"
        )
      }
    )
