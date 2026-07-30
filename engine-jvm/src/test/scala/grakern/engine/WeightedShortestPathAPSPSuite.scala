package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Edge, Graph, Link}
import munit.FunSuite

final class WeightedShortestPathAPSPSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode

  final private case class Bond(length: Double)

  final private case class GraphSpec(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int, Double)]
  )

  private def sample(
      spec: GraphSpec
  ): GraphSample[Int, String, Bond] =
    val topology =
      Graph
        .of(
          spec.labels.indices,
          spec.edges.map { case (left, right, _) =>
            Link(left, right)
          }
        )
        .toOption
        .get
    val weights =
      spec.edges.map { case (left, right, weight) =>
        endpointKey(left, right) -> weight
      }.toMap
    GraphSample.build(SampleKey(spec.key), topology)(
      nodeValue = spec.labels.apply,
      edgeValue = edge => Bond(weights(endpointKey(edge.first, edge.second)))
    )

  private def endpointKey(
      left: Int,
      right: Int
  ): (Int, Int) =
    math.min(left, right) -> math.max(left, right)

  private def lengths(
      projection: Projection[Bond, Double] = Projection.of(_.length)
  ): WeightedEdgeLengths[Bond] =
    WeightedEdgeLengths(
      ProjectionDescriptor("bond.length"),
      projection
    )

  private def recipe(
      edgeLengths: WeightedEdgeLengths[Bond]
  ): WeightedShortestPathRecipe[String, Bond, String] =
    WeightedShortestPaths.parts(
      Projection.of(identity),
      edgeLengths
    )

  private def referenceParts(
      spec: GraphSpec
  ): Vector[ShortestPathPart[String, PathLength]] =
    val size = spec.labels.size
    val distances =
      Array.fill(size, size)(Double.PositiveInfinity)
    var vertex = 0
    while vertex < size do
      distances(vertex)(vertex) = 0.0
      vertex += 1
    spec.edges.foreach { case (left, right, length) =>
      distances(left)(right) = length
      distances(right)(left) = length
    }
    var intermediate = 0
    while intermediate < size do
      var left = 0
      while left < size do
        var right = 0
        while right < size do
          val candidate =
            distances(left)(intermediate) +
              distances(intermediate)(right)
          if candidate < distances(left)(right) then distances(left)(right) = candidate
          right += 1
        left += 1
      intermediate += 1
    val output =
      Vector.newBuilder[
        ShortestPathPart[String, PathLength]
      ]
    var left = 0
    while left < size do
      var right = left + 1
      while right < size do
        val distance = distances(left)(right)
        if distance.isFinite then
          output += ShortestPathPart(
            UndirectedEndpoints(
              spec.labels(left),
              spec.labels(right)
            ),
            PathLength.from(distance).toOption.get
          )
        right += 1
      left += 1
    output.result()

  private def partCounts(
      parts: Vector[ShortestPathPart[String, PathLength]]
  ): Map[ShortestPathPart[String, PathLength], Int] =
    parts.groupMapReduce(identity)(_ => 1)(_ + _)

  test("Dijkstra agrees with the independent weighted Floyd denotation"):
    val specs =
      Vector(
        GraphSpec(
          "weighted-path",
          Vector("C", "N", "O"),
          Vector(
            (0, 1, 0.5),
            (1, 2, 1.25)
          )
        ),
        GraphSpec(
          "weighted-cycle",
          Vector("A", "B", "C", "D"),
          Vector(
            (0, 1, 1.0),
            (1, 2, 2.0),
            (2, 3, 0.5),
            (3, 0, 0.25)
          )
        ),
        GraphSpec(
          "disconnected",
          Vector("A", "B", "C"),
          Vector((0, 1, 3.0))
        )
      )
    val samples = specs.map(sample)
    val edgeLengths = lengths()
    val trace =
      WeightedShortestPathAPSP
        .prepare(samples, edgeLengths)
        .toOption
        .get
    val observed =
      trace
        .occurrences(recipe(edgeLengths))
        .toOption
        .get

    specs.zip(observed).foreach { case (spec, occurrences) =>
      assertEquals(
        partCounts(occurrences.map(_.part)),
        partCounts(referenceParts(spec)),
        spec.key
      )
    }
    assertEquals(trace.stats.graphTransforms, specs.size)
    assertEquals(
      trace.stats.sourceSearches,
      specs.map(_.labels.size).sum
    )

  test("zero-length edges yield zero distance between distinct vertices"):
    val spec =
      GraphSpec(
        "zero",
        Vector("A", "B", "C"),
        Vector(
          (0, 1, 0.0),
          (1, 2, 1.0)
        )
      )
    val edgeLengths = lengths()
    val trace =
      WeightedShortestPathAPSP
        .prepare(Vector(sample(spec)), edgeLengths)
        .toOption
        .get
    val observed =
      trace
        .occurrences(recipe(edgeLengths))
        .toOption
        .get
        .head

    assertEquals(observed.size, 3)
    assert(
      observed.exists(_.part.distance.toDouble == 0.0)
    )

  test("negative and nonfinite edge lengths fail with edge provenance"):
    Vector(
      -1.0 -> classOf[NumericError.Negative],
      Double.NaN -> classOf[NumericError.NonFinite],
      Double.PositiveInfinity -> classOf[NumericError.NonFinite]
    ).foreach { case (invalid, expectedReason) =>
      val spec =
        GraphSpec(
          s"invalid-$invalid",
          Vector("A", "B"),
          Vector((0, 1, invalid))
        )
      val result =
        WeightedShortestPathAPSP.prepare(
          Vector(sample(spec)),
          lengths()
        )

      result match
        case Left(
              WeightedShortestPathAPSPError.EdgeProjection(
                PrepareError.InvalidNumeric(
                  SourceRef.Edge(sample, _: Edge[?]),
                  projection,
                  value,
                  reason
                )
              )
            ) =>
          assertEquals(sample.value, spec.key)
          assertEquals(
            projection,
            ProjectionDescriptor("bond.length")
          )
          if invalid.isNaN then assert(value.isNaN)
          else assertEquals(value, invalid)
          assert(
            expectedReason.isInstance(reason),
            clues(expectedReason, reason)
          )
        case other =>
          fail(s"expected edge projection failure, observed $other")
    }

  test("overflow during path accumulation is reported"):
    val maximumFiniteEdge =
      GraphSpec(
        "maximum-finite-edge",
        Vector("A", "B"),
        Vector((0, 1, Double.MaxValue))
      )
    assert(
      WeightedShortestPathAPSP
        .prepare(
          Vector(sample(maximumFiniteEdge)),
          lengths()
        )
        .isRight
    )

    val spec =
      GraphSpec(
        "overflow",
        Vector("A", "B", "C"),
        Vector(
          (0, 1, Double.MaxValue),
          (1, 2, Double.MaxValue)
        )
      )
    val result =
      WeightedShortestPathAPSP.prepare(
        Vector(sample(spec)),
        lengths()
      )

    assert(
      result.left.exists(
        _.isInstanceOf[
          WeightedShortestPathAPSPError.DistanceOverflow[Int]
        ]
      )
    )

  test("one weighted trace serves multiple node projections"):
    val spec =
      GraphSpec(
        "shared",
        Vector("Carbon", "Oxygen"),
        Vector((0, 1, 1.5))
      )
    val edgeLengths = lengths()
    val trace =
      WeightedShortestPathAPSP
        .prepare(Vector(sample(spec)), edgeLengths)
        .toOption
        .get
    val full =
      trace
        .occurrences(recipe(edgeLengths))
        .toOption
        .get
    val initials =
      trace
        .occurrences(
          WeightedShortestPaths.parts(
            Projection.of[String, Char](_.head),
            edgeLengths
          )
        )
        .toOption
        .get

    assertEquals(full.head.size, 1)
    assertEquals(initials.head.size, 1)
    assertEquals(trace.stats.graphTransforms, 1)
    assertEquals(trace.stats.sourceSearches, 2)

  test("a trace rejects a recipe with different edge-length semantics"):
    val spec =
      GraphSpec(
        "mismatch",
        Vector("A", "B"),
        Vector((0, 1, 1.0))
      )
    val preparedLengths = lengths()
    val otherLengths =
      lengths(
        Projection.of(bond => bond.length * 2.0)
      )
    val trace =
      WeightedShortestPathAPSP
        .prepare(Vector(sample(spec)), preparedLengths)
        .toOption
        .get

    assertEquals(
      trace.occurrences(recipe(otherLengths)).left.toOption,
      Some(
        WeightedAPSPTraceError.EdgeLengthPlanMismatch(
          preparedLengths.planNodeId.number,
          otherLengths.planNodeId.number
        )
      )
    )
