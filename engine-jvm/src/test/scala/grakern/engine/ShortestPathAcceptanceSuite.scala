package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Edge, Graph, Link}
import munit.FunSuite

final class ShortestPathAcceptanceSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  final private case class Bond(length: Double)

  private def sample(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int, Double)]
  ): GraphSample[Int, String, Bond] =
    val topology =
      Graph
        .of(
          labels.indices,
          edges.map { case (left, right, _) =>
            Link(left, right)
          }
        )
        .toOption
        .get
    val weights =
      edges.map { case (left, right, length) =>
        endpointKey(left, right) -> length
      }.toMap
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = labels.apply,
      edgeValue = edge => Bond(weights(endpointKey(edge.first, edge.second)))
    )

  private def endpointKey(
      left: Int,
      right: Int
  ): (Int, Int) =
    math.min(left, right) -> math.max(left, right)

  private val samples =
    Vector(
      sample(
        "pair",
        Vector("C", "O"),
        Vector((0, 1, 1.0))
      ),
      sample(
        "path",
        Vector("C", "O", "C"),
        Vector((0, 1, 1.0), (1, 2, 1.0))
      ),
      sample(
        "disconnected",
        Vector("C", "O", "C"),
        Vector((0, 1, 1.0))
      )
    )

  private val categoricalRecipe =
    UnweightedShortestPaths.parts[String, Bond, String](
      Projection.of(identity)
    )

  private def referenceKernel(
      left: GraphSample[Int, String, Bond],
      right: GraphSample[Int, String, Bond]
  ): Double =
    val leftParts =
      categoricalRecipe
        .materializeReference(left)
        .toOption
        .get
        .map(_.part)
    val rightParts =
      categoricalRecipe
        .materializeReference(right)
        .toOption
        .get
        .map(_.part)
    leftParts.iterator
      .flatMap(leftPart =>
        rightParts.iterator.map(rightPart =>
          if CategoricalShortestPathFeatures.sameFeature(
              leftPart,
              rightPart
            )
          then 1.0
          else 0.0
        )
      )
      .sum

  test("dual lowering shares one BFS trace and agrees with Floyd denotation"):
    val plan =
      ShortestPathPlanner.categorical(
        samples,
        categoricalRecipe,
        CategoricalShortestPathPlanOptions(
          lowering = CategoricalShortestPathLowering.ValidateBoth
        )
      )
    val prepared = plan.run().toOption.get

    samples.indices.foreach { left =>
      samples.indices.foreach { right =>
        assertEqualsDouble(
          prepared.gram.values(left, right),
          referenceKernel(samples(left), samples(right)),
          0.0
        )
      }
    }
    assertEquals(
      plan.graph.count(
        ShortestPathPlanNodeKind.UnweightedBfsAPSP
      ),
      1
    )
    assertEquals(
      plan.graph.consumers(plan.graph.apspNodeId).size,
      2
    )
    assertEquals(
      prepared.planReport.denotationMaximumDifference,
      Some(0.0)
    )

  test("soft plan selects checked Dijkstra and agrees with direct convolution"):
    val gamma = PositiveFinite.from(2.0).toOption.get
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    val kernel =
      SoftShortestPath.kernel(
        WeightedShortestPaths.parts(
          Projection.of[String, String](identity),
          lengths
        ),
        Kernel.delta[String],
        Kernel.rbf(gamma)
      )
    val softSamples =
      Vector(
        sample(
          "left",
          Vector("C", "O"),
          Vector((0, 1, 1.0))
        ),
        sample(
          "right",
          Vector("C", "O"),
          Vector((0, 1, 1.5))
        )
      )
    val plan =
      ShortestPathPlanner.soft(
        softSamples,
        kernel
      )
    val prepared = plan.run().toOption.get

    assertEquals(
      prepared.planReport.algorithm,
      ShortestPathAPSPAlgorithm.WeightedDijkstra
    )
    assertEquals(
      plan.graph.count(
        ShortestPathPlanNodeKind.WeightedDijkstraAPSP
      ),
      1
    )
    val expected =
      math.exp(-gamma.toDouble * 0.5 * 0.5)
    assertEqualsDouble(
      prepared.gram.values(0, 1),
      expected,
      1e-12
    )

  test("invalid edge weights remain typed checked-preparation failures"):
    val invalid =
      sample(
        "invalid",
        Vector("C", "O"),
        Vector((0, 1, Double.NaN))
      )
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    val kernel =
      SoftShortestPath.kernel(
        WeightedShortestPaths.parts(
          Projection.of[String, String](identity),
          lengths
        ),
        Kernel.delta[String],
        Kernel.rbf(PositiveFinite.from(1.0).toOption.get)
      )
    val result =
      ShortestPathPlanner
        .soft(Vector(invalid), kernel)
        .run()

    assert(
      result.left.exists {
        case SoftShortestPathCompileError.Dijkstra(
              WeightedShortestPathAPSPError.EdgeProjection(
                PrepareError.InvalidNumeric(
                  SourceRef.Edge(sampleKey, _: EdgeRef[?]),
                  projection,
                  value,
                  NumericError.NonFinite(_)
                )
              )
            ) =>
          sampleKey.value == "invalid" &&
          projection.value == "bond.length" &&
          value.isNaN
        case _ =>
          false
      }
    )
