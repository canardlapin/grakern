package grakern.engine

import cats.kernel.Hash
import gale.linalg.DVec
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class ShortestPathPlanSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
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
        Vector(
          (0, 1, 1.0),
          (1, 2, 1.0)
        )
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

  test("categorical planner selects BFS and exact sparse features"):
    val prepared =
      ShortestPathPlanner
        .categorical(samples, categoricalRecipe)
        .run()
        .toOption
        .get

    assertEquals(
      prepared.planReport.algorithm,
      ShortestPathAPSPAlgorithm.UnweightedBfs
    )
    assertEquals(
      prepared.planReport.realization,
      ShortestPathNumericalRealization.ExactSparseFeatures
    )
    assert(prepared.features.isRight)
    assert(prepared.provenance.isRight)
    assert(prepared.capabilities.exactFeatures)
    assertEquals(
      prepared.operator.realization,
      KernelOperatorRealization.FeatureBacked
    )
    assert(
      prepared.planReport.explain
        .contains("UnweightedBfs")
    )

  test("pairwise categorical lowering preserves denotation and hides features"):
    val exact =
      ShortestPathPlanner
        .categorical(samples, categoricalRecipe)
        .run()
        .toOption
        .get
    val pairwise =
      ShortestPathPlanner
        .categorical(
          samples,
          categoricalRecipe,
          CategoricalShortestPathPlanOptions(
            lowering = CategoricalShortestPathLowering.Pairwise
          )
        )
        .run()
        .toOption
        .get

    samples.indices.foreach { left =>
      samples.indices.foreach { right =>
        assertEquals(
          pairwise.kernel(left, right),
          exact.kernel(left, right)
        )
      }
    }
    assertEquals(
      pairwise.features.left.toOption,
      Some(
        ShortestPathFeatureUnavailable.PairwiseLoweringSelected
      )
    )
    assertEquals(
      pairwise.provenance.left.toOption,
      Some(
        ShortestPathProvenanceUnavailable.PairwiseLoweringSelected
      )
    )
    assertEquals(
      pairwise.operator.realization,
      KernelOperatorRealization.GramBacked
    )
    assert(!pairwise.capabilities.exactFeatures)

  test("dual validation shares one BFS node across both consumers"):
    val plan =
      ShortestPathPlanner.categorical(
        samples,
        categoricalRecipe,
        CategoricalShortestPathPlanOptions(
          lowering = CategoricalShortestPathLowering.ValidateBoth
        )
      )
    val prepared =
      plan.run().toOption.get
    val consumers =
      plan.graph
        .consumers(plan.graph.apspNodeId)
        .map(_.kind)
        .toSet

    assertEquals(
      plan.graph.count(
        ShortestPathPlanNodeKind.UnweightedBfsAPSP
      ),
      1
    )
    assertEquals(
      consumers,
      Set(
        ShortestPathPlanNodeKind.CategoricalFeatures,
        ShortestPathPlanNodeKind.CategoricalPairwise
      )
    )
    assertEquals(
      prepared.planReport.denotationMaximumDifference,
      Some(0.0)
    )
    assertEquals(
      prepared.planReport.apspStats.graphTransforms,
      samples.size
    )
    assertEquals(
      prepared.planReport.realization,
      ShortestPathNumericalRealization.DualValidatedFeatures
    )

  test("feature-backed and Gram-backed operators reproduce Gram products"):
    val exact =
      ShortestPathPlanner
        .categorical(samples, categoricalRecipe)
        .run()
        .toOption
        .get
    val pairwise =
      ShortestPathPlanner
        .categorical(
          samples,
          categoricalRecipe,
          CategoricalShortestPathPlanOptions(
            lowering = CategoricalShortestPathLowering.Pairwise
          )
        )
        .run()
        .toOption
        .get
    val input =
      DVec.fromSeq(Vector(0.25, -0.5, 1.5))

    Vector(exact, pairwise).foreach { prepared =>
      val observed = prepared.operator.values(input)
      samples.indices.foreach { row =>
        val expected =
          samples.indices.map { column =>
            prepared.gram.values(row, column) *
              input(column)
          }.sum
        assertEqualsDouble(
          observed(row),
          expected,
          1e-12
        )
      }
    }

  test("soft planner selects Dijkstra, pairwise Gram, and Gram-backed operator"):
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    val gamma =
      PositiveFinite.from(1.5).toOption.get
    val kernel =
      SoftShortestPath
        .kernel(
          WeightedShortestPaths.parts(
            Projection.of[String, String](identity),
            lengths
          ),
          Kernel.delta[String],
          Kernel.rbf(gamma)
        )
        .normalized
    val plan =
      ShortestPathPlanner.soft(
        samples,
        kernel,
        SoftShortestPathPlanOptions(
          diagonalTolerance = NonNegativeFinite.from(1e-12).toOption.get
        )
      )
    val result =
      plan.run()
    assert(result.isRight, clues(result))
    val prepared =
      result.toOption.get

    assertEquals(
      prepared.planReport.algorithm,
      ShortestPathAPSPAlgorithm.WeightedDijkstra
    )
    assertEquals(
      prepared.planReport.realization,
      ShortestPathNumericalRealization.ExactPairwise
    )
    assertEquals(
      prepared.operator.realization,
      KernelOperatorRealization.GramBacked
    )
    assert(!prepared.capabilities.exactFeatures)
    assertEquals(
      plan.graph.count(
        ShortestPathPlanNodeKind.WeightedDijkstraAPSP
      ),
      1
    )
    assertEquals(
      plan.graph.count(
        ShortestPathPlanNodeKind.Normalize
      ),
      1
    )
    assert(
      prepared.planReport.explain
        .contains("WeightedDijkstra")
    )
