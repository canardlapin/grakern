package grakern.engine

import cats.kernel.{Eq, Hash}
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class SoftShortestPathCompilerSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

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

  private val gamma =
    PositiveFinite.from(2.0).toOption.get

  private def softKernel(
      distanceKernel: Kernel[Double] = Kernel.rbf(gamma)
  ): SoftShortestPathKernel[String, Bond, String] =
    val edgeLengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    SoftShortestPath.kernel(
      WeightedShortestPaths.parts(
        Projection.of(identity),
        edgeLengths
      ),
      Kernel.delta[String],
      distanceKernel
    )

  test("single-path values follow the lawful symmetrized part formula"):
    val left =
      sample(
        "left",
        Vector("C", "O"),
        Vector((0, 1, 1.0))
      )
    val right =
      sample(
        "right",
        Vector("C", "O"),
        Vector((0, 1, 1.2))
      )
    val compiled =
      SoftShortestPathCompiler
        .prepare(Vector(left, right), softKernel())
        .toOption
        .get
    val expected =
      math.exp(-gamma.toDouble * 0.2 * 0.2)

    assertEqualsDouble(
      compiled.kernel(0, 1).get,
      expected,
      1e-14
    )
    assertEquals(compiled.kernel(0, 0), Some(1.0))
    assertEquals(
      compiled.executionStats.graphPairEvaluations,
      3L
    )
    assertEquals(
      compiled.executionStats.partPairEvaluations,
      3L
    )

  test("equal endpoints retain both tensor-symmetrized matches"):
    val graph =
      sample(
        "equal",
        Vector("C", "C"),
        Vector((0, 1, 0.5))
      )
    val compiled =
      SoftShortestPathCompiler
        .prepare(Vector(graph), softKernel())
        .toOption
        .get

    assertEquals(compiled.kernel(0, 0), Some(2.0))

  test("normalization makes nonzero diagonals one and preserves zero graphs"):
    val pair =
      sample(
        "pair",
        Vector("C", "O"),
        Vector((0, 1, 1.0))
      )
    val singleton =
      sample(
        "singleton",
        Vector("C"),
        Vector.empty
      )
    val tolerance =
      NonNegativeFinite.from(1e-12).toOption.get
    val compiled =
      SoftShortestPathCompiler
        .prepare(
          Vector(pair, singleton),
          softKernel().normalized,
          tolerance
        )
        .toOption
        .get

    assert(compiled.normalized)
    assertEquals(compiled.kernel(0, 0), Some(1.0))
    assertEquals(compiled.kernel(0, 1), Some(0.0))
    assertEquals(compiled.kernel(1, 1), Some(0.0))
    assertEquals(compiled.auditEvents.size, 1)

  test("the exact Gram matrix satisfies sampled PSD quadratic forms"):
    val samples =
      Vector(
        sample(
          "co",
          Vector("C", "O"),
          Vector((0, 1, 1.0))
        ),
        sample(
          "cn",
          Vector("C", "N"),
          Vector((0, 1, 1.5))
        ),
        sample(
          "cc",
          Vector("C", "C"),
          Vector((0, 1, 0.5))
        )
      )
    val compiled =
      SoftShortestPathCompiler
        .prepare(samples, softKernel())
        .toOption
        .get
    val vectors =
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, -1.0, 0.5),
        Vector(-2.0, 3.0, 1.0),
        Vector(0.25, -0.75, 2.0)
      )

    vectors.foreach { vector =>
      val quadratic =
        vector.indices.iterator
          .flatMap(left =>
            vector.indices.iterator.map(right =>
              vector(left) *
                compiled.gram.values(left, right) *
                vector(right)
            )
          )
          .sum
      assert(
        quadratic >= -1e-12,
        clues(vector, quadratic)
      )
    }

  test("invalid edge values remain typed preparation failures"):
    val graph =
      sample(
        "invalid",
        Vector("C", "O"),
        Vector((0, 1, Double.NaN))
      )
    val result =
      SoftShortestPathCompiler.prepare(
        Vector(graph),
        softKernel()
      )

    assert(
      result.left.exists {
        case SoftShortestPathCompileError.Dijkstra(
              WeightedShortestPathAPSPError.EdgeProjection(
                PrepareError.InvalidNumeric(
                  _,
                  _,
                  value,
                  NumericError.NonFinite(_)
                )
              )
            ) =>
          value.isNaN
        case _ =>
          false
      }
    )

  test("nonfinite part-kernel output reports its numerical stage"):
    val graph =
      sample(
        "nan-kernel",
        Vector("C", "O"),
        Vector((0, 1, 1.0))
      )
    val invalidDistance =
      unsafe.assumeKernel[Double]((_, _) => Double.NaN)
    val result =
      SoftShortestPathCompiler.prepare(
        Vector(graph),
        softKernel(invalidDistance)
      )

    assert(
      result.left.exists {
        case SoftShortestPathCompileError.Numeric(error) =>
          error.stage == SoftPartEvaluationStage.DistanceKernel &&
          error.leftSample.value == "nan-kernel" &&
          error.rightSample.value == "nan-kernel"
        case _ =>
          false
      }
    )

  test("normalization rejects a materially negative diagonal"):
    val graph =
      sample(
        "negative-diagonal",
        Vector("C", "O"),
        Vector((0, 1, 1.0))
      )
    val negativeDistance =
      unsafe.assumeKernel[Double]((_, _) => -1.0)
    val result =
      SoftShortestPathCompiler.prepare(
        Vector(graph),
        softKernel(negativeDistance).normalized
      )

    assert(
      result.left.exists(
        _.isInstanceOf[
          SoftShortestPathCompileError.Normalization[Int]
        ]
      )
    )

  test("vertex reindexing leaves soft shortest-path values unchanged"):
    val original =
      sample(
        "original",
        Vector("C", "N", "O"),
        Vector(
          (0, 1, 0.5),
          (1, 2, 1.0)
        )
      )
    val reindexed =
      sample(
        "reindexed",
        Vector("O", "N", "C"),
        Vector(
          (2, 1, 0.5),
          (1, 0, 1.0)
        )
      )
    val compiled =
      SoftShortestPathCompiler
        .prepare(
          Vector(original, reindexed),
          softKernel()
        )
        .toOption
        .get

    assertEqualsDouble(
      compiled.kernel(0, 0).get,
      compiled.kernel(0, 1).get,
      1e-12
    )
    assertEqualsDouble(
      compiled.kernel(1, 1).get,
      compiled.kernel(0, 1).get,
      1e-12
    )
