package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite
import scala.io.Source
import scala.util.Using

final class GraKeLShortestPathParitySuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  private val fixture =
    Using.resource(
      Source.fromResource(
        "grakel/0.1.10/shortest_path.json"
      )
    )(_.mkString)

  private def flatValues(key: String): Vector[Double] =
    val marker = s"\"$key\": ["
    val start = fixture.indexOf(marker)
    assert(start >= 0, clues(key))
    val contentStart = start + marker.length
    val end = fixture.indexOf("]", contentStart)
    assert(end >= 0, clues(key))
    fixture
      .substring(contentStart, end)
      .split(",")
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toDouble)
      .toVector

  private def sample(
      key: String,
      vertexCount: Int,
      edges: Vector[(Int, Int)]
  ): GraphSample[Int, String, Unit] =
    val topology =
      Graph
        .of(
          0 until vertexCount,
          edges.map { case (left, right) =>
            Link(left, right)
          }
        )
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = _ => "A",
      edgeValue = _ => ()
    )

  private val samples =
    Vector(
      sample("pair", 2, Vector(0 -> 1)),
      sample(
        "triangle",
        3,
        Vector(0 -> 1, 1 -> 2, 0 -> 2)
      ),
      sample(
        "cycle4",
        4,
        Vector(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)
      )
    )

  private val recipe =
    UnweightedShortestPaths.parts[String, Unit, String](
      Projection.of(identity)
    )

  private def observedGram: Vector[Double] =
    val prepared =
      ShortestPathPlanner
        .categorical(samples, recipe)
        .run()
        .toOption
        .get
    Vector.tabulate(samples.size * samples.size) { index =>
      prepared.gram.values(
        index / samples.size,
        index % samples.size
      )
    }

  private def normalized(values: Vector[Double]): Vector[Double] =
    Vector.tabulate(values.size) { index =>
      val row = index / samples.size
      val column = index % samples.size
      val leftDiagonal = values(row * samples.size + row)
      val rightDiagonal =
        values(column * samples.size + column)
      val denominator =
        math.sqrt(leftDiagonal * rightDiagonal)
      if denominator == 0.0 then 0.0
      else values(index) / denominator
    }

  private def assertParity(
      observed: Vector[Double],
      expected: Vector[Double]
  ): Unit =
    assertEquals(observed.size, expected.size)
    observed.zip(expected).zipWithIndex.foreach { case ((actual, target), index) =>
      assertEqualsDouble(
        actual,
        target,
        1e-12,
        clues(index)
      )
    }

  test("fixture pins GraKeL 0.1.10 and declares the pair translation"):
    assert(
      fixture.contains(
        "\"distribution_version\": \"0.1.10\""
      )
    )
    assert(
      fixture.contains(
        "\"pair_enumeration\": \"ordered off-diagonal reachable vertex pairs\""
      )
    )
    assert(
      fixture.contains("\"raw_gram_divisor\": 4.0")
    )
    assert(
      fixture.contains(
        "\"authority\": \"compatibility evidence, not semantic oracle\""
      )
    )

  test("categorical shortest-path Gram agrees after explicit pair translation"):
    val translated =
      flatValues("shortest_path_gram_flat").map(_ / 4.0)

    assertParity(observedGram, translated)

  test("normalization removes GraKeL ordered-pair scale"):
    assertParity(
      normalized(observedGram),
      flatValues("shortest_path_normalized_gram_flat")
    )
