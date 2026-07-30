package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite
import scala.io.Source
import scala.util.Using

final class GraphKernelsJlShortestPathParitySuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  private val revision =
    "f04797de0b86a39172b6adbcfcee35ec4b39f91b"
  private val fixture =
    Using.resource(
      Source.fromResource(
        s"graphkernels-jl/$revision/shortest_path.json"
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

  test("fixture pins the source revision and exposes the additive endpoint rule"):
    assert(
      fixture.contains(
        s"\"source_revision\": \"$revision\""
      )
    )
    assert(
      fixture.contains(
        "\"endpoint_combination\": \"addition\""
      )
    )
    assert(
      fixture.contains("\"raw_gram_divisor\": 8.0")
    )
    assert(
      fixture.contains(
        "\"status\": \"declared compatibility behavior, not Grakern's product-kernel denotation\""
      )
    )

  test("categorical shortest-path Gram agrees after both declared translations"):
    val prepared =
      ShortestPathPlanner
        .categorical(samples, recipe)
        .run()
        .toOption
        .get
    val expected =
      flatValues("shortest_path_gram_flat").map(_ / 8.0)
    val observed =
      Vector.tabulate(samples.size * samples.size) { index =>
        prepared.gram.values(
          index / samples.size,
          index % samples.size
        )
      }

    assertEquals(observed.size, expected.size)
    observed.zip(expected).zipWithIndex.foreach { case ((actual, target), index) =>
      assertEqualsDouble(
        actual,
        target,
        1e-12,
        clues(index)
      )
    }
