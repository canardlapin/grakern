package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite
import scala.io.Source
import scala.util.Using

final class GraphKernelsJlParitySuite extends FunSuite:
  final case class Atom(label: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private val revision =
    "f04797de0b86a39172b6adbcfcee35ec4b39f91b"
  private val fixture =
    Using.resource(
      Source.fromResource(
        s"graphkernels-jl/$revision/wl_regular_uniform.json"
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

  private def samples: Vector[GraphSample[Int, Atom, Bond]] =
    val pair =
      Graph
        .of(Vector(1, 2), Vector(Link(1, 2)))
        .toOption
        .get
    val triangle =
      Graph
        .of(
          Vector(1, 2, 3),
          Vector(Link(1, 2), Link(2, 3), Link(1, 3))
        )
        .toOption
        .get
    val cycle =
      Graph
        .of(
          Vector(1, 2, 3, 4),
          Vector(
            Link(1, 2),
            Link(2, 3),
            Link(3, 4),
            Link(4, 1)
          )
        )
        .toOption
        .get
    Vector(
      GraphSample.build(SampleKey("pair"), pair)(
        nodeValue = _ => Atom("A"),
        edgeValue = _ => Bond(1)
      ),
      GraphSample.build(SampleKey("triangle"), triangle)(
        nodeValue = _ => Atom("A"),
        edgeValue = _ => Bond(1)
      ),
      GraphSample.build(SampleKey("cycle4"), cycle)(
        nodeValue = _ => Atom("A"),
        edgeValue = _ => Bond(1)
      )
    )

  test("fixture records the exact revision and audited parity boundary"):
    assert(
      fixture.contains(
        s"\"source_revision\": \"$revision\""
      )
    )
    assert(
      fixture.contains(
        "\"scope_restriction\": \"uniform-labeled regular graphs only\""
      )
    )
    assert(
      fixture.contains("\"broad_wl_parity\": \"excluded\"")
    )
    assert(
      fixture.contains("\"graphkernels_num_iterations\": 3")
    )

  test("aligned WL subtree Gram agrees with pinned GraphKernels.jl"):
    val refinement =
      WLRefinement
        .create(
          2,
          Projection.of[Atom, String](_.label),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    val prepared =
      WLCompiler
        .plan(samples, refinement.subtree)
        .run()
        .toOption
        .get
    val observed =
      Vector.tabulate(prepared.graphCount * prepared.graphCount) { index =>
        prepared.gram.values(
          index / prepared.graphCount,
          index % prepared.graphCount
        )
      }
    val expected = flatValues("wl_subtree_gram_flat")

    assertEquals(observed.size, expected.size)
    observed.zip(expected).zipWithIndex.foreach { case ((actual, target), index) =>
      assertEqualsDouble(
        actual,
        target,
        1e-12,
        clues(index)
      )
    }
