package grakern.engine

import cats.kernel.Hash
import gale.linalg.DVec
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLNumericalViewsSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def compiled =
    val pairTopology =
      Graph
        .fromEdges(Link(AtomId(1), AtomId(2)))
        .toOption
        .get
    val pathTopology =
      Graph
        .fromEdges(
          Link(AtomId(10), AtomId(20)),
          Link(AtomId(20), AtomId(30))
        )
        .toOption
        .get
    val samples =
      Vector(
        GraphSample.build(SampleKey("pair"), pairTopology)(
          nodeValue = _ => Atom("C"),
          edgeValue = _ => Bond(1)
        ),
        GraphSample.build(SampleKey("path"), pathTopology)(
          nodeValue = id => Atom(if id.value == 20 then "O" else "C"),
          edgeValue = _ => Bond(1)
        )
      )
    val refinement =
      WLRefinement
        .create(
          2,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    val kernel =
      (
        refinement.subtree +
          refinement.optimalAssignment
      ).normalized
    WLCompiler.plan(samples, kernel).run().toOption.get

  test("the Gale Gram view reproduces scalar evaluation"):
    val prepared = compiled

    prepared.sampleKeys.indices.foreach { left =>
      prepared.sampleKeys.indices.foreach { right =>
        assertEqualsDouble(
          prepared.gram.values(left, right),
          prepared.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }
    assertEquals(prepared.gram.samples.keys, prepared.sampleKeys)

  test("the feature-backed operator computes Gram matrix products"):
    val prepared = compiled
    val input = DVec.fromSeq(Vector(0.25, -0.5))
    val observed = prepared.operator.values(input)
    val expected =
      DVec.tabulate(prepared.graphCount) { row =>
        var value = 0.0
        var column = 0
        while column < prepared.graphCount do
          value +=
            prepared.gram.values(row, column) * input(column)
          column += 1
        value
      }

    assertEquals(
      prepared.operator.realization,
      KernelOperatorRealization.FeatureBacked
    )
    observed.toSeq.zip(expected.toSeq).foreach { case (actual, reference) =>
      assertEqualsDouble(actual, reference, 1e-12)
    }
