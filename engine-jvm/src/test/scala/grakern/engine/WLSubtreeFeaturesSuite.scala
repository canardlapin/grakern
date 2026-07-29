package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLSubtreeFeaturesSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def trace: WLTrace[AtomId, String, Int] =
    val chainTopology =
      Graph
        .fromEdges(
          Link(AtomId(1), AtomId(2)),
          Link(AtomId(2), AtomId(3))
        )
        .toOption
        .get
    val pairTopology =
      Graph
        .fromEdges(Link(AtomId(10), AtomId(20)))
        .toOption
        .get
    val chain =
      GraphSample.build(SampleKey("chain"), chainTopology)(
        nodeValue = id => Atom(if id.value == 2 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    val pair =
      GraphSample.build(SampleKey("pair"), pairTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val refinement =
      WLRefinement
        .create(
          1,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    WLTrace.prepare(Vector(chain, pair), refinement).toOption.get

  test("subtree rows exactly count round and signature occurrences"):
    val source = trace
    val features = WLSubtreeFeatures.unitWeighted(source)

    assertEquals(features.graphCount, 2)
    assertEquals(features.squaredNorm(0), Some(10.0))
    assertEquals(features.squaredNorm(1), Some(8.0))
    assertEquals(features.kernel(0, 1), Some(4.0))
    assertEquals(
      features.refinementNodeId.number,
      source.refinementNodeId.number
    )

  test("round weights scale feature blocks by their square roots"):
    val features =
      WLSubtreeFeatures
        .fromTrace(
          trace,
          Vector(
            NonNegativeFinite.one,
            NonNegativeFinite.from(4.0).toOption.get
          )
        )
        .toOption
        .get

    assertEquals(features.squaredNorm(0), Some(25.0))

  test("every active feature retains representative source vertices"):
    val features = WLSubtreeFeatures.unitWeighted(trace)

    assert(features.provenance.nonEmpty)
    assert(
      features.provenance.forall(_.occurrences.nonEmpty)
    )
    assert(
      features.provenance
        .flatMap(_.occurrences)
        .exists(_.sample.value == "chain")
    )

  test("the round-weight vector must cover the complete trace"):
    val result =
      WLSubtreeFeatures.fromTrace(
        trace,
        Vector(NonNegativeFinite.one)
      )

    assertEquals(
      result,
      Left(WLSubtreeFeatureError.WeightCount(2, 1))
    )
