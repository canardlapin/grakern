package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLQueryTraceSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def refinement(rounds: Int = 2) =
    WLRefinement
      .create(
        rounds,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get

  private def pair(
      key: String,
      left: Int = 1,
      right: Int = 2
  ): GraphSample[AtomId, Atom, Bond] =
    val topology =
      Graph
        .fromEdges(Link(AtomId(left), AtomId(right)))
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = _ => Atom("C"),
      edgeValue = _ => Bond(1)
    )

  test("an identical query reuses every training signature"):
    val specification = refinement()
    val training =
      WLTrace
        .prepare(Vector(pair("train")), specification)
        .toOption
        .get
    val query =
      WLQueryTrace
        .prepare(
          training,
          Vector(pair("query", 10, 20)),
          specification
        )
        .toOption
        .get

    assertEquals(query.novelSignatureCount, 0)
    (0 to query.rounds).foreach { level =>
      query.graphColours(0, level).get.foreach { colour =>
        assert(!query.isNovel(colour))
      }
    }

  test("a novel query descendant retains a known training ancestor"):
    val specification = refinement(rounds = 1)
    val training =
      WLTrace
        .prepare(Vector(pair("train")), specification)
        .toOption
        .get
    val queryTopology =
      Graph
        .fromEdges(
          Link(AtomId(1), AtomId(2)),
          Link(AtomId(2), AtomId(3))
        )
        .toOption
        .get
    val querySample =
      GraphSample.build(SampleKey("query-chain"), queryTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val query =
      WLQueryTrace
        .prepare(training, Vector(querySample), specification)
        .toOption
        .get
    val centre =
      (0 until query.vertexCount).find { index =>
        query.source(index).exists(_.vertex.value == 2)
      }.get
    val parent = query.colour(0, centre).get
    val descendant = query.colour(1, centre).get

    assert(!query.isNovel(parent))
    assert(query.isNovel(descendant))
    assertEquals(
      query.signature(descendant).flatMap(_.parent),
      Some(parent)
    )

  test("query preparation leaves the training interner unchanged"):
    val specification = refinement(rounds = 1)
    val training =
      WLTrace
        .prepare(Vector(pair("train")), specification)
        .toOption
        .get
    val before = training.signatureCount
    val queryTopology = Graph.singleton(AtomId(99))
    val querySample =
      GraphSample.build(SampleKey("novel"), queryTopology)(
        nodeValue = _ => Atom("Xe"),
        edgeValue = _ => Bond(1)
      )
    val query =
      WLQueryTrace
        .prepare(training, Vector(querySample), specification)
        .toOption
        .get

    assert(query.novelSignatureCount > 0)
    assertEquals(training.signatureCount, before)

  test("a different refinement plan cannot query a prepared trace"):
    val trainingRefinement = refinement(rounds = 1)
    val otherRefinement = refinement(rounds = 1)
    val training =
      WLTrace
        .prepare(Vector(pair("train")), trainingRefinement)
        .toOption
        .get
    val result =
      WLQueryTrace.prepare(
        training,
        Vector(pair("query")),
        otherRefinement
      )

    assert(
      result.left.exists {
        case WLQueryTraceError.RefinementMismatch(_, _) => true
        case _                                          => false
      }
    )
