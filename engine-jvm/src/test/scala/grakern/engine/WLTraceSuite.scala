package grakern.engine

import cats.Hash
import cats.kernel.Eq
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLTraceSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

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

  test("packed graphs produce every requested colour level and source"):
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(1), AtomId(2)),
          Link(AtomId(2), AtomId(3))
        )
        .toOption
        .get
    val sample =
      GraphSample.build(SampleKey("chain"), topology)(
        nodeValue = id => Atom(if id.value == 2 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    val specification = refinement()
    val trace =
      WLTrace.prepare(Vector(sample), specification).toOption.get

    assertEquals(trace.rounds, 2)
    assertEquals(trace.vertexCount, 3)
    assertEquals(trace.graphColours(0, 0).map(_.size), Some(3))
    assertEquals(trace.graphColours(0, 1).map(_.size), Some(3))
    assertEquals(trace.graphColours(0, 2).map(_.size), Some(3))
    assertEquals(
      trace.source(0).map(_.sample),
      Some(SampleKey("chain"))
    )
    assertEquals(
      trace.refinementNodeId.number,
      specification.planNodeId.number
    )

  test("every refined signature points to the same vertex at prior level"):
    val topology =
      Graph.fromEdges(Link(AtomId(1), AtomId(2))).toOption.get
    val sample =
      GraphSample.build(SampleKey("pair"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val trace =
      WLTrace.prepare(Vector(sample), refinement()).toOption.get

    (1 to trace.rounds).foreach { level =>
      (0 until trace.vertexCount).foreach { vertex =>
        val current = trace.colour(level, vertex).get
        val previous = trace.colour(level - 1, vertex).get
        assertEquals(
          trace.signature(current).flatMap(_.parent),
          Some(previous)
        )
      }
    }

  test("symmetric vertices share exact colours within one trace"):
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(1), AtomId(2)),
          Link(AtomId(2), AtomId(3))
        )
        .toOption
        .get
    val sample =
      GraphSample.build(SampleKey("symmetric"), topology)(
        nodeValue = id => Atom(if id.value == 2 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    val trace =
      WLTrace.prepare(Vector(sample), refinement()).toOption.get
    val endpointIndices =
      (0 until trace.vertexCount).filter { index =>
        trace.source(index).exists(source => source.vertex.value != 2)
      }

    (0 to trace.rounds).foreach { level =>
      assertEquals(endpointIndices.size, 2)
      assertEquals(
        trace.colour(level, endpointIndices.head),
        trace.colour(level, endpointIndices.last)
      )
    }

  test("edge keys participate in refined identity"):
    val topology =
      Graph.fromEdges(Link(AtomId(1), AtomId(2))).toOption.get
    val single =
      GraphSample.build(SampleKey("single"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val double =
      GraphSample.build(SampleKey("double"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(2)
      )
    val trace =
      WLTrace
        .prepare(Vector(single, double), refinement(rounds = 1))
        .toOption
        .get
    val singleColour = trace.graphColours(0, 1).get.head
    val doubleColour = trace.graphColours(1, 1).get.head

    assertNotEquals(singleColour, doubleColour)
    assertEquals(
      trace.graphColours(0, 0).get.head,
      trace.graphColours(1, 0).get.head
    )
