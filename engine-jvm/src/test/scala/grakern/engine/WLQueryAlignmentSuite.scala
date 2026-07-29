package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLQueryAlignmentSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val refinement =
    WLRefinement
      .create(
        1,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get

  private val kernel =
    (
      refinement.subtree +
        refinement.optimalAssignment
    ).normalized

  private val training =
    val topology =
      Graph
        .fromEdges(Link(AtomId(1), AtomId(2)))
        .toOption
        .get
    Vector(
      GraphSample.build(SampleKey("train-pair"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    )

  private val query =
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(10), AtomId(20)),
          Link(AtomId(20), AtomId(30))
        )
        .toOption
        .get
    Vector(
      GraphSample.build(SampleKey("query-path"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    )

  test("aligned queries retain query-only norm contributions"):
    val prepared =
      WLCompiler.plan(training, kernel).run().toOption.get
    val result = prepared.cross(query).toOption.get

    assertEquals(result.features.shared.rows, 1)
    assertEquals(
      result.features.shared.cols,
      prepared.features.columns.size
    )
    assert(result.features.novelSquaredNorm(0) > 0.0)
    assertEqualsDouble(
      result.features.completeSquaredNorm(0).get,
      1.0,
      1e-12
    )
    assertEquals(
      result.executionStats,
      WLQueryExecutionStats(1)
    )

  test("aligned cross values agree with a joint-corpus oracle"):
    val prepared =
      WLCompiler.plan(training, kernel).run().toOption.get
    val observed =
      prepared.cross(query).toOption.get.cross.values(0, 0)
    val joint =
      WLCompiler
        .plan(training ++ query, kernel)
        .run()
        .toOption
        .get

    assertEqualsDouble(
      observed,
      joint.kernel(1, 0).get,
      1e-12
    )

  test("an unseen final leaf still shares its known OA ancestor"):
    val assignment =
      WLCompiler
        .plan(training, refinement.optimalAssignment)
        .run()
        .toOption
        .get
    val result =
      assignment.cross(query).toOption.get

    assert(result.features.novelSquaredNorm(0) > 0.0)
    assert(
      result.cross.values(0, 0) > 0.0,
      "the novel degree-two leaf should match through its known C ancestor"
    )

  test("query preparation does not mutate training state"):
    val prepared =
      WLCompiler.plan(training, kernel).run().toOption.get
    val gramBefore = prepared.gram.values(0, 0)
    val columnsBefore = prepared.features.columns.size
    val first = prepared.cross(query).toOption.get
    val second = prepared.cross(query).toOption.get

    assertEqualsDouble(
      first.cross.values(0, 0),
      second.cross.values(0, 0),
      0.0
    )
    assertEquals(prepared.gram.values(0, 0), gramBefore)
    assertEquals(prepared.features.columns.size, columnsBefore)
