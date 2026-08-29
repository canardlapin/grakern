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

  test("single-query sparse results agree bit-for-bit with the batch API"):
    val prepared = WLCompiler.plan(training, kernel).run().toOption.get
    val single = prepared.query(query.head).toOption.get
    val batch = prepared.cross(query).toOption.get
    val batchRow = batch.features.sparseRow(0).get

    assertEquals(single.features.shared, batchRow)
    assertEquals(single.features.novelSquaredNorm, batch.features.novelSquaredNorm(0))
    assertEquals(single.features.completeSquaredNorm, batch.features.completeSquaredNorm(0).get)
    assertEquals(single.cross.referenceColumns, batch.cross.referenceColumns)
    assertEquals(
      single.cross.indices.map(index => index -> single.cross.valueAt(index)),
      single.cross.indices.map(index => index -> batch.cross.values(0, index))
    )
    assertEquals(single.executionStats, batch.executionStats)

  test("one thousand incremental queries leave the prepared state structurally unchanged"):
    val prepared = WLCompiler.plan(training, kernel).run().toOption.get
    val columnsBefore = prepared.features.columns.coordinates
    val fingerprintBefore = prepared.semanticFingerprint
    val gramBefore = prepared.gram.values(0, 0)
    val sparseBefore = prepared.features.sparseRow(0)

    var index = 0
    while index < 1000 do
      assert(prepared.query(query.head).isRight)
      index += 1

    assertEquals(prepared.features.columns.coordinates, columnsBefore)
    assertEquals(prepared.semanticFingerprint, fingerprintBefore)
    assertEquals(prepared.gram.values(0, 0), gramBefore)
    assertEquals(prepared.features.sparseRow(0), sparseBefore)
