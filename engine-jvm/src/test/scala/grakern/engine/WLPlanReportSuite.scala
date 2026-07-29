package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLPlanReportSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def prepared =
    val topology =
      Graph
        .fromEdges(Link(AtomId(1), AtomId(2)))
        .toOption
        .get
    val sample =
      GraphSample.build(SampleKey("pair"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
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
    WLCompiler
      .plan(Vector(sample), kernel)
      .run()
      .toOption
      .get

  test("prepared capabilities describe every exact E4 view"):
    val capabilities = prepared.capabilities

    assert(capabilities.exactFeatures)
    assert(capabilities.gram)
    assert(capabilities.cross)
    assert(capabilities.operator)
    assert(capabilities.queryAlignment)
    assert(capabilities.provenance)

  test("the report exposes one refinement with two consumers"):
    val report = prepared.planReport
    val refinement =
      report.graph.nodes
        .find(
          _.kind == WLPlanNodeKind.Refinement
        )
        .get

    assertEquals(
      report.graph.consumers(refinement.id).size,
      2
    )
    assertEquals(report.executionStats.refinementExecutions, 1)
    assertEquals(report.sampleCount, 1)
    assertEquals(
      report.featureCount,
      prepared.features.columns.size
    )
    assertEquals(
      report.featureNonZeros,
      prepared.features.values.nnz
    )
    assert(report.featureDensity > 0.0)
    assert(report.normalized)
    assert(report.queryAlignmentRetainsNovelNorm)

  test("explain discloses realizations and durable-cache blockers"):
    val explanation = prepared.planReport.explain

    assert(explanation.contains("WL execution plan"))
    assert(explanation.contains("SubtreeFeatures"))
    assert(
      explanation.contains(
        "OptimalAssignmentThresholdFeatures"
      )
    )
    assert(explanation.contains("operator: FeatureBacked"))
    assert(explanation.contains("anonymous projection"))
    assert(
      explanation.contains(
        "missing corpus content fingerprint"
      )
    )
    assert(
      explanation.contains(
        "frozen dictionary with novel squared norm = true"
      )
    )
