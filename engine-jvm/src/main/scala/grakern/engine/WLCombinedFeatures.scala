package grakern.engine

import grakern.core.*
import grakern.standard.wl.*

enum WLCombinedFeature:
  case Subtree(feature: WLSubtreeFeature)
  case OptimalAssignment(
      feature: HierarchyFeature[WLHierarchyNode]
  )

sealed trait WLCombinedFeatureError

object WLCombinedFeatureError:
  final case class Subtree(error: WLSubtreeFeatureError) extends WLCombinedFeatureError

  final case class Hierarchy(
      error: HierarchyError[WLHierarchyNode, WLColour]
  ) extends WLCombinedFeatureError

  final case class NonFiniteDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double
  ) extends WLCombinedFeatureError

  final case class NegativeDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double,
      tolerance: NonNegativeFinite
  ) extends WLCombinedFeatureError

final class WLCombinedFeatures[V, NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    val sampleKeys: Vector[SampleKey],
    private[grakern] val rawRows: Vector[
      ExactFeatureRow[WLCombinedFeature]
    ],
    private[grakern] val normalizedRows: Vector[
      ExactFeatureRow[WLCombinedFeature]
    ],
    private[grakern] val subtree: WLSubtreeFeatures[V],
    private[grakern] val optimalAssignment: WLOAThresholdFeatures[NodeKey, EdgeKey],
    val provenance: WLFeatureProvenanceIndex[V, NodeKey, EdgeKey],
    val auditEvents: Vector[NumericalAuditEvent]
):
  def graphCount: Int = sampleKeys.size

  def activeCoordinates(graphIndex: Int): Option[Int] =
    normalizedRows.lift(graphIndex).map(_.activeSize)

  def rawSquaredNorm(graphIndex: Int): Option[Double] =
    rawRows.lift(graphIndex).map(_.squaredNorm)

  def squaredNorm(graphIndex: Int): Option[Double] =
    normalizedRows.lift(graphIndex).map(_.squaredNorm)

  def rawKernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    for
      left <- rawRows.lift(leftGraph)
      right <- rawRows.lift(rightGraph)
    yield left.dot(right)(WLCombinedFeatures.sameFeature)

  def kernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    for
      left <- normalizedRows.lift(leftGraph)
      right <- normalizedRows.lift(rightGraph)
    yield left.dot(right)(WLCombinedFeatures.sameFeature)

  def explain(
      leftGraph: Int,
      rightGraph: Int
  ): Option[
    Vector[WLFeatureContribution[V, NodeKey, EdgeKey]]
  ] =
    for
      left <- normalizedRows.lift(leftGraph)
      right <- normalizedRows.lift(rightGraph)
    yield provenance.contributions(left, right)

private[grakern] object WLCombinedFeatures:
  def fromTrace[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite],
      subtreeWeight: NonNegativeFinite,
      optimalAssignmentWeight: NonNegativeFinite,
      diagonalTolerance: NonNegativeFinite
  ): Either[
    WLCombinedFeatureError,
    WLCombinedFeatures[V, NodeKey, EdgeKey]
  ] =
    for
      subtree <- WLSubtreeFeatures
        .fromTrace(trace, roundWeights)
        .left
        .map(WLCombinedFeatureError.Subtree(_))
      observed <- WLObservedHierarchy
        .fromTrace(trace)
        .left
        .map(WLCombinedFeatureError.Hierarchy(_))
      assignment =
        WLOAThresholdFeatures.fromObserved(observed)
      rawRows =
        combineRows(
          subtree,
          assignment,
          subtreeWeight,
          optimalAssignmentWeight
        )
      normalized <- normalize(
        trace.sampleKeys,
        rawRows,
        diagonalTolerance
      )
      provenance =
        WLFeatureProvenanceIndex.fromTrace(
          trace,
          subtree,
          rawRows
        )
    yield new WLCombinedFeatures(
      trace.refinementNodeId,
      trace.sampleKeys,
      rawRows,
      normalized._1,
      subtree,
      assignment,
      provenance,
      normalized._2
    )

  private def combineRows[V, NodeKey, EdgeKey](
      subtree: WLSubtreeFeatures[V],
      assignment: WLOAThresholdFeatures[NodeKey, EdgeKey],
      subtreeWeight: NonNegativeFinite,
      optimalAssignmentWeight: NonNegativeFinite
  ): Vector[ExactFeatureRow[WLCombinedFeature]] =
    val subtreeScale = math.sqrt(subtreeWeight.toDouble)
    val assignmentScale =
      math.sqrt(optimalAssignmentWeight.toDouble)
    subtree.rows.zip(assignment.rows).map { case (subtreeRow, assignmentRow) =>
      val left =
        if subtreeScale == 0.0 then ExactFeatureRow(Vector.empty)
        else
          subtreeRow
            .scaled(subtreeScale)
            .mapKeys(WLCombinedFeature.Subtree(_))
      val right =
        if assignmentScale == 0.0 then ExactFeatureRow(Vector.empty)
        else
          assignmentRow
            .scaled(assignmentScale)
            .mapKeys(WLCombinedFeature.OptimalAssignment(_))
      left ++ right
    }

  private def normalize(
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[WLCombinedFeature]],
      tolerance: NonNegativeFinite
  ): Either[
    WLCombinedFeatureError,
    (
        Vector[ExactFeatureRow[WLCombinedFeature]],
        Vector[NumericalAuditEvent]
    )
  ] =
    val normalized =
      Vector.newBuilder[ExactFeatureRow[WLCombinedFeature]]
    val events = Vector.newBuilder[NumericalAuditEvent]
    var failure: Option[WLCombinedFeatureError] = None
    var index = 0

    while index < rows.size && failure.isEmpty do
      val diagonal = rows(index).squaredNorm
      if !diagonal.isFinite then
        failure = Some(
          WLCombinedFeatureError.NonFiniteDiagonal(
            sampleKeys(index),
            index,
            diagonal
          )
        )
      else if diagonal < -tolerance.toDouble then
        failure = Some(
          WLCombinedFeatureError.NegativeDiagonal(
            sampleKeys(index),
            index,
            diagonal,
            tolerance
          )
        )
      else if math.abs(diagonal) <= tolerance.toDouble then
        normalized += ExactFeatureRow(Vector.empty)
        events += NumericalAuditEvent.DiagonalTreatedAsZero(
          sampleKeys(index),
          index,
          diagonal,
          tolerance
        )
      else normalized += rows(index).scaled(1.0 / math.sqrt(diagonal))
      index += 1

    failure.toLeft(normalized.result() -> events.result())

  private[engine] def sameFeature(
      left: WLCombinedFeature,
      right: WLCombinedFeature
  ): Boolean =
    (left, right) match
      case (
            WLCombinedFeature.Subtree(first),
            WLCombinedFeature.Subtree(second)
          ) =>
        WLSubtreeFeatures.sameFeature(first, second)
      case (
            WLCombinedFeature.OptimalAssignment(first),
            WLCombinedFeature.OptimalAssignment(second)
          ) =>
        WLOAThresholdFeatures.sameFeature(first, second)
      case _ =>
        false
