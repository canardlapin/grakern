package grakern.engine

import grakern.standard.wl.PortableSparseFeatureRow

enum WLRequestedOutput derives CanEqual:
  case AllViews
  case Gram
  case Operator
  case Query

enum WLGramPreference derives CanEqual:
  case Auto
  case DenseScratchRows
  case SparseFeaturePostings
  case DenseRows

enum WLGramRealization derives CanEqual:
  case DenseScratchRows
  case SparseFeaturePostings
  case DenseRows

  def description: String =
    this match
      case DenseScratchRows      => "pairwise sparse rows with a dense feature scratch"
      case SparseFeaturePostings => "feature postings accumulated into the exact Gram"
      case DenseRows             => "dense row multiplication in canonical feature order"

enum WLQueryRealization derives CanEqual:
  case FeaturePostings
  case PairwiseSparseRows
  case DenseRows

  def description: String =
    this match
      case FeaturePostings    => "shared-feature postings"
      case PairwiseSparseRows => "pairwise sparse-row intersections"
      case DenseRows          => "dense aligned rows"

enum WLOptimalAssignmentRealization derives CanEqual:
  case ThresholdFeatures
  case HierarchyIntersection

final case class WLMemoryBudget private (bytes: Long)

object WLMemoryBudget:
  def fromBytes(bytes: Long): Option[WLMemoryBudget] =
    Option.when(bytes > 0L)(WLMemoryBudget(bytes))

final case class WLQueryNovelty private (fraction: Double)

object WLQueryNovelty:
  val zero: WLQueryNovelty = WLQueryNovelty(0.0)

  def fromFraction(fraction: Double): Option[WLQueryNovelty] =
    Option.when(fraction.isFinite && fraction >= 0.0 && fraction <= 1.0)(
      WLQueryNovelty(fraction)
    )

final case class WLStrategyEstimate(
    realization: WLGramRealization,
    multiplyAdds: Long,
    workingBytes: Long,
    admitted: Boolean,
    reason: String
)

final case class WLExecutionStrategy(
    gram: WLGramRealization,
    query: WLQueryRealization,
    optimalAssignment: WLOptimalAssignmentRealization,
    requestedOutput: WLRequestedOutput,
    anticipatedQueryNovelty: WLQueryNovelty,
    estimates: Vector[WLStrategyEstimate],
    decision: String
)

private[grakern] object WLExecutionPlanner:
  def select(
      rows: Vector[PortableSparseFeatureRow],
      featureCount: Int,
      graph: WLPlanGraph,
      options: WLPlanOptions
  ): WLExecutionStrategy =
    val sampleCount = rows.size
    val nonZeros = rows.iterator.map(_.activeSize.toLong).sum
    val slots = sampleCount.toLong * featureCount.toLong
    val density = if slots == 0L then 0.0 else nonZeros.toDouble / slots.toDouble
    val gramBytes = saturatedProduct(sampleCount.toLong, sampleCount.toLong, 8L)
    val denseRowsBytes = saturatedProduct(sampleCount.toLong, featureCount.toLong, 8L)
    val scratchBytes = saturatedProduct(featureCount.toLong, 8L)
    val postingsBytes =
      saturatedAdd(
        saturatedProduct(nonZeros, 12L),
        saturatedProduct(featureCount.toLong + 1L, 4L)
      )
    val pairwiseWork =
      rows.iterator.zipWithIndex.foldLeft(0L) { case (total, (row, left)) =>
        saturatedAdd(total, saturatedProduct(row.activeSize.toLong, sampleCount - left))
      }
    val columnCounts = Array.fill(featureCount)(0L)
    rows.foreach { row =>
      row.indices.foreach(column => columnCounts(column) += 1L)
    }
    val postingsWork =
      columnCounts.foldLeft(0L) { (total, count) =>
        saturatedAdd(total, saturatedProduct(count, count + 1L) / 2L)
      }
    val denseWork =
      saturatedProduct(
        sampleCount.toLong * (sampleCount.toLong + 1L) / 2L,
        featureCount.toLong
      )
    val budget = options.memoryBudget.map(_.bytes)
    val executionAllowsSequentialPlanner = options.gramExecution == GramExecution.Sequential

    def admitted(workingBytes: Long): Boolean =
      budget.forall(limit => saturatedAdd(workingBytes, gramBytes) <= limit)

    val estimates =
      Vector(
        WLStrategyEstimate(
          WLGramRealization.DenseScratchRows,
          pairwiseWork,
          scratchBytes,
          admitted(scratchBytes),
          "canonical exact sparse-row products"
        ),
        WLStrategyEstimate(
          WLGramRealization.SparseFeaturePostings,
          postingsWork,
          postingsBytes,
          executionAllowsSequentialPlanner && admitted(postingsBytes),
          if executionAllowsSequentialPlanner then "exact column-order posting accumulation"
          else "parallel schedule requires deterministic row tiles"
        ),
        WLStrategyEstimate(
          WLGramRealization.DenseRows,
          denseWork,
          denseRowsBytes,
          executionAllowsSequentialPlanner && density >= 0.2 && admitted(denseRowsBytes),
          if density < 0.2 then "rejected below density 0.2"
          else if executionAllowsSequentialPlanner then "canonical dense feature-order products"
          else "parallel schedule requires deterministic row tiles"
        )
      )

    val forced =
      options.gramPreference match
        case WLGramPreference.Auto                  => None
        case WLGramPreference.DenseScratchRows      => Some(WLGramRealization.DenseScratchRows)
        case WLGramPreference.SparseFeaturePostings => Some(WLGramRealization.SparseFeaturePostings)
        case WLGramPreference.DenseRows             => Some(WLGramRealization.DenseRows)
    val admittedEstimates = estimates.filter(_.admitted)
    val selected =
      forced
        .flatMap(value => admittedEstimates.find(_.realization == value))
        .orElse(
          admittedEstimates.minByOption(estimate => (estimate.multiplyAdds, estimate.workingBytes))
        )
        .getOrElse(estimates.minBy(estimate => (estimate.workingBytes, estimate.multiplyAdds)))
    val query =
      if density >= 0.2 && admitted(denseRowsBytes) then WLQueryRealization.DenseRows
      else if options.anticipatedQueryNovelty.fraction >= 0.5 then
        WLQueryRealization.PairwiseSparseRows
      else WLQueryRealization.FeaturePostings
    val optimalAssignment = WLOptimalAssignmentRealization.ThresholdFeatures
    val forcedRejected = forced.exists(_ != selected.realization)
    val gramDecision =
      if forcedRejected then
        s"requested ${forced.get} was inadmissible; deterministic fallback selected ${selected.realization}"
      else if admittedEstimates.isEmpty then
        s"memory budget admitted no candidate; smallest deterministic working set selected ${selected.realization}"
      else s"minimum admitted multiply-add estimate selected ${selected.realization}"
    val optimalAssignmentDecision =
      if graph.count(WLPlanNodeKind.OptimalAssignmentThresholdFeatures) > 0 then
        "threshold features selected over hierarchy intersection because the compiled contract requests exact features and query alignment"
      else "optimal-assignment realization not applicable"
    val decision = s"$gramDecision; $optimalAssignmentDecision"

    WLExecutionStrategy(
      selected.realization,
      query,
      optimalAssignment,
      options.requestedOutput,
      options.anticipatedQueryNovelty,
      estimates,
      decision
    )

  private def saturatedAdd(left: Long, right: Long): Long =
    if left > Long.MaxValue - right then Long.MaxValue else left + right

  private def saturatedProduct(values: Long*): Long =
    values.foldLeft(1L) { (result, value) =>
      if result == 0L || value == 0L then 0L
      else if result > Long.MaxValue / value then Long.MaxValue
      else result * value
    }
