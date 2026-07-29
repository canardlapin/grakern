package grakern.engine

import grakern.core.*

final class WLOptimalAssignmentOracle[NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    private val observed: WLObservedHierarchy[NodeKey, EdgeKey]
):
  def evaluate(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- observed.bag(leftGraph)
      right <- observed.bag(rightGraph)
    yield OptimalAssignment.reference(
      observed.kernel,
      left,
      right
    )

private[grakern] object WLOptimalAssignmentOracle:
  def apply[NodeKey, EdgeKey](
      observed: WLObservedHierarchy[NodeKey, EdgeKey]
  ): WLOptimalAssignmentOracle[NodeKey, EdgeKey] =
    new WLOptimalAssignmentOracle(
      observed.refinementNodeId,
      observed
    )
