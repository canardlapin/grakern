package grakern.engine

import grakern.core.PlanNodeId

final class WLOAIntersection[NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    private val observed: WLObservedHierarchy[NodeKey, EdgeKey]
):
  def evaluate(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- observed.bag(leftGraph)
      right <- observed.bag(rightGraph)
    yield observed.hierarchy.optimalAssignment(left, right)

private[grakern] object WLOAIntersection:
  def apply[NodeKey, EdgeKey](
      observed: WLObservedHierarchy[NodeKey, EdgeKey]
  ): WLOAIntersection[NodeKey, EdgeKey] =
    new WLOAIntersection(
      observed.refinementNodeId,
      observed
    )
