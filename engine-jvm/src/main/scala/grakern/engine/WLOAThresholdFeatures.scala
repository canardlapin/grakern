package grakern.engine

import grakern.core.*

final class WLOAThresholdFeatures[NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    private[grakern] val rows: Vector[
      ExactFeatureRow[HierarchyFeature[WLHierarchyNode]]
    ]
):
  def graphCount: Int = rows.size

  def activeCoordinates(graphIndex: Int): Option[Int] =
    rows.lift(graphIndex).map(_.activeSize)

  def squaredNorm(graphIndex: Int): Option[Double] =
    rows.lift(graphIndex).map(_.squaredNorm)

  def kernel(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.dot(right)(WLOAThresholdFeatures.sameFeature)

private[grakern] object WLOAThresholdFeatures:
  def fromObserved[NodeKey, EdgeKey](
      observed: WLObservedHierarchy[NodeKey, EdgeKey]
  ): WLOAThresholdFeatures[NodeKey, EdgeKey] =
    val rows =
      observed.graphBags.map { bag =>
        ExactFeatureRow(
          observed.hierarchy.thresholdFeatures(bag)
        )
      }
    new WLOAThresholdFeatures(
      observed.refinementNodeId,
      rows
    )

  private[engine] def sameFeature(
      left: HierarchyFeature[WLHierarchyNode],
      right: HierarchyFeature[WLHierarchyNode]
  ): Boolean =
    left.multiplicityThreshold == right.multiplicityThreshold &&
      sameNode(left.node, right.node)

  private[engine] def sameNode(
      left: WLHierarchyNode,
      right: WLHierarchyNode
  ): Boolean =
    (left, right) match
      case (WLHierarchyNode.Root, WLHierarchyNode.Root) =>
        true
      case (
            WLHierarchyNode.Signature(first),
            WLHierarchyNode.Signature(second)
          ) =>
        first.equals(second)
      case _ =>
        false
