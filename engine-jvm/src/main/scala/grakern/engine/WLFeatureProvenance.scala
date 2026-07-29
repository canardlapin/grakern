package grakern.engine

import grakern.standard.wl.*

enum WLFeatureComponent derives CanEqual:
  case Subtree
  case OptimalAssignment

final case class WLFeatureProvenance[V, NodeKey, EdgeKey](
    feature: WLCombinedFeature,
    component: WLFeatureComponent,
    round: Option[Int],
    exactSignature: Option[WLSignature[NodeKey, EdgeKey]],
    hierarchyNode: Option[WLHierarchyNode],
    multiplicityThreshold: Option[Int],
    representatives: Vector[WLSource[V]]
)

final case class WLFeatureContribution[V, NodeKey, EdgeKey](
    provenance: WLFeatureProvenance[V, NodeKey, EdgeKey],
    value: Double
)

final class WLFeatureProvenanceIndex[V, NodeKey, EdgeKey] private[engine] (
    val entries: Vector[
      WLFeatureProvenance[V, NodeKey, EdgeKey]
    ]
):
  def find(
      feature: WLCombinedFeature
  ): Option[WLFeatureProvenance[V, NodeKey, EdgeKey]] =
    entries.find(value => WLCombinedFeatures.sameFeature(value.feature, feature))

  private[engine] def contributions(
      left: ExactFeatureRow[WLCombinedFeature],
      right: ExactFeatureRow[WLCombinedFeature]
  ): Vector[WLFeatureContribution[V, NodeKey, EdgeKey]] =
    left.entries.flatMap { case (feature, leftValue) =>
      right.entries
        .collectFirst {
          case (candidate, rightValue)
              if WLCombinedFeatures.sameFeature(
                feature,
                candidate
              ) =>
            rightValue
        }
        .flatMap { rightValue =>
          val value = leftValue * rightValue
          if value == 0.0 then None
          else
            find(feature).map(
              WLFeatureContribution(_, value)
            )
        }
    }

private[grakern] object WLFeatureProvenanceIndex:
  def fromTrace[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey],
      subtree: WLSubtreeFeatures[V],
      rows: Vector[ExactFeatureRow[WLCombinedFeature]]
  ): WLFeatureProvenanceIndex[V, NodeKey, EdgeKey] =
    val features =
      rows.iterator
        .flatMap(_.entries.iterator.map(_._1))
        .foldLeft(Vector.empty[WLCombinedFeature]) { (current, feature) =>
          if current.exists(
              WLCombinedFeatures.sameFeature(_, feature)
            )
          then current
          else current :+ feature
        }
    val provenance =
      features.map {
        case value @ WLCombinedFeature.Subtree(feature) =>
          val representatives =
            subtree.provenance
              .find(item =>
                WLSubtreeFeatures.sameFeature(
                  item.feature,
                  feature
                )
              )
              .fold(Vector.empty[WLSource[V]])(
                _.occurrences
              )
          WLFeatureProvenance(
            value,
            WLFeatureComponent.Subtree,
            Some(feature.round),
            trace.signature(feature.signature),
            None,
            None,
            representatives
          )
        case value @ WLCombinedFeature.OptimalAssignment(feature) =>
          val signature =
            feature.node match
              case WLHierarchyNode.Root =>
                None
              case WLHierarchyNode.Signature(colour) =>
                trace.signature(colour)
          val round =
            feature.node match
              case WLHierarchyNode.Root =>
                None
              case WLHierarchyNode.Signature(colour) =>
                Some(colour.level)
          WLFeatureProvenance(
            value,
            WLFeatureComponent.OptimalAssignment,
            round,
            signature,
            Some(feature.node),
            Some(feature.multiplicityThreshold),
            assignmentRepresentatives(
              trace,
              feature.node,
              feature.multiplicityThreshold
            )
          )
      }
    new WLFeatureProvenanceIndex(provenance)

  private def assignmentRepresentatives[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey],
      node: WLHierarchyNode,
      threshold: Int
  ): Vector[WLSource[V]] =
    trace.graphRanges.flatMap { range =>
      val supporting =
        range.iterator.flatMap { vertexIndex =>
          val finalColour =
            trace.colour(trace.rounds, vertexIndex).get
          if isUnder(trace, finalColour, node) then trace.source(vertexIndex)
          else None
        }.toVector
      if supporting.size >= threshold then supporting
      else Vector.empty
    }

  private def isUnder[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey],
      leaf: WLColour,
      node: WLHierarchyNode
  ): Boolean =
    node match
      case WLHierarchyNode.Root =>
        true
      case WLHierarchyNode.Signature(expected) =>
        var current = Option(leaf)
        var found = false
        while current.nonEmpty && !found do
          val colour = current.get
          if colour.equals(expected) then found = true
          else current = trace.signature(colour).flatMap(_.parent)
        found
