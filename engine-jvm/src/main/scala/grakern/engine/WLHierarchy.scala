package grakern.engine

import cats.data.NonEmptyVector
import cats.kernel.Hash
import grakern.core.*
import grakern.standard.wl.*

enum WLHierarchyNode derives CanEqual:
  case Root
  case Signature(colour: WLColour)

final class WLObservedHierarchy[NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    val hierarchy: Hierarchy[WLColour, WLHierarchyNode],
    val graphBags: Vector[FiniteBag[WLColour]]
):
  def bag(graphIndex: Int): Option[FiniteBag[WLColour]] =
    graphBags.lift(graphIndex)

  def kernel: StrongKernel[WLColour] =
    hierarchy.kernel

private[grakern] object WLObservedHierarchy:
  private given Hash[WLColour] with
    def eqv(left: WLColour, right: WLColour): Boolean =
      left.equals(right)

    def hash(value: WLColour): Int =
      value.hashCode()

  private given Hash[WLHierarchyNode] with
    def eqv(
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

    def hash(value: WLHierarchyNode): Int =
      value match
        case WLHierarchyNode.Root =>
          0x51a7
        case WLHierarchyNode.Signature(colour) =>
          31 * colour.hashCode() + 1

  def fromTrace[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey]
  ): Either[
    HierarchyError[WLHierarchyNode, WLColour],
    WLObservedHierarchy[NodeKey, EdgeKey]
  ] =
    val root = WLHierarchyNode.Root
    val signatureNodes =
      trace.interner.colours.map(WLHierarchyNode.Signature(_))
    val nodes = NonEmptyVector(root, signatureNodes)

    def parent(node: WLHierarchyNode): Option[WLHierarchyNode] =
      node match
        case WLHierarchyNode.Root =>
          None
        case WLHierarchyNode.Signature(colour) =>
          trace.signature(colour).flatMap(_.parent) match
            case Some(previous) =>
              Some(WLHierarchyNode.Signature(previous))
            case None =>
              Some(root)

    def height(node: WLHierarchyNode): NonNegativeFinite =
      node match
        case WLHierarchyNode.Root =>
          NonNegativeFinite.zero
        case WLHierarchyNode.Signature(colour) =>
          NonNegativeFinite
            .from(colour.level.toDouble + 1.0)
            .toOption
            .get

    val graphBags =
      Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
        FiniteBag(
          trace
            .graphColours(graphIndex, trace.rounds)
            .getOrElse(Vector.empty)
        )
      }
    val finalColours =
      graphBags.iterator
        .flatMap(_.items)
        .foldLeft(Vector.empty[WLColour]) { (current, colour) =>
          if current.exists(_.equals(colour)) then current
          else current :+ colour
        }
    val leaves =
      finalColours.map(colour => colour -> WLHierarchyNode.Signature(colour))

    Hierarchy
      .build(
        nodes,
        root,
        parent,
        height,
        leaves
      )
      .map(
        new WLObservedHierarchy(
          trace.refinementNodeId,
          _,
          graphBags
        )
      )
