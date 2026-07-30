package grakern.core

import cats.data.NonEmptyVector
import cats.kernel.{Eq, Hash}
import scala.collection.immutable.IntMap

enum HierarchyError[+Node, +A]:
  case DuplicateNode(node: Node)
  case RootNotMember(root: Node)
  case RootHasParent(root: Node, parent: Node)
  case AdditionalRoot(node: Node)
  case ParentNotMember(node: Node, parent: Node)
  case Cycle(path: Vector[Node])
  case DecreasingHeight(
      parent: Node,
      child: Node,
      parentHeight: Double,
      childHeight: Double
  )
  case ItemNotAtLeaf(item: A, node: Node)
  case ItemNodeNotMember(item: A, node: Node)
  case DuplicateItem(item: A)
  case DuplicateItemLeaf(node: Node)

final case class HierarchyFeature[+Node](
    node: Node,
    multiplicityThreshold: Int
)

enum StrongKernelRealization:
  case HierarchyIntersection

/** A validated finite hierarchy realizing a strong kernel on its associated items.
  *
  * Items absent from `leaves` denote the external zero object. Their self-kernel and similarities
  * are zero. This keeps the semantic kernel total while allowing the hierarchy to describe a finite
  * observed domain.
  */
final class Hierarchy[A, Node] private (
    val nodes: NonEmptyVector[Node],
    val root: Node,
    private val parents: Vector[(Node, Option[Node])],
    private val heights: Vector[(Node, NonNegativeFinite)],
    private val itemLeaves: Vector[(A, Node)],
    private val itemIndex: Option[Hierarchy.ItemIndex[A]]
)(using nodeHash: Hash[Node], itemEq: Eq[A]):
  private val nodeValues = nodes.toVector

  private val nodeIndex =
    Hierarchy.NodeIndex(nodeValues)

  private val parentIndices =
    parents.map { case (_, parent) =>
      parent.flatMap(indexOfNode).getOrElse(-1)
    }.toArray

  private val heightValues =
    heights.map(_._2.toDouble).toArray

  private val incrementalHeights =
    Array.tabulate(nodeValues.size) { index =>
      val parent = parentIndices(index)
      if parent < 0 then heightValues(index)
      else heightValues(index) - heightValues(parent)
    }

  private val ancestorIndices =
    Array.tabulate(nodeValues.size) { start =>
      val result = Vector.newBuilder[Int]
      var current = start
      while current >= 0 do
        result += current
        current = parentIndices(current)
      result.result()
    }

  def parentOf(node: Node): Option[Node] =
    indexOfNode(node).flatMap { index =>
      val parent = parentIndices(index)
      if parent < 0 then None else Some(nodeValues(parent))
    }

  def heightOf(node: Node): Option[NonNegativeFinite] =
    indexOfNode(node).map(index => heights(index)._2)

  def leafOf(item: A): Option[Node] =
    itemIndex match
      case Some(index) =>
        index.indexOf(item).map(itemLeaves(_)._2)
      case None =>
        itemLeaves.collectFirst {
          case (candidate, node) if itemEq.eqv(candidate, item) => node
        }

  def leastCommonAncestor(left: Node, right: Node): Option[Node] =
    for
      leftIndex <- indexOfNode(left)
      rightIndex <- indexOfNode(right)
      common <- ancestorIndices(rightIndex).find(
        ancestorIndices(leftIndex).contains
      )
    yield nodeValues(common)

  def evaluate(left: A, right: A): Double =
    val result =
      for
        leftLeaf <- leafOf(left)
        rightLeaf <- leafOf(right)
        common <- leastCommonAncestor(leftLeaf, rightLeaf)
        height <- heightOf(common)
      yield height.toDouble
    result.getOrElse(0.0)

  def optimalAssignment(
      left: FiniteBag[A],
      right: FiniteBag[A]
  ): Double =
    val leftCounts = subtreeCounts(left)
    val rightCounts = subtreeCounts(right)
    var result = 0.0
    var index = 0
    while index < nodes.length do
      val increment = incrementalHeight(index)
      result +=
        increment *
          math.min(leftCounts(index), rightCounts(index)).toDouble
      index += 1
    result

  def thresholdFeatures(
      bag: FiniteBag[A]
  ): Vector[(HierarchyFeature[Node], Double)] =
    val counts = subtreeCounts(bag)
    val result = Vector.newBuilder[(HierarchyFeature[Node], Double)]
    var nodeIndex = 0
    while nodeIndex < nodes.length do
      val increment = incrementalHeight(nodeIndex)
      if increment > 0.0 then
        val value = math.sqrt(increment)
        var threshold = 1
        while threshold <= counts(nodeIndex) do
          result += ((
            HierarchyFeature(nodes.toVector(nodeIndex), threshold),
            value
          ))
          threshold += 1
      nodeIndex += 1
    result.result()

  def kernel: StrongKernel[A] =
    val assignment =
      BagAssignmentEvaluator.hierarchy(this)
    StrongKernel.fromExpression(
      grakern.core.internal.KernelExpr.HierarchyBase(
        PlanNodeId.fresh(),
        evaluate
      ),
      Some(assignment)
    )

  private def subtreeCounts(bag: FiniteBag[A]): Array[Int] =
    val result = Array.fill(nodes.length)(0)
    bag.items.foreach { item =>
      leafOf(item).flatMap(indexOfNode).foreach { leaf =>
        ancestorIndices(leaf).foreach { index =>
          result(index) += 1
        }
      }
    }
    result

  private def incrementalHeight(nodeIndex: Int): Double =
    incrementalHeights(nodeIndex)

  private def indexOfNode(node: Node): Option[Int] =
    nodeIndex.indexOf(node)

object Hierarchy:
  def build[Node: Hash, A: Eq](
      nodes: NonEmptyVector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)]
  ): Either[HierarchyError[Node, A], Hierarchy[A, Node]] =
    buildIndexed(
      nodes,
      root,
      parent,
      height,
      leaves,
      None
    )

  private[grakern] def buildHashed[
      Node: Hash,
      A: Hash
  ](
      nodes: NonEmptyVector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)]
  ): Either[HierarchyError[Node, A], Hierarchy[A, Node]] =
    buildIndexed(
      nodes,
      root,
      parent,
      height,
      leaves,
      Some(Hash[A])
    )

  private def buildIndexed[Node: Hash, A: Eq](
      nodes: NonEmptyVector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)],
      itemHash: Option[Hash[A]]
  ): Either[HierarchyError[Node, A], Hierarchy[A, Node]] =
    val nodeValues = nodes.toVector
    val nodeHash = Hash[Node]
    val itemEq = Eq[A]
    val nodeIndex = NodeIndex(nodeValues)
    val itemIndex =
      itemHash.map(
        ItemIndex(
          leaves.map(_._1),
          _
        )
      )
    val duplicateNode =
      nodeIndex.firstDuplicate.map(
        HierarchyError.DuplicateNode(_)
      )

    duplicateNode match
      case Some(error) => Left(error)
      case None        =>
        validateStructure(
          nodeValues,
          root,
          parent,
          height,
          leaves,
          nodeIndex,
          itemIndex
        ).map { case (parents, heights) =>
          new Hierarchy(
            nodes,
            root,
            parents,
            heights,
            leaves,
            itemIndex
          )(using nodeHash, itemEq)
        }

  private def validateStructure[Node: Hash, A: Eq](
      nodes: Vector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)],
      nodeIndex: NodeIndex[Node],
      itemIndex: Option[ItemIndex[A]]
  ): Either[
    HierarchyError[Node, A],
    (Vector[(Node, Option[Node])], Vector[(Node, NonNegativeFinite)])
  ] =
    val parents = nodes.map(node => node -> parent(node))
    val heights = nodes.map(node => node -> height(node))

    if !nodeIndex.contains(root) then Left(HierarchyError.RootNotMember(root))
    else
      parents.collectFirst {
        case (node, Some(foundParent)) if Hash[Node].eqv(node, root) =>
          HierarchyError.RootHasParent(root, foundParent)
        case (node, None) if !Hash[Node].eqv(node, root) =>
          HierarchyError.AdditionalRoot(node)
        case (node, Some(foundParent)) if !nodeIndex.contains(foundParent) =>
          HierarchyError.ParentNotMember(node, foundParent)
      } match
        case Some(error) => Left(error)
        case None        =>
          val parentIndices =
            parents.map { case (_, parent) =>
              parent.flatMap(nodeIndex.indexOf).getOrElse(-1)
            }.toArray
          validateReachability(
            nodes,
            nodeIndex.indexOf(root).get,
            parentIndices
          ).flatMap { _ =>
            validateHeights(
              nodes,
              parentIndices,
              heights
            ).flatMap { _ =>
              validateLeaves(
                nodes,
                parentIndices,
                leaves,
                nodeIndex,
                itemIndex
              ).map(_ => (parents, heights))
            }
          }

  private def validateReachability[Node, A](
      nodes: Vector[Node],
      rootIndex: Int,
      parentIndices: Array[Int]
  ): Either[HierarchyError[Node, A], Unit] =
    val state = Array.fill[Byte](nodes.size)(0)
    state(rootIndex) = 2
    var start = 0
    var failure: Option[HierarchyError[Node, A]] = None
    while start < nodes.size && failure.isEmpty do
      if state(start) == 0 then
        val path = Vector.newBuilder[Int]
        var current = start
        while current >= 0 && state(current) == 0 do
          state(current) = 1
          path += current
          current = parentIndices(current)
        val visited = path.result()
        if current >= 0 && state(current) == 1 then
          val cycleStart = visited.indexOf(current)
          val cycle =
            visited.drop(cycleStart).map(nodes) :+
              nodes(current)
          failure = Some(HierarchyError.Cycle(cycle))
        visited.foreach(index => state(index) = 2)
      start += 1
    failure.toLeft(())

  private def validateHeights[Node, A](
      nodes: Vector[Node],
      parentIndices: Array[Int],
      heights: Vector[(Node, NonNegativeFinite)]
  ): Either[HierarchyError[Node, A], Unit] =
    nodes.indices
      .collectFirst {
        case childIndex
            if parentIndices(childIndex) >= 0 &&
              heights(childIndex)._2.toDouble <
              heights(parentIndices(childIndex))._2.toDouble =>
          val parentIndex = parentIndices(childIndex)
          HierarchyError.DecreasingHeight(
            nodes(parentIndex),
            nodes(childIndex),
            heights(parentIndex)._2.toDouble,
            heights(childIndex)._2.toDouble
          )
      }
      .toLeft(())

  private def validateLeaves[Node, A: Eq](
      nodes: Vector[Node],
      parentIndices: Array[Int],
      leaves: Vector[(A, Node)],
      nodeIndex: NodeIndex[Node],
      itemIndex: Option[ItemIndex[A]]
  ): Either[HierarchyError[Node, A], Unit] =
    val itemEq = Eq[A]
    val hasChild = Array.fill(nodes.size)(false)
    parentIndices.foreach { parent =>
      if parent >= 0 then hasChild(parent) = true
    }
    val seenLeafNodes =
      scala.collection.mutable.BitSet.empty
    var index = 0
    var failure: Option[HierarchyError[Node, A]] = None
    while index < leaves.size && failure.isEmpty do
      val (item, node) = leaves(index)
      nodeIndex.indexOf(node) match
        case None =>
          failure = Some(
            HierarchyError.ItemNodeNotMember(item, node)
          )
        case Some(foundNode) if hasChild(foundNode) =>
          failure = Some(
            HierarchyError.ItemNotAtLeaf(item, node)
          )
        case Some(foundNode) =>
          val duplicateItem =
            itemIndex match
              case Some(items) =>
                items.hasEarlier(item, index)
              case None =>
                leaves
                  .take(index)
                  .exists { case (candidate, _) =>
                    itemEq.eqv(candidate, item)
                  }
          if duplicateItem then
            failure = Some(
              HierarchyError.DuplicateItem(item)
            )
          else if seenLeafNodes.contains(foundNode) then
            failure = Some(
              HierarchyError.DuplicateItemLeaf(node)
            )
          else seenLeafNodes += foundNode
      index += 1
    failure.toLeft(())

  final private class NodeIndex[Node] private (
      nodes: Vector[Node],
      buckets: IntMap[Vector[Int]]
  )(using nodeHash: Hash[Node]):
    def indexOf(node: Node): Option[Int] =
      buckets
        .get(nodeHash.hash(node))
        .flatMap(
          _.find(index => nodeHash.eqv(nodes(index), node))
        )

    def contains(node: Node): Boolean =
      indexOf(node).nonEmpty

    def firstDuplicate: Option[Node] =
      nodes.zipWithIndex.collectFirst {
        case (node, index)
            if buckets
              .getOrElse(nodeHash.hash(node), Vector.empty)
              .exists(candidate =>
                candidate < index &&
                  nodeHash.eqv(nodes(candidate), node)
              ) =>
          node
      }

  private object NodeIndex:
    def apply[Node: Hash](
        nodes: Vector[Node]
    ): NodeIndex[Node] =
      val nodeHash = Hash[Node]
      val buckets =
        nodes.zipWithIndex.foldLeft(
          IntMap.empty[Vector[Int]]
        ) { case (current, (node, index)) =>
          val hash = nodeHash.hash(node)
          current.updated(
            hash,
            current.getOrElse(hash, Vector.empty) :+ index
          )
        }
      new NodeIndex(nodes, buckets)

  final private class ItemIndex[A] private (
      items: Vector[A],
      buckets: IntMap[Vector[Int]]
  )(using itemHash: Hash[A]):
    def indexOf(item: A): Option[Int] =
      buckets
        .get(itemHash.hash(item))
        .flatMap(
          _.find(index => itemHash.eqv(items(index), item))
        )

    def hasEarlier(item: A, index: Int): Boolean =
      buckets
        .getOrElse(itemHash.hash(item), Vector.empty)
        .exists(candidate =>
          candidate < index &&
            itemHash.eqv(items(candidate), item)
        )

  private object ItemIndex:
    def apply[A](
        items: Vector[A],
        itemHash: Hash[A]
    ): ItemIndex[A] =
      val buckets =
        items.zipWithIndex.foldLeft(
          IntMap.empty[Vector[Int]]
        ) { case (current, (item, index)) =>
          val hash = itemHash.hash(item)
          current.updated(
            hash,
            current.getOrElse(hash, Vector.empty) :+ index
          )
        }
      new ItemIndex(items, buckets)(using itemHash)

private[grakern] trait BagAssignmentEvaluator[A]:
  def metadata: StrongKernelRealization

  def evaluate(left: FiniteBag[A], right: FiniteBag[A]): Double

  final def on[B](projection: Projection[B, A]): BagAssignmentEvaluator[B] =
    val self = this
    new BagAssignmentEvaluator[B]:
      val metadata: StrongKernelRealization = self.metadata

      def evaluate(left: FiniteBag[B], right: FiniteBag[B]): Double =
        self.evaluate(
          left.map(item => projection(item)),
          right.map(item => projection(item))
        )

  final def scaled(
      weight: NonNegativeFinite
  ): BagAssignmentEvaluator[A] =
    val self = this
    new BagAssignmentEvaluator[A]:
      val metadata: StrongKernelRealization = self.metadata

      def evaluate(left: FiniteBag[A], right: FiniteBag[A]): Double =
        weight.toDouble * self.evaluate(left, right)

private[grakern] object BagAssignmentEvaluator:
  def hierarchy[A, Node](
      hierarchy: Hierarchy[A, Node]
  ): BagAssignmentEvaluator[A] =
    new BagAssignmentEvaluator[A]:
      val metadata: StrongKernelRealization =
        StrongKernelRealization.HierarchyIntersection

      def evaluate(left: FiniteBag[A], right: FiniteBag[A]): Double =
        hierarchy.optimalAssignment(left, right)
