package grakern.core

import cats.data.NonEmptyVector
import cats.kernel.{Eq, Hash}

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
    private val itemLeaves: Vector[(A, Node)]
)(using nodeHash: Hash[Node], itemEq: Eq[A]):
  def parentOf(node: Node): Option[Node] =
    association(node, parents).flatten

  def heightOf(node: Node): Option[NonNegativeFinite] =
    association(node, heights)

  def leafOf(item: A): Option[Node] =
    itemLeaves.collectFirst {
      case (candidate, node) if itemEq.eqv(candidate, item) => node
    }

  def leastCommonAncestor(left: Node, right: Node): Option[Node] =
    if !containsNode(left) || !containsNode(right) then None
    else
      val leftAncestors = ancestors(left)
      ancestors(right).find(candidate => leftAncestors.exists(nodeHash.eqv(_, candidate)))

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
      leafOf(item).foreach { leaf =>
        ancestors(leaf).foreach { node =>
          indexOfNode(node).foreach(index => result(index) += 1)
        }
      }
    }
    result

  private def incrementalHeight(nodeIndex: Int): Double =
    val current = heights(nodeIndex)._2.toDouble
    parents(nodeIndex)._2 match
      case Some(parent) =>
        current - heightOf(parent).fold(0.0)(_.toDouble)
      case None =>
        current

  private def ancestors(node: Node): Vector[Node] =
    val result = Vector.newBuilder[Node]
    var current: Option[Node] = Some(node)
    while current.nonEmpty do
      val value = current.get
      result += value
      current = parentOf(value)
    result.result()

  private def containsNode(node: Node): Boolean =
    indexOfNode(node).nonEmpty

  private def indexOfNode(node: Node): Option[Int] =
    nodes.toVector.indexWhere(nodeHash.eqv(_, node)) match
      case -1    => None
      case index => Some(index)

  private def association[B](
      key: Node,
      values: Vector[(Node, B)]
  ): Option[B] =
    values.collectFirst {
      case (candidate, value) if nodeHash.eqv(candidate, key) => value
    }

object Hierarchy:
  def build[Node: Hash, A: Eq](
      nodes: NonEmptyVector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)]
  ): Either[HierarchyError[Node, A], Hierarchy[A, Node]] =
    val nodeValues = nodes.toVector
    val nodeHash = Hash[Node]
    val itemEq = Eq[A]
    val duplicateNode =
      nodeValues.zipWithIndex.collectFirst {
        case (node, index)
            if nodeValues
              .take(index)
              .exists(nodeHash.eqv(_, node)) =>
          HierarchyError.DuplicateNode(node)
      }

    duplicateNode match
      case Some(error) => Left(error)
      case None        =>
        validateStructure(nodeValues, root, parent, height, leaves).map { case (parents, heights) =>
          new Hierarchy(
            nodes,
            root,
            parents,
            heights,
            leaves
          )(using nodeHash, itemEq)
        }

  private def validateStructure[Node: Hash, A: Eq](
      nodes: Vector[Node],
      root: Node,
      parent: Node => Option[Node],
      height: Node => NonNegativeFinite,
      leaves: Vector[(A, Node)]
  ): Either[
    HierarchyError[Node, A],
    (Vector[(Node, Option[Node])], Vector[(Node, NonNegativeFinite)])
  ] =
    val nodeHash = Hash[Node]
    val parents = nodes.map(node => node -> parent(node))
    val heights = nodes.map(node => node -> height(node))

    def contains(node: Node): Boolean =
      nodes.exists(nodeHash.eqv(_, node))

    if !contains(root) then Left(HierarchyError.RootNotMember(root))
    else
      parents.collectFirst {
        case (node, Some(foundParent)) if nodeHash.eqv(node, root) =>
          HierarchyError.RootHasParent(root, foundParent)
        case (node, None) if !nodeHash.eqv(node, root) =>
          HierarchyError.AdditionalRoot(node)
        case (node, Some(foundParent)) if !contains(foundParent) =>
          HierarchyError.ParentNotMember(node, foundParent)
      } match
        case Some(error) => Left(error)
        case None        =>
          validateReachability(nodes, root, parents).flatMap { _ =>
            validateHeights(parents, heights).flatMap { _ =>
              validateLeaves(nodes, parents, leaves).map(_ => (parents, heights))
            }
          }

  private def validateReachability[Node: Hash, A](
      nodes: Vector[Node],
      root: Node,
      parents: Vector[(Node, Option[Node])]
  ): Either[HierarchyError[Node, A], Unit] =
    val nodeHash = Hash[Node]

    def parentOf(node: Node): Option[Node] =
      parents.collectFirst {
        case (candidate, value) if nodeHash.eqv(candidate, node) => value
      }.flatten

    nodes.iterator
      .map { start =>
        var current = start
        var path = Vector.empty[Node]
        var finished = false
        var error: Option[HierarchyError[Node, A]] = None
        while !finished && error.isEmpty do
          if path.exists(nodeHash.eqv(_, current)) then
            error = Some(HierarchyError.Cycle(path :+ current))
          else if nodeHash.eqv(current, root) then finished = true
          else
            path = path :+ current
            current = parentOf(current).get
        error
      }
      .collectFirst { case Some(error) => error }
      .toLeft(())

  private def validateHeights[Node: Hash, A](
      parents: Vector[(Node, Option[Node])],
      heights: Vector[(Node, NonNegativeFinite)]
  ): Either[HierarchyError[Node, A], Unit] =
    val nodeHash = Hash[Node]

    def heightOf(node: Node): NonNegativeFinite =
      heights.collectFirst {
        case (candidate, value) if nodeHash.eqv(candidate, node) => value
      }.get

    parents
      .collectFirst {
        case (child, Some(parent)) if heightOf(child).toDouble < heightOf(parent).toDouble =>
          HierarchyError.DecreasingHeight(
            parent,
            child,
            heightOf(parent).toDouble,
            heightOf(child).toDouble
          )
      }
      .toLeft(())

  private def validateLeaves[Node: Hash, A: Eq](
      nodes: Vector[Node],
      parents: Vector[(Node, Option[Node])],
      leaves: Vector[(A, Node)]
  ): Either[HierarchyError[Node, A], Unit] =
    val nodeHash = Hash[Node]
    val itemEq = Eq[A]

    def contains(node: Node): Boolean =
      nodes.exists(nodeHash.eqv(_, node))

    def hasChild(node: Node): Boolean =
      parents.exists {
        case (_, Some(candidate)) => nodeHash.eqv(candidate, node)
        case _                    => false
      }

    leaves.zipWithIndex
      .collectFirst {
        case ((item, node), _) if !contains(node) =>
          HierarchyError.ItemNodeNotMember(item, node)
        case ((item, node), _) if hasChild(node) =>
          HierarchyError.ItemNotAtLeaf(item, node)
        case ((item, _), index) if leaves.take(index).exists { case (candidate, _) =>
              itemEq.eqv(candidate, item)
            } =>
          HierarchyError.DuplicateItem(item)
        case ((_, node), index) if leaves.take(index).exists { case (_, candidate) =>
              nodeHash.eqv(candidate, node)
            } =>
          HierarchyError.DuplicateItemLeaf(node)
      }
      .toLeft(())

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
