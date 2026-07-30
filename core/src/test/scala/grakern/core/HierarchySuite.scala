package grakern.core

import cats.data.NonEmptyVector
import cats.kernel.{Eq, Hash}
import munit.FunSuite

final class HierarchySuite extends FunSuite:
  enum Node derives CanEqual:
    case Root, Warm, Cool, C, N, O

  given Hash[Node] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private val height: Node => NonNegativeFinite =
    node =>
      NonNegativeFinite
        .from(
          node match
            case Node.Root       => 0.5
            case Node.Warm       => 1.5
            case Node.Cool       => 1.0
            case Node.C | Node.N => 3.0
            case Node.O          => 2.0
        )
        .toOption
        .get

  private val parent: Node => Option[Node] =
    node =>
      node match
        case Node.Root             => None
        case Node.Warm | Node.Cool => Some(Node.Root)
        case Node.C | Node.N       => Some(Node.Warm)
        case Node.O                => Some(Node.Cool)

  private def hierarchy: Hierarchy[String, Node] =
    Hierarchy
      .build(
        NonEmptyVector.of(
          Node.Root,
          Node.Warm,
          Node.Cool,
          Node.C,
          Node.N,
          Node.O
        ),
        Node.Root,
        parent,
        height,
        Vector("C" -> Node.C, "N" -> Node.N, "O" -> Node.O)
      )
      .toOption
      .get

  test("positive root height contributes to real cross-branch matches"):
    val value = hierarchy

    assertEquals(value.evaluate("C", "N"), 1.5)
    assertEquals(value.evaluate("C", "O"), 0.5)
    assertEquals(value.evaluate("C", "C"), 3.0)
    assertEquals(value.evaluate("unknown", "unknown"), 0.0)

  test("hierarchy validation rejects cycles and decreasing heights"):
    val nodes = NonEmptyVector.of(Node.Root, Node.Warm, Node.C)
    val cycle =
      Hierarchy.build(
        nodes,
        Node.Root,
        {
          case Node.Root => None
          case Node.Warm => Some(Node.C)
          case Node.C    => Some(Node.Warm)
          case other     => fail(s"unexpected node $other")
        },
        height,
        Vector("C" -> Node.C)
      )
    assert(cycle.left.exists {
      case HierarchyError.Cycle(_) => true
      case _                       => false
    })

    val decreasing =
      Hierarchy.build(
        NonEmptyVector.of(Node.Root, Node.C),
        Node.Root,
        {
          case Node.Root => None
          case Node.C    => Some(Node.Root)
          case other     => fail(s"unexpected node $other")
        },
        {
          case Node.Root => NonNegativeFinite.from(2.0).toOption.get
          case Node.C    => NonNegativeFinite.from(1.0).toOption.get
          case other     => fail(s"unexpected node $other")
        },
        Vector("C" -> Node.C)
      )
    assert(decreasing.left.exists {
      case HierarchyError.DecreasingHeight(_, _, 2.0, 1.0) => true
      case _                                               => false
    })

  test("items must map injectively to actual leaves"):
    val duplicateLeaf =
      Hierarchy.build(
        NonEmptyVector.of(Node.Root, Node.C),
        Node.Root,
        {
          case Node.Root => None
          case Node.C    => Some(Node.Root)
          case other     => fail(s"unexpected node $other")
        },
        height,
        Vector("C" -> Node.C, "N" -> Node.C)
      )

    assert(duplicateLeaf.left.exists {
      case HierarchyError.DuplicateItemLeaf(_) => true
      case _                                   => false
    })

  test("threshold expansion reproduces hierarchy intersection"):
    val value = hierarchy
    val left = FiniteBag(Vector("C", "C", "N"))
    val right = FiniteBag(Vector("C", "N", "N"))
    val leftFeatures = value.thresholdFeatures(left).toMap
    val rightFeatures = value.thresholdFeatures(right).toMap
    val dot =
      leftFeatures.iterator.map { case (key, amount) =>
        amount * rightFeatures.getOrElse(key, 0.0)
      }.sum

    assertEqualsDouble(dot, value.optimalAssignment(left, right), 1e-12)

  test("hierarchy-backed strong kernel advertises only its realization"):
    val base = hierarchy.kernel

    assertEquals(
      base.optimalAssignmentRealization,
      Some(StrongKernelRealization.HierarchyIntersection)
    )
    assertEquals(
      base.normalized.fingerprintability.isInstanceOf[
        Fingerprintability.Ephemeral
      ],
      true
    )

  test("node-index acceleration resolves deliberate hash collisions"):
    final case class CollisionNode(value: Int) derives CanEqual
    given Hash[CollisionNode] with
      def hash(value: CollisionNode): Int = 0
      def eqv(
          left: CollisionNode,
          right: CollisionNode
      ): Boolean =
        left.value == right.value

    val root = CollisionNode(0)
    val branch = CollisionNode(1)
    val leaf = CollisionNode(2)
    val value =
      Hierarchy
        .build(
          NonEmptyVector.of(root, branch, leaf),
          root,
          {
            case `root`   => None
            case `branch` => Some(root)
            case `leaf`   => Some(branch)
            case other    => fail(s"unexpected node $other")
          },
          node =>
            NonNegativeFinite
              .from(node.value.toDouble)
              .toOption
              .get,
          Vector("leaf" -> leaf)
        )
        .toOption
        .get

    assertEquals(value.parentOf(leaf), Some(branch))
    assertEquals(
      value.leastCommonAncestor(leaf, branch),
      Some(branch)
    )
    assertEquals(value.evaluate("leaf", "leaf"), 2.0)
