package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.*
import munit.FunSuite

final class WLKernelSuite extends FunSuite:
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def refinement =
    WLRefinement
      .create(
        2,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs(None, None)
      )
      .toOption
      .get

  test("one refinement value is retained by both WL consumers"):
    val shared = refinement
    val subtree = shared.subtree
    val assignment = shared.optimalAssignment

    subtree.expression match
      case node: WLKernelExpr.Subtree[Atom, Bond, String, Int] =>
        assertEquals(
          node.refinement.planNodeId.number,
          shared.planNodeId.number
        )
      case _ =>
        fail("expected a subtree expression")
    assignment.expression match
      case node: WLKernelExpr.OptimalAssignment[
            Atom,
            Bond,
            String,
            Int
          ] =>
        assertEquals(
          node.refinement.planNodeId.number,
          shared.planNodeId.number
        )
      case _ =>
        fail("expected an optimal-assignment expression")

  test("weighted sums and normalization allocate explicit plan nodes"):
    val shared = refinement
    val left = shared.subtree.weighted(NonNegativeFinite.one)
    val right = shared.optimalAssignment
    val combined = (left + right).normalized

    assertNotEquals(left.planNodeId.number, right.planNodeId.number)
    assertNotEquals(combined.planNodeId.number, left.planNodeId.number)
    assert(
      combined.expression
        .isInstanceOf[
          WLKernelExpr.Normalize[Atom, Bond, String, Int]
        ]
    )

  test("round weights must cover level zero and every refinement round"):
    val result =
      refinement.subtree(Vector(NonNegativeFinite.one))

    assertEquals(
      result,
      Left(WLSubtreeKernelError.WeightCount(3, 1))
    )
