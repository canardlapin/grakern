package grakern.core

import cats.data.NonEmptyVector
import cats.kernel.{Eq, Hash}
import compiletime.testing.typeCheckErrors
import munit.FunSuite

final class OptimalAssignmentSuite extends FunSuite:
  enum Node derives CanEqual:
    case Root, Left, Right, A, B, C

  given Hash[Node] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private def hierarchy: Hierarchy[String, Node] =
    val height = Map(
      Node.Root -> 0.5,
      Node.Left -> 1.5,
      Node.Right -> 1.0,
      Node.A -> 3.0,
      Node.B -> 2.5,
      Node.C -> 2.0
    )
    val parent = Map(
      Node.Left -> Node.Root,
      Node.Right -> Node.Root,
      Node.A -> Node.Left,
      Node.B -> Node.Left,
      Node.C -> Node.Right
    )
    Hierarchy
      .build(
        NonEmptyVector.of(
          Node.Root,
          Node.Left,
          Node.Right,
          Node.A,
          Node.B,
          Node.C
        ),
        Node.Root,
        node => parent.get(node),
        node => NonNegativeFinite.from(height(node)).toOption.get,
        Vector("A" -> Node.A, "B" -> Node.B, "C" -> Node.C)
      )
      .toOption
      .get

  test("histogram intersection agrees with padded Hungarian assignment"):
    val value = hierarchy
    val base = value.kernel
    val examples = Vector(
      FiniteBag.empty[String],
      FiniteBag(Vector("A")),
      FiniteBag(Vector("A", "A")),
      FiniteBag(Vector("A", "B")),
      FiniteBag(Vector("A", "B", "C"))
    )

    examples.foreach { left =>
      examples.foreach { right =>
        assertEqualsDouble(
          value.optimalAssignment(left, right),
          OptimalAssignment.reference(base, left, right),
          1e-12,
          clues(left, right)
        )
      }
    }

  test("external zero padding does not add the positive root baseline"):
    val value = hierarchy
    val short = FiniteBag(Vector("A"))
    val long = FiniteBag(Vector("C", "C"))

    assertEquals(value.optimalAssignment(short, long), 0.5)
    assertEquals(
      OptimalAssignment(value.kernel).evaluate(short, long),
      0.5
    )

  test("optimal assignment requires StrongKernel at compile time"):
    val errors = typeCheckErrors(
      """
        import grakern.core.*

        val gamma = PositiveFinite.from(0.5).toOption.get
        val invalid = OptimalAssignment(Kernel.rbf(gamma))
      """
    )

    assert(errors.nonEmpty)

  test("finite bags retain repeated occurrences"):
    val bag = FiniteBag(Vector("A", "A", "B"))

    assertEquals(bag.size, 3)
    assertEquals(bag.multiplicityOf("A"), 2)
