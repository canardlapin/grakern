package grakern.core

import cats.kernel.Eq
import compiletime.testing.typeCheckErrors
import munit.FunSuite

final class KernelSuite extends FunSuite:
  given Eq[String] = Eq.fromUniversalEquals

  test("delta is a strong kernel and pullback preserves strongness"):
    val length = Projection.of[String, Int](_.length)
    val pulled: StrongKernel[String] = Kernel.delta[Int].on(length)

    assertEquals(pulled.evaluate("ab", "cd"), 1.0)
    assertEquals(pulled.evaluate("a", "cd"), 0.0)

  test("sum and product evaluate in expression order"):
    val delta = Kernel.delta[String]
    val one = Kernel.constant[String](NonNegativeFinite.one)
    val sum = delta + one
    val product = delta * one

    assertEquals(sum.evaluate("a", "a"), 2.0)
    assertEquals(sum.evaluate("a", "b"), 1.0)
    assertEquals(product.evaluate("a", "a"), 1.0)
    assertEquals(product.evaluate("a", "b"), 0.0)

  test("sum and normalization do not preserve the StrongKernel return type"):
    val errors = typeCheckErrors(
      """
        import cats.kernel.Eq
        import grakern.core.*
        given Eq[String] = Eq.fromUniversalEquals

        val delta = Kernel.delta[String]
        val invalidSum: StrongKernel[String] = delta + delta
        val invalidNormalization: StrongKernel[String] = delta.normalized
      """
    )
    assert(errors.size >= 2)

  test("normalization leaves zero-feature objects at zero"):
    val zero = Kernel.constant[String](NonNegativeFinite.zero).normalized

    assertEquals(zero.evaluate("a", "a"), 0.0)
    assertEquals(zero.evaluate("a", "b"), 0.0)

  test("linear and induced distance agree with Euclidean geometry"):
    val left = Vector(1.0, 2.0)
    val right = Vector(4.0, 6.0)

    assertEquals(Kernel.linear.evaluate(left, right), 16.0)
    assertEqualsDouble(
      Kernel.linear.inducedDistance.distance(left, right),
      5.0,
      1e-12
    )

  test("RBF is a safe numeric primitive"):
    val gamma = PositiveFinite.from(0.5).toOption.get
    val kernel = Kernel.rbf(gamma)

    assertEquals(kernel.evaluate(2.0, 2.0), 1.0)
    assertEqualsDouble(kernel.evaluate(2.0, 3.0), math.exp(-0.5), 1e-12)

  test("unsafe assumptions remain explicitly ephemeral"):
    val kernel = unsafe.assumeKernel[String]((left, right) => if left == right then 1.0 else 0.0)

    assert(kernel.fingerprintability.isInstanceOf[Fingerprintability.Ephemeral])
