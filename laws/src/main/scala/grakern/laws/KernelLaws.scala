package grakern.laws

import grakern.core.*
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Discipline laws for the core kernel algebra, evaluated through the reference interpreter.
  *
  * Scope: these rule sets are stated extensionally with tolerances and hold for the kernel families
  * grakern constructs (`delta`, `constant`, `linear`, `rbf`, hierarchy kernels) and for anything
  * closed under `+`, `*`, `weighted` and `normalized`. They are deliberately not applied to
  * `unsafe.assumeKernel`, whose PSD-ness is the caller's unverified assertion.
  */
object KernelLaws extends Laws:

  /** Laws every kernel must satisfy. `items` samples the domain for Gram matrices. */
  def kernel[A](name: String, k: Kernel[A])(using
      arbitrary: Arbitrary[A]
  ): RuleSet =
    new DefaultRuleSet(
      name = s"kernel[$name]",
      parent = None,
      "symmetry" -> forAll { (a: A, b: A) =>
        Tolerance.close(k.evaluate(a, b), k.evaluate(b, a), Tolerance.algebra)
      },
      "diagonal is non-negative" -> forAll((a: A) => k.evaluate(a, a) >= -Tolerance.bounds),
      "gram is positive semidefinite" -> forAll(items[A]) { xs =>
        val gram = Array.tabulate(xs.size, xs.size)((i, j) => k.evaluate(xs(i), xs(j)))
        val min = SymmetricEigen.minEigenvalue(gram)
        val scale = math.max(1.0, gram.map(_.map(math.abs).max).max)
        Prop(min >= -Tolerance.psd * scale) :| s"min eigenvalue $min"
      },
      "cauchy-schwarz" -> forAll { (a: A, b: A) =>
        val ab = k.evaluate(a, b)
        ab * ab <= k.evaluate(a, a) * k.evaluate(b, b) + Tolerance.algebra
      }
    )

  /** Algebraic laws of the combinators, stated extensionally. */
  def algebra[A](name: String, x: Kernel[A], y: Kernel[A], z: Kernel[A])(using
      Arbitrary[A]
  ): RuleSet =
    new DefaultRuleSet(
      name = s"algebra[$name]",
      parent = None,
      "sum is associative" -> forAll { (a: A, b: A) =>
        Tolerance.close(
          ((x + y) + z).evaluate(a, b),
          (x + (y + z)).evaluate(a, b),
          Tolerance.algebra
        )
      },
      "sum is commutative" -> forAll { (a: A, b: A) =>
        Tolerance.close((x + y).evaluate(a, b), (y + x).evaluate(a, b), Tolerance.algebra)
      },
      "weighted is homogeneous" -> forAll(weight, Arbitrary.arbitrary[A], Arbitrary.arbitrary[A]) {
        (w, a, b) =>
          Tolerance.close(
            x.weighted(w).evaluate(a, b),
            w.toDouble * x.evaluate(a, b),
            Tolerance.algebra
          )
      },
      "weighted distributes over sum" -> forAll(
        weight,
        Arbitrary.arbitrary[A],
        Arbitrary.arbitrary[A]
      ) { (w, a, b) =>
        Tolerance.close(
          (x + y).weighted(w).evaluate(a, b),
          (x.weighted(w) + y.weighted(w)).evaluate(a, b),
          Tolerance.algebra
        )
      },
      "product is commutative" -> forAll { (a: A, b: A) =>
        Tolerance.close((x * y).evaluate(a, b), (y * x).evaluate(a, b), Tolerance.algebra)
      }
    )

  /** Laws of `normalized`: bounded in [-1, 1] by Cauchy–Schwarz for every kernel, and in [0, 1]
    * when the base kernel is non-negative-valued (`nonNegative = true`; e.g. `rbf`, `delta`,
    * histogram and hierarchy kernels — not `linear`, whose cosine can be negative); unit diagonal
    * where the base diagonal is positive; zero where the base diagonal is zero.
    */
  def normalized[A](name: String, base: Kernel[A], nonNegative: Boolean)(using
      Arbitrary[A]
  ): RuleSet =
    val k = base.normalized
    val lower = if nonNegative then 0.0 else -1.0
    new DefaultRuleSet(
      name = s"normalized[$name]",
      parent = None,
      s"bounded in [$lower, 1]" -> forAll { (a: A, b: A) =>
        val v = k.evaluate(a, b)
        v >= lower - Tolerance.bounds && v <= 1.0 + Tolerance.bounds
      },
      "unit diagonal where the base diagonal is positive" -> forAll { (a: A) =>
        val self = base.evaluate(a, a)
        if self > 0.0 then Tolerance.close(k.evaluate(a, a), 1.0, Tolerance.bounds)
        else k.evaluate(a, a) == 0.0
      },
      "symmetric" -> forAll { (a: A, b: A) =>
        Tolerance.close(k.evaluate(a, b), k.evaluate(b, a), Tolerance.algebra)
      }
    )

  /** Weights are typed: negative, NaN and infinite values are rejected at construction. */
  val weights: RuleSet =
    new DefaultRuleSet(
      name = "weights",
      parent = None,
      "negative weights are rejected" -> forAll(Gen.choose(-1e6, -1e-300)) { (w: Double) =>
        NonNegativeFinite.from(w).isLeft
      },
      "non-finite weights are rejected" -> Prop(
        NonNegativeFinite.from(Double.NaN).isLeft &&
          NonNegativeFinite.from(Double.PositiveInfinity).isLeft &&
          NonNegativeFinite.from(Double.NegativeInfinity).isLeft
      ),
      "non-negative finite weights are accepted and round-trip" -> forAll(Gen.choose(0.0, 1e6)) {
        (w: Double) => NonNegativeFinite.from(w).exists(_.toDouble == w)
      }
    )

  private[laws] val weight: Gen[NonNegativeFinite] =
    Gen.choose(0.0, 8.0).map(w => NonNegativeFinite.from(w).getOrElse(NonNegativeFinite.zero))

  private[laws] def items[A](using arbitrary: Arbitrary[A]): Gen[Vector[A]] =
    Gen.choose(1, 6).flatMap(n => Gen.listOfN(n, arbitrary.arbitrary).map(_.toVector))
