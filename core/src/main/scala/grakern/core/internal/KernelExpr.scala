package grakern.core.internal

import cats.kernel.Eq
import grakern.core.*

sealed private[grakern] trait KernelExpr[A]:
  def nodeId: PlanNodeId

private[grakern] object KernelExpr:
  final case class Constant[A](
      nodeId: PlanNodeId,
      value: NonNegativeFinite
  ) extends KernelExpr[A]

  final case class Delta[A](
      nodeId: PlanNodeId,
      equality: Eq[A],
      descriptor: Option[StableId]
  ) extends KernelExpr[A]

  final case class Linear(
      nodeId: PlanNodeId
  ) extends KernelExpr[Vector[Double]]

  final case class Rbf(
      nodeId: PlanNodeId,
      gamma: PositiveFinite
  ) extends KernelExpr[Double]

  final case class Pullback[A, B](
      nodeId: PlanNodeId,
      source: KernelExpr[B],
      projection: Projection[A, B]
  ) extends KernelExpr[A]

  final case class Sum[A](
      nodeId: PlanNodeId,
      left: KernelExpr[A],
      right: KernelExpr[A]
  ) extends KernelExpr[A]

  final case class Product[A](
      nodeId: PlanNodeId,
      left: KernelExpr[A],
      right: KernelExpr[A]
  ) extends KernelExpr[A]

  final case class Scale[A](
      nodeId: PlanNodeId,
      source: KernelExpr[A],
      weight: NonNegativeFinite
  ) extends KernelExpr[A]

  final case class Normalize[A](
      nodeId: PlanNodeId,
      source: KernelExpr[A]
  ) extends KernelExpr[A]

  final case class InducedRbf[A](
      nodeId: PlanNodeId,
      source: KernelExpr[A],
      gamma: PositiveFinite
  ) extends KernelExpr[A]

  final case class HierarchyBase[A](
      nodeId: PlanNodeId,
      evaluator: (A, A) => Double
  ) extends KernelExpr[A]

  final case class OptimalAssignment[A](
      nodeId: PlanNodeId,
      base: KernelExpr[A],
      referenceEvaluator: (FiniteBag[A], FiniteBag[A]) => Double,
      realization: Option[BagAssignmentEvaluator[A]]
  ) extends KernelExpr[FiniteBag[A]]

  final case class Assumed[A](
      nodeId: PlanNodeId,
      evaluator: (A, A) => Double,
      claim: String
  ) extends KernelExpr[A]

private[grakern] object ReferenceInterpreter:
  def evaluate[A](expression: KernelExpr[A], left: A, right: A): Double =
    expression match
      case KernelExpr.Constant(_, value) =>
        value.toDouble
      case delta: KernelExpr.Delta[A] =>
        if delta.equality.eqv(left, right) then 1.0 else 0.0
      case KernelExpr.Linear(_) =>
        dot(left, right)
      case KernelExpr.Rbf(_, gamma) =>
        val difference = left - right
        math.exp(-gamma.toDouble * difference * difference)
      case pullback: KernelExpr.Pullback[A, ?] =>
        evaluate(
          pullback.source,
          pullback.projection(left),
          pullback.projection(right)
        )
      case KernelExpr.Sum(_, first, second) =>
        evaluate(first, left, right) + evaluate(second, left, right)
      case KernelExpr.Product(_, first, second) =>
        evaluate(first, left, right) * evaluate(second, left, right)
      case KernelExpr.Scale(_, source, weight) =>
        weight.toDouble * evaluate(source, left, right)
      case KernelExpr.Normalize(_, source) =>
        val diagonalLeft = evaluate(source, left, left)
        val diagonalRight = evaluate(source, right, right)
        if diagonalLeft == 0.0 || diagonalRight == 0.0 then 0.0
        else
          evaluate(source, left, right) /
            math.sqrt(diagonalLeft * diagonalRight)
      case KernelExpr.InducedRbf(_, source, gamma) =>
        val squaredDistance =
          evaluate(source, left, left) +
            evaluate(source, right, right) -
            2.0 * evaluate(source, left, right)
        math.exp(-gamma.toDouble * squaredDistance)
      case KernelExpr.HierarchyBase(_, evaluator) =>
        evaluator(left, right)
      case KernelExpr.OptimalAssignment(_, _, evaluator, _) =>
        evaluator(left, right)
      case KernelExpr.Assumed(_, evaluator, _) =>
        evaluator(left, right)

  private def dot(left: Vector[Double], right: Vector[Double]): Double =
    val count = math.min(left.size, right.size)
    var result = 0.0
    var index = 0
    while index < count do
      result += left(index) * right(index)
      index += 1
    result

private[grakern] object KernelFingerprint:
  def apply[A](expression: KernelExpr[A]): Fingerprintability =
    expression match
      case KernelExpr.Constant(_, value) =>
        durable("constant", Vector(bits(value.toDouble)))
      case KernelExpr.Delta(_, _, Some(descriptor)) =>
        durable("delta", Vector(descriptor.text))
      case KernelExpr.Delta(node, _, None) =>
        Fingerprintability.Ephemeral(
          FingerprintBlockers.one(
            FingerprintBlocker.MissingCodec(node, "delta equality")
          )
        )
      case KernelExpr.Linear(_) =>
        durable("linear", Vector.empty)
      case KernelExpr.Rbf(_, gamma) =>
        durable("rbf", Vector(bits(gamma.toDouble)))
      case KernelExpr.Pullback(node, source, projection) =>
        Fingerprintability.combine(
          "pullback",
          Vector.empty,
          Vector(apply(source), projection.fingerprintAt(node))
        )
      case KernelExpr.Sum(_, left, right) =>
        combine("sum", left, right)
      case KernelExpr.Product(_, left, right) =>
        combine("product", left, right)
      case KernelExpr.Scale(_, source, weight) =>
        Fingerprintability.combine(
          "scale",
          Vector(bits(weight.toDouble)),
          Vector(apply(source))
        )
      case KernelExpr.Normalize(_, source) =>
        Fingerprintability.combine(
          "normalize",
          Vector.empty,
          Vector(apply(source))
        )
      case KernelExpr.InducedRbf(_, source, gamma) =>
        Fingerprintability.combine(
          "induced-rbf",
          Vector(bits(gamma.toDouble)),
          Vector(apply(source))
        )
      case KernelExpr.HierarchyBase(node, _) =>
        Fingerprintability.Ephemeral(
          FingerprintBlockers.one(
            FingerprintBlocker.MissingCodec(node, "hierarchy")
          )
        )
      case KernelExpr.OptimalAssignment(_, base, _, _) =>
        Fingerprintability.combine(
          "optimal-assignment",
          Vector.empty,
          Vector(apply(base))
        )
      case KernelExpr.Assumed(node, _, claim) =>
        Fingerprintability.Ephemeral(
          FingerprintBlockers.one(
            FingerprintBlocker.UnsafeAssumption(node, claim)
          )
        )

  private def combine[A](
      schema: String,
      left: KernelExpr[A],
      right: KernelExpr[A]
  ): Fingerprintability =
    Fingerprintability.combine(
      schema,
      Vector.empty,
      Vector(apply(left), apply(right))
    )

  private def durable(
      schema: String,
      atoms: Vector[String]
  ): Fingerprintability =
    Fingerprintability.Durable(
      SemanticFingerprint.compose(schema, atoms, Vector.empty)
    )

  private def bits(value: Double): String =
    java.lang.Long.toUnsignedString(
      java.lang.Double.doubleToRawLongBits(value),
      16
    )
