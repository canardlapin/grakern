package grakern.core

import cats.kernel.Eq
import grakern.core.internal.*

trait PairScore[A]:
  def evaluate(left: A, right: A): Double

trait Similarity[A] extends PairScore[A]

sealed trait Kernel[A] extends Similarity[A]:
  private[grakern] def expression: KernelExpr[A]

  final def evaluate(left: A, right: A): Double =
    ReferenceInterpreter.evaluate(expression, left, right)

  def on[B](projection: Projection[B, A]): Kernel[B] =
    Kernel.fromExpression(
      KernelExpr.Pullback(PlanNodeId.fresh(), expression, projection)
    )

  def +(that: Kernel[A]): Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.Sum(PlanNodeId.fresh(), expression, that.expression)
    )

  def *(that: Kernel[A]): Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.Product(PlanNodeId.fresh(), expression, that.expression)
    )

  def weighted(weight: NonNegativeFinite): Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.Scale(PlanNodeId.fresh(), expression, weight)
    )

  def normalized: Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.Normalize(PlanNodeId.fresh(), expression)
    )

  def inducedDistance: PseudoMetric[A] =
    PseudoMetric(this)

  def rbf(gamma: PositiveFinite): Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.InducedRbf(PlanNodeId.fresh(), expression, gamma)
    )

  final def fingerprintability: Fingerprintability =
    KernelFingerprint(expression)

sealed trait StrongKernel[A] extends Kernel[A]:
  private[grakern] def assignmentEvaluator: Option[BagAssignmentEvaluator[A]]

  final def optimalAssignmentRealization: Option[StrongKernelRealization] =
    assignmentEvaluator.map(_.metadata)

  override def on[B](projection: Projection[B, A]): StrongKernel[B] =
    StrongKernel.fromExpression(
      KernelExpr.Pullback(PlanNodeId.fresh(), expression, projection),
      assignmentEvaluator.map(_.on(projection))
    )

  override def weighted(weight: NonNegativeFinite): StrongKernel[A] =
    StrongKernel.fromExpression(
      KernelExpr.Scale(PlanNodeId.fresh(), expression, weight),
      assignmentEvaluator.map(_.scaled(weight))
    )

object Kernel:
  def constant[A](value: NonNegativeFinite): StrongKernel[A] =
    StrongKernel.fromExpression(
      KernelExpr.Constant(PlanNodeId.fresh(), value)
    )

  def delta[A: Eq]: StrongKernel[A] =
    StrongKernel.fromExpression(
      KernelExpr.Delta(PlanNodeId.fresh(), Eq[A], None)
    )

  def deltaNamed[A: Eq](equalityId: StableId): StrongKernel[A] =
    StrongKernel.fromExpression(
      KernelExpr.Delta(PlanNodeId.fresh(), Eq[A], Some(equalityId))
    )

  val linear: Kernel[Vector[Double]] =
    fromExpression(KernelExpr.Linear(PlanNodeId.fresh()))

  def rbf(gamma: PositiveFinite): Kernel[Double] =
    fromExpression(KernelExpr.Rbf(PlanNodeId.fresh(), gamma))

  private[grakern] def fromExpression[A](
      value: KernelExpr[A]
  ): Kernel[A] =
    new Kernel[A]:
      val expression: KernelExpr[A] = value

object StrongKernel:
  private[grakern] def fromExpression[A](
      value: KernelExpr[A],
      assignment: Option[BagAssignmentEvaluator[A]] = None
  ): StrongKernel[A] =
    new StrongKernel[A]:
      val expression: KernelExpr[A] = value
      val assignmentEvaluator: Option[BagAssignmentEvaluator[A]] =
        assignment

final class PseudoMetric[A] private (kernel: Kernel[A]):
  def distance(left: A, right: A): Double =
    math.sqrt(
      kernel.evaluate(left, left) +
        kernel.evaluate(right, right) -
        2.0 * kernel.evaluate(left, right)
    )

object PseudoMetric:
  private[grakern] def apply[A](kernel: Kernel[A]): PseudoMetric[A] =
    new PseudoMetric(kernel)

object unsafe:
  def assumeSimilarity[A](f: (A, A) => Double): Similarity[A] =
    new Similarity[A]:
      def evaluate(left: A, right: A): Double = f(left, right)

  def assumeKernel[A](f: (A, A) => Double): Kernel[A] =
    Kernel.fromExpression(
      KernelExpr.Assumed(PlanNodeId.fresh(), f, "kernel")
    )

  def assumeStrongKernel[A](f: (A, A) => Double): StrongKernel[A] =
    StrongKernel.fromExpression(
      KernelExpr.Assumed(PlanNodeId.fresh(), f, "strong kernel")
    )
