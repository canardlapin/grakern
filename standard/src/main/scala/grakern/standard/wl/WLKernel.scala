package grakern.standard.wl

import grakern.core.*

/** A lawful WL kernel program.
  *
  * The public value is immutable. Its closed expression is visible only to grakern's engine, so
  * v0.1 does not publish a lowering SPI before the paired WL slice has established the contract.
  */
final class WLKernel[N, E, NodeKey, EdgeKey] private[wl] (
    private[grakern] val expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
):
  def planNodeId: PlanNodeId = expression.planNodeId

  def fingerprintability: Fingerprintability =
    expression.fingerprintability

  def +(
      that: WLKernel[N, E, NodeKey, EdgeKey]
  ): WLKernel[N, E, NodeKey, EdgeKey] =
    new WLKernel(
      WLKernelExpr.Sum(
        PlanNodeId.fresh(),
        expression,
        that.expression
      )
    )

  def weighted(
      weight: NonNegativeFinite
  ): WLKernel[N, E, NodeKey, EdgeKey] =
    new WLKernel(
      WLKernelExpr.Scale(
        PlanNodeId.fresh(),
        expression,
        weight
      )
    )

  def normalized: WLKernel[N, E, NodeKey, EdgeKey] =
    new WLKernel(
      WLKernelExpr.Normalize(
        PlanNodeId.fresh(),
        expression
      )
    )

sealed private[grakern] trait WLKernelExpr[N, E, NodeKey, EdgeKey]:
  def planNodeId: PlanNodeId
  def fingerprintability: Fingerprintability

private[grakern] object WLKernelExpr:
  final case class Subtree[N, E, NodeKey, EdgeKey](
      planNodeId: PlanNodeId,
      refinement: WLRefinement[N, E, NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite]
  ) extends WLKernelExpr[N, E, NodeKey, EdgeKey]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.wl-subtree.v1",
        atoms = roundWeights.map(_.toDouble.toString),
        children = Vector(refinement.fingerprintability)
      )

  final case class OptimalAssignment[N, E, NodeKey, EdgeKey](
      planNodeId: PlanNodeId,
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ) extends WLKernelExpr[N, E, NodeKey, EdgeKey]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.wl-optimal-assignment.v1",
        atoms = Vector("threshold-expansion"),
        children = Vector(refinement.fingerprintability)
      )

  final case class Scale[N, E, NodeKey, EdgeKey](
      planNodeId: PlanNodeId,
      child: WLKernelExpr[N, E, NodeKey, EdgeKey],
      weight: NonNegativeFinite
  ) extends WLKernelExpr[N, E, NodeKey, EdgeKey]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.wl-scale.v1",
        atoms = Vector(weight.toDouble.toString),
        children = Vector(child.fingerprintability)
      )

  final case class Sum[N, E, NodeKey, EdgeKey](
      planNodeId: PlanNodeId,
      left: WLKernelExpr[N, E, NodeKey, EdgeKey],
      right: WLKernelExpr[N, E, NodeKey, EdgeKey]
  ) extends WLKernelExpr[N, E, NodeKey, EdgeKey]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.wl-sum.v1",
        atoms = Vector.empty,
        children = Vector(
          left.fingerprintability,
          right.fingerprintability
        )
      )

  final case class Normalize[N, E, NodeKey, EdgeKey](
      planNodeId: PlanNodeId,
      child: WLKernelExpr[N, E, NodeKey, EdgeKey]
  ) extends WLKernelExpr[N, E, NodeKey, EdgeKey]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.wl-normalize.v1",
        atoms = Vector("zero-diagonal-is-zero"),
        children = Vector(child.fingerprintability)
      )

extension [N, E, NodeKey, EdgeKey](
    refinement: WLRefinement[N, E, NodeKey, EdgeKey]
)
  def subtree: WLKernel[N, E, NodeKey, EdgeKey] =
    new WLKernel(
      WLKernelExpr.Subtree(
        PlanNodeId.fresh(),
        refinement,
        Vector.fill(refinement.rounds + 1)(
          NonNegativeFinite.one
        )
      )
    )

  def subtree(
      roundWeights: Vector[NonNegativeFinite]
  ): Either[
    WLSubtreeKernelError,
    WLKernel[N, E, NodeKey, EdgeKey]
  ] =
    val expected = refinement.rounds + 1
    if roundWeights.size != expected then
      Left(
        WLSubtreeKernelError.WeightCount(
          expected,
          roundWeights.size
        )
      )
    else
      Right(
        new WLKernel(
          WLKernelExpr.Subtree(
            PlanNodeId.fresh(),
            refinement,
            roundWeights
          )
        )
      )

  def optimalAssignment: WLKernel[N, E, NodeKey, EdgeKey] =
    new WLKernel(
      WLKernelExpr.OptimalAssignment(
        PlanNodeId.fresh(),
        refinement
      )
    )

enum WLSubtreeKernelError:
  case WeightCount(expected: Int, observed: Int)
