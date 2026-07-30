package grakern.standard.shortestpath

import grakern.core.*

/** A lawful continuous shortest-path convolution program.
  *
  * Endpoint pairs are unordered. Given a base endpoint kernel `k`, their kernel is the
  * tensor-symmetrized value
  *
  * `k(a, c) * k(b, d) + k(a, d) * k(b, c)`.
  *
  * This preserves positive semidefiniteness without imposing an arbitrary ordering on endpoint
  * values. The result is multiplied by the distance kernel and lifted through the weighted
  * shortest-path decomposition.
  */
final class SoftShortestPathKernel[N, E, Key] private[shortestpath] (
    private[grakern] val expression: SoftShortestPathKernelExpr[N, E, Key]
):
  def planNodeId: PlanNodeId =
    expression.planNodeId

  def fingerprintability: Fingerprintability =
    expression.fingerprintability

  def normalized: SoftShortestPathKernel[N, E, Key] =
    new SoftShortestPathKernel(
      SoftShortestPathKernelExpr.Normalize(
        PlanNodeId.fresh(),
        expression
      )
    )

sealed private[grakern] trait SoftShortestPathKernelExpr[N, E, Key]:
  def planNodeId: PlanNodeId
  def fingerprintability: Fingerprintability

private[grakern] object SoftShortestPathKernelExpr:
  final case class Convolution[N, E, Key](
      planNodeId: PlanNodeId,
      recipe: WeightedShortestPathRecipe[N, E, Key],
      endpointKernel: Kernel[Key],
      distanceKernel: Kernel[Double]
  ) extends SoftShortestPathKernelExpr[N, E, Key]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.shortest-path.soft-convolution.v1",
        atoms = Vector("tensor-symmetrized-unordered-endpoints"),
        children = Vector(
          recipe.fingerprintability,
          endpointKernel.fingerprintability,
          distanceKernel.fingerprintability
        )
      )

  final case class Normalize[N, E, Key](
      planNodeId: PlanNodeId,
      child: SoftShortestPathKernelExpr[N, E, Key]
  ) extends SoftShortestPathKernelExpr[N, E, Key]:
    def fingerprintability: Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.shortest-path.soft-normalize.v1",
        atoms = Vector("zero-diagonal-is-zero"),
        children = Vector(child.fingerprintability)
      )

object SoftShortestPath:
  def kernel[N, E, Key](
      recipe: WeightedShortestPathRecipe[N, E, Key],
      endpointKernel: Kernel[Key],
      distanceKernel: Kernel[Double]
  ): SoftShortestPathKernel[N, E, Key] =
    new SoftShortestPathKernel(
      SoftShortestPathKernelExpr.Convolution(
        PlanNodeId.fresh(),
        recipe,
        endpointKernel,
        distanceKernel
      )
    )
