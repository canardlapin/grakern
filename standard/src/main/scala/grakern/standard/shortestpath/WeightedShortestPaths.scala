package grakern.standard.shortestpath

import cats.kernel.Eq
import grakern.core.*

/** A finite, nonnegative weighted shortest-path length.
  *
  * This is distinct from `HopDistance`: two different vertices may have weighted distance zero when
  * connected by zero-length edges.
  */
opaque type PathLength = Double

object PathLength:
  def from(value: Double): Either[NumericError, PathLength] =
    NonNegativeFinite.from(value).map(_.toDouble)

  private[grakern] def knownNonNegative(value: Double): PathLength =
    value

  extension (length: PathLength) def toDouble: Double = length

  given CanEqual[PathLength, PathLength] = CanEqual.derived
  given Eq[PathLength] = Eq.fromUniversalEquals

/** One reusable choice of weighted-edge semantics.
  *
  * Reusing this value gives multiple path-kernel consumers one session-local identity for shared
  * Dijkstra preparation.
  */
final class WeightedEdgeLengths[E] private[shortestpath] (
    val planNodeId: PlanNodeId,
    val descriptor: ProjectionDescriptor,
    private[grakern] val projection: Projection[E, Double]
):
  def fingerprintability: Fingerprintability =
    Fingerprintability.combine(
      schema = "grakern.shortest-path.edge-lengths.v1",
      atoms = Vector.empty,
      children = Vector(projection.fingerprintAt(planNodeId))
    )

object WeightedEdgeLengths:
  def apply[E](
      descriptor: ProjectionDescriptor,
      projection: Projection[E, Double]
  ): WeightedEdgeLengths[E] =
    new WeightedEdgeLengths(
      PlanNodeId.fresh(),
      descriptor,
      projection
    )

final class WeightedShortestPathRecipe[N, E, Key] private[shortestpath] (
    val planNodeId: PlanNodeId,
    val edgeLengths: WeightedEdgeLengths[E],
    private[grakern] val nodeKey: Projection[N, Key]
):
  def fingerprintability: Fingerprintability =
    Fingerprintability.combine(
      schema = "grakern.shortest-path.weighted.v1",
      atoms = Vector.empty,
      children = Vector(
        nodeKey.fingerprintAt(planNodeId),
        edgeLengths.fingerprintability
      )
    )

object WeightedShortestPaths:
  def parts[N, E, Key](
      nodeKey: Projection[N, Key],
      edgeLengths: WeightedEdgeLengths[E]
  ): WeightedShortestPathRecipe[N, E, Key] =
    new WeightedShortestPathRecipe(
      PlanNodeId.fresh(),
      edgeLengths,
      nodeKey
    )
