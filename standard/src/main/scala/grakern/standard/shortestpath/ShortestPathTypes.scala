package grakern.standard.shortestpath

import cats.kernel.{Eq, Hash}
import grakern.core.SampleKey
import scala.util.hashing.MurmurHash3

enum HopDistanceError derives CanEqual:
  case NonPositive(value: Int)

/** A strictly positive, unweighted shortest-path length. */
opaque type HopDistance = Int

object HopDistance:
  def from(value: Int): Either[HopDistanceError, HopDistance] =
    if value > 0 then Right(value)
    else Left(HopDistanceError.NonPositive(value))

  /** Internal constructor used after the reference algorithm has established that two distinct
    * vertices are connected. A shortest simple path in a finite graph has at most `n - 1` edges, so
    * the value also fits in `Int`.
    */
  private[shortestpath] def knownPositive(value: Int): HopDistance =
    value

  extension (distance: HopDistance) def toInt: Int = distance

  given CanEqual[HopDistance, HopDistance] = CanEqual.derived
  given Hash[HopDistance] = Hash.fromUniversalHashCode

/** An unordered endpoint pair.
  *
  * `first` and `second` are exposed for explanation and rendering only. Both ordinary Scala
  * equality and Cats equality treat the two endpoint orders as identical.
  */
final class UndirectedEndpoints[A] private (
    val first: A,
    val second: A
):
  override def equals(other: Any): Boolean =
    other match
      case endpoints: UndirectedEndpoints[?] =>
        (
          scala.runtime.BoxesRunTime.equals(first, endpoints.first) &&
            scala.runtime.BoxesRunTime.equals(second, endpoints.second)
        ) ||
        (
          scala.runtime.BoxesRunTime.equals(first, endpoints.second) &&
            scala.runtime.BoxesRunTime.equals(second, endpoints.first)
        )
      case _ =>
        false

  override def hashCode(): Int =
    MurmurHash3.unorderedHash(
      Iterator(first.##, second.##),
      MurmurHash3.productSeed
    )

  override def toString: String =
    s"UndirectedEndpoints($first, $second)"

private[shortestpath] trait UndirectedEndpointsLowPriorityInstances:
  given [A: Eq]: Eq[UndirectedEndpoints[A]] =
    Eq.instance { (left, right) =>
      val A = Eq[A]
      (A.eqv(left.first, right.first) && A.eqv(left.second, right.second)) ||
      (A.eqv(left.first, right.second) && A.eqv(left.second, right.first))
    }

object UndirectedEndpoints extends UndirectedEndpointsLowPriorityInstances:
  def apply[A](first: A, second: A): UndirectedEndpoints[A] =
    new UndirectedEndpoints(first, second)

  given [A]: CanEqual[UndirectedEndpoints[A], UndirectedEndpoints[A]] =
    CanEqual.derived

  given [A: Hash]: Hash[UndirectedEndpoints[A]] with
    def eqv(
        left: UndirectedEndpoints[A],
        right: UndirectedEndpoints[A]
    ): Boolean =
      val A = Hash[A]
      (A.eqv(left.first, right.first) && A.eqv(left.second, right.second)) ||
      (A.eqv(left.first, right.second) && A.eqv(left.second, right.first))

    def hash(endpoints: UndirectedEndpoints[A]): Int =
      val A = Hash[A]
      MurmurHash3.unorderedHash(
        Iterator(A.hash(endpoints.first), A.hash(endpoints.second)),
        MurmurHash3.productSeed
      )

final case class ShortestPathPart[A, +D](
    endpoints: UndirectedEndpoints[A],
    distance: D
)

object ShortestPathPart:
  given [A: Hash, D: Hash]: Hash[ShortestPathPart[A, D]] with
    def eqv(
        left: ShortestPathPart[A, D],
        right: ShortestPathPart[A, D]
    ): Boolean =
      Hash[UndirectedEndpoints[A]].eqv(left.endpoints, right.endpoints) &&
        Hash[D].eqv(left.distance, right.distance)

    def hash(part: ShortestPathPart[A, D]): Int =
      MurmurHash3.mix(
        Hash[UndirectedEndpoints[A]].hash(part.endpoints),
        Hash[D].hash(part.distance)
      )

final case class ShortestPathWitness[V](
    sample: SampleKey,
    endpoints: UndirectedEndpoints[V]
)

final case class ShortestPathOccurrence[V, A, +D](
    part: ShortestPathPart[A, D],
    witness: ShortestPathWitness[V]
)
