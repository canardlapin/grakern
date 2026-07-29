package grakern.core

final case class SampleKey(value: String)
final case class ProjectionDescriptor(value: String)

sealed trait SourceRef[+V, +E]:
  def sample: SampleKey

object SourceRef:
  final case class Sample(sample: SampleKey) extends SourceRef[Nothing, Nothing]
  final case class Node[V](sample: SampleKey, vertex: V) extends SourceRef[V, Nothing]
  final case class Edge[E](sample: SampleKey, edge: E) extends SourceRef[Nothing, E]

sealed trait PrepareError[+V, +E]

object PrepareError:
  final case class InvalidNumeric[V, E](
      source: SourceRef[V, E],
      projection: ProjectionDescriptor,
      value: Double,
      reason: NumericError
  ) extends PrepareError[V, E]

  final case class InvalidNegativeDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double,
      tolerance: NonNegativeFinite
  ) extends PrepareError[Nothing, Nothing]

  final case class InvalidNormalizedValue(
      left: SampleKey,
      right: SampleKey,
      value: Double,
      tolerance: NonNegativeFinite
  ) extends PrepareError[Nothing, Nothing]

  final case class InvalidConfiguration(message: String) extends PrepareError[Nothing, Nothing]

enum NumericalAuditEvent:
  case DiagonalTreatedAsZero(
      sample: SampleKey,
      index: Int,
      observed: Double,
      tolerance: NonNegativeFinite
  )
