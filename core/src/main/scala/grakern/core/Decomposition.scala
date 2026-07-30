package grakern.core

enum DecompositionError:
  case Numeric(error: NumericError)
  case InvariantViolation(
      sample: SampleKey,
      contract: DecompositionInvariant
  )

enum DecompositionInvariant derives CanEqual:
  case TotalNodeField
  case TotalEdgeField

/** A finite nonnegative weighted decomposition.
  *
  * `foreachWeighted` is the fused operational contract. `fold` is derived without materializing a
  * pattern collection. Composition remains fallible because multiplying two finite weights can
  * overflow `Double`.
  */
trait Decomposition[-A, P]:
  self =>

  def foreachWeighted(
      value: A
  )(
      emit: (
          P,
          NonNegativeFinite
      ) => Either[DecompositionError, Unit]
  ): Either[DecompositionError, Unit]

  final def fold[B](
      value: A,
      initial: B
  )(
      step: (
          B,
          P,
          NonNegativeFinite
      ) => B
  ): Either[DecompositionError, B] =
    var result = initial
    foreachWeighted(value) { (pattern, weight) =>
      result = step(result, pattern, weight)
      Right(())
    }.map(_ => result)

  final def map[Q](f: P => Q): Decomposition[A, Q] =
    new Decomposition[A, Q]:
      def foreachWeighted(
          value: A
      )(
          emit: (
              Q,
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        self.foreachWeighted(value) { (pattern, weight) =>
          emit(f(pattern), weight)
        }

  final def flatMap[Q](
      f: P => Decomposition[Unit, Q]
  ): Decomposition[A, Q] =
    new Decomposition[A, Q]:
      def foreachWeighted(
          value: A
      )(
          emit: (
              Q,
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        self.foreachWeighted(value) { (pattern, outerWeight) =>
          f(pattern).foreachWeighted(()) { (nested, innerWeight) =>
            (outerWeight * innerWeight).left
              .map(DecompositionError.Numeric(_))
              .flatMap(weight => emit(nested, weight))
          }
        }

  final def filter(
      predicate: P => Boolean
  ): Decomposition[A, P] =
    new Decomposition[A, P]:
      def foreachWeighted(
          value: A
      )(
          emit: (
              P,
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        self.foreachWeighted(value) { (pattern, weight) =>
          if predicate(pattern) then emit(pattern, weight)
          else Right(())
        }

  final def reweight(
      factor: P => NonNegativeFinite
  ): Decomposition[A, P] =
    new Decomposition[A, P]:
      def foreachWeighted(
          value: A
      )(
          emit: (
              P,
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        self.foreachWeighted(value) { (pattern, weight) =>
          (weight * factor(pattern)).left
            .map(DecompositionError.Numeric(_))
            .flatMap(updated => emit(pattern, updated))
        }

object Decomposition:
  def empty[A, P]: Decomposition[A, P] =
    fromWeighted(_ => Vector.empty)

  def single[P](
      pattern: P,
      weight: NonNegativeFinite = NonNegativeFinite.one
  ): Decomposition[Unit, P] =
    fromWeighted(_ => Vector(pattern -> weight))

  def fromWeighted[A, P](
      patterns: A => IterableOnce[(P, NonNegativeFinite)]
  ): Decomposition[A, P] =
    new Decomposition[A, P]:
      def foreachWeighted(
          value: A
      )(
          emit: (
              P,
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        val iterator = patterns(value).iterator
        var failure: Option[DecompositionError] = None
        while iterator.hasNext && failure.isEmpty do
          val (pattern, weight) = iterator.next()
          emit(pattern, weight) match
            case Left(error) => failure = Some(error)
            case Right(_)    => ()
        failure.toLeft(())
