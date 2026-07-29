package grakern.architecture.probe

import cats.kernel.Eq
import graph4s.{Edge, Graph}

final case class SampleKey(value: String)

final class GraphSample[V, N, E] private (
    val key: SampleKey,
    val topology: Graph[V],
    val nodeValue: V => N,
    val edgeValue: Edge[V] => E
)

object GraphSample:
  def build[V, N, E](
      key: SampleKey,
      topology: Graph[V]
  )(
      nodeValue: V => N,
      edgeValue: Edge[V] => E
  ): GraphSample[V, N, E] =
    new GraphSample(key, topology, nodeValue, edgeValue)

enum NumericError:
  case NonFinite(value: Double)
  case Negative(value: Double)
  case NonPositive(value: Double)

opaque type FiniteDouble = Double

object FiniteDouble:
  def from(value: Double): Either[NumericError, FiniteDouble] =
    if value.isFinite then Right(value)
    else Left(NumericError.NonFinite(value))

  extension (value: FiniteDouble) def toDouble: Double = value

opaque type NonNegativeFinite = Double

object NonNegativeFinite:
  def from(value: Double): Either[NumericError, NonNegativeFinite] =
    if !value.isFinite then Left(NumericError.NonFinite(value))
    else if value < 0.0 then Left(NumericError.Negative(value))
    else Right(if value == 0.0 then 0.0 else value)

  extension (value: NonNegativeFinite) def toDouble: Double = value

opaque type PositiveFinite = Double

object PositiveFinite:
  def from(value: Double): Either[NumericError, PositiveFinite] =
    if !value.isFinite then Left(NumericError.NonFinite(value))
    else if value <= 0.0 then Left(NumericError.NonPositive(value))
    else Right(value)

  extension (value: PositiveFinite) def toDouble: Double = value

final case class StableId(value: String)

sealed trait Projection[-A, +B]:
  def apply(value: A): B

sealed trait DurableProjection[-A, +B] extends Projection[A, B]:
  def id: StableId
  def version: Int

object Projection:
  def of[A, B](f: A => B): Projection[A, B] =
    new Projection[A, B]:
      def apply(value: A): B = f(value)

  def named[A, B](
      stableId: StableId,
      stableVersion: Int
  )(f: A => B): DurableProjection[A, B] =
    new DurableProjection[A, B]:
      val id: StableId = stableId
      val version: Int = stableVersion
      def apply(value: A): B = f(value)

final case class SemanticFingerprint private (value: String)

object SemanticFingerprint:
  def fromProjection[A, B](
      projection: DurableProjection[A, B]
  ): SemanticFingerprint =
    SemanticFingerprint(s"${projection.id.value}:${projection.version}")

final case class PlanNodeId(value: Long)
final case class ArtifactFingerprint(value: String)

enum FingerprintBlocker:
  case AnonymousProjection(node: PlanNodeId)
  case MissingCodec(node: PlanNodeId)

enum Fingerprintability:
  case Durable(fingerprint: SemanticFingerprint)
  case Ephemeral(blockers: Vector[FingerprintBlocker])

trait PairScore[A]:
  def evaluate(left: A, right: A): Double

trait Similarity[A] extends PairScore[A]

trait Kernel[A] extends Similarity[A]:
  self =>

  def on[B](projection: Projection[B, A]): Kernel[B] =
    new Kernel[B]:
      def evaluate(left: B, right: B): Double =
        self.evaluate(projection(left), projection(right))

  def +(that: Kernel[A]): Kernel[A] =
    new Kernel[A]:
      def evaluate(left: A, right: A): Double =
        self.evaluate(left, right) + that.evaluate(left, right)

  def *(that: Kernel[A]): Kernel[A] =
    new Kernel[A]:
      def evaluate(left: A, right: A): Double =
        self.evaluate(left, right) * that.evaluate(left, right)

  def normalized: Kernel[A] =
    new Kernel[A]:
      def evaluate(left: A, right: A): Double =
        val leftDiagonal = self.evaluate(left, left)
        val rightDiagonal = self.evaluate(right, right)
        if leftDiagonal <= 0.0 || rightDiagonal <= 0.0 then 0.0
        else self.evaluate(left, right) / math.sqrt(leftDiagonal * rightDiagonal)

trait StrongKernel[A] extends Kernel[A]:
  self =>

  override def on[B](projection: Projection[B, A]): StrongKernel[B] =
    new StrongKernel[B]:
      def evaluate(left: B, right: B): Double =
        self.evaluate(projection(left), projection(right))

  def weighted(weight: NonNegativeFinite): StrongKernel[A] =
    new StrongKernel[A]:
      def evaluate(left: A, right: A): Double =
        weight.toDouble * self.evaluate(left, right)

object Kernel:
  def delta[A: Eq]: StrongKernel[A] =
    new StrongKernel[A]:
      def evaluate(left: A, right: A): Double =
        if Eq[A].eqv(left, right) then 1.0 else 0.0

  def rbf(gamma: PositiveFinite): Kernel[Double] =
    new Kernel[Double]:
      def evaluate(left: Double, right: Double): Double =
        val difference = left - right
        math.exp(-gamma.toDouble * difference * difference)

object unsafe:
  def assumeSimilarity[A](f: (A, A) => Double): Similarity[A] =
    new Similarity[A]:
      def evaluate(left: A, right: A): Double = f(left, right)

  def assumeKernel[A](f: (A, A) => Double): Kernel[A] =
    new Kernel[A]:
      def evaluate(left: A, right: A): Double = f(left, right)

  def assumeStrongKernel[A](f: (A, A) => Double): StrongKernel[A] =
    new StrongKernel[A]:
      def evaluate(left: A, right: A): Double = f(left, right)

final case class FiniteBag[A](items: Vector[A])

final class HierarchyRealization[A, Node](
    val base: StrongKernel[A],
    val nodes: Vector[Node]
)

object OptimalAssignment:
  def apply[A](base: StrongKernel[A]): Kernel[FiniteBag[A]] =
    new Kernel[FiniteBag[A]]:
      def evaluate(left: FiniteBag[A], right: FiniteBag[A]): Double =
        val pairCount = math.min(left.items.size, right.items.size)
        var total = 0.0
        var index = 0
        while index < pairCount do
          total += base.evaluate(left.items(index), right.items(index))
          index += 1
        total

trait Decomposition[-A, P]:
  def fold[B](value: A, initial: B)(
      step: (B, P, NonNegativeFinite) => B
  ): B

final case class FeatureMatrix(rowCount: Int, columnCount: Int)
final case class AlignedFeatures(shared: FeatureMatrix, novelSquaredNorm: Vector[Double])
final case class KernelOperator(dimension: Int)
final case class FeatureUnavailable(reason: String)

final class PreparedKernel private (
    val operator: KernelOperator,
    val features: Either[FeatureUnavailable, FeatureMatrix]
)

object PreparedKernel:
  def pairwiseOnly(size: Int): PreparedKernel =
    new PreparedKernel(
      KernelOperator(size),
      Left(FeatureUnavailable("selected lowering has no exact finite feature map"))
    )

  def explicit(features: FeatureMatrix): PreparedKernel =
    new PreparedKernel(KernelOperator(features.rowCount), Right(features))

final class RefinementTrace:
  final class Colour private[RefinementTrace] (val index: Int)

  def colour(index: Int): Colour = new Colour(index)

  def queryOverlay: QueryOverlay[this.type] = QueryOverlay(this)

final case class QueryOverlay[T <: RefinementTrace & Singleton](owner: T)
