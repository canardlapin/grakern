package grakern.core

sealed trait Projection[-A, +B]:
  def apply(value: A): B

  private[grakern] def fingerprintAt(node: PlanNodeId): Fingerprintability

sealed trait DurableProjection[-A, +B] extends Projection[A, B]:
  def id: StableId
  def version: StableVersion

  final private[grakern] def fingerprintAt(
      node: PlanNodeId
  ): Fingerprintability =
    Fingerprintability.Durable(
      SemanticFingerprint.compose(
        schema = "grakern.projection.v1",
        atoms = Vector(id.text, version.number.toString),
        children = Vector.empty
      )
    )

object Projection:
  def of[A, B](f: A => B): Projection[A, B] =
    new Projection[A, B]:
      def apply(value: A): B = f(value)

      private[grakern] def fingerprintAt(
          node: PlanNodeId
      ): Fingerprintability =
        Fingerprintability.Ephemeral(
          FingerprintBlockers.one(
            FingerprintBlocker.AnonymousProjection(node)
          )
        )

  def named[A, B](
      stableId: StableId,
      stableVersion: StableVersion
  )(f: A => B): DurableProjection[A, B] =
    new DurableProjection[A, B]:
      val id: StableId = stableId
      val version: StableVersion = stableVersion
      def apply(value: A): B = f(value)
