package grakern.core

enum IdentityError:
  case EmptyStableId
  case NegativeVersion(value: Int)
  case EmptyCorpusFingerprint

opaque type StableId = String

object StableId:
  def from(value: String): Either[IdentityError, StableId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(IdentityError.EmptyStableId)
    else Right(normalized)

  extension (value: StableId) def text: String = value

opaque type StableVersion = Int

object StableVersion:
  val initial: StableVersion = 1

  def from(value: Int): Either[IdentityError, StableVersion] =
    if value < 0 then Left(IdentityError.NegativeVersion(value))
    else Right(value)

  extension (value: StableVersion) def number: Int = value

opaque type PlanNodeId = Long

object PlanNodeId:
  private var nextValue: Long = 0L

  private[grakern] def fresh(): PlanNodeId = synchronized {
    val result = nextValue
    nextValue += 1L
    result
  }

  extension (value: PlanNodeId) def number: Long = value

opaque type SemanticFingerprint = String

object SemanticFingerprint:
  private[grakern] def compose(
      schema: String,
      atoms: Vector[String],
      children: Vector[SemanticFingerprint]
  ): SemanticFingerprint =
    encode(Vector(schema) ++ atoms ++ children.map(_.value))

  extension (value: SemanticFingerprint) def value: String = value

  private def encode(parts: Vector[String]): String =
    parts.iterator.map(part => s"${part.length}:$part").mkString

opaque type CorpusContentFingerprint = String

object CorpusContentFingerprint:
  def from(value: String): Either[IdentityError, CorpusContentFingerprint] =
    val normalized = value.trim
    if normalized.isEmpty then Left(IdentityError.EmptyCorpusFingerprint)
    else Right(normalized)

  extension (value: CorpusContentFingerprint) def value: String = value

opaque type ArtifactFingerprint = String

object ArtifactFingerprint:
  def from(input: ArtifactFingerprintInput): ArtifactFingerprint =
    val approximation =
      input.approximation.fold(Vector("exact")) { descriptor =>
        Vector(
          descriptor.method.text,
          descriptor.seed.toString
        ) ++ descriptor.parameters.flatMap { case (key, value) =>
          Vector(key.text, value)
        }
      }
    SemanticFingerprint
      .compose(
        schema = "grakern.artifact.v1",
        atoms = Vector(
          input.semantic.value,
          input.corpus.value,
          input.compilerSchema.text,
          input.loweringSchema.text,
          input.numericalPolicy.text,
          input.engineVersion.text
        ) ++ input.orderedSamples.map(_.value) ++ approximation,
        children = Vector.empty
      )
      .value

  extension (value: ArtifactFingerprint) def value: String = value

final case class ApproximationDescriptor(
    method: StableId,
    seed: Long,
    parameters: Vector[(StableId, String)]
)

final case class ArtifactFingerprintInput(
    semantic: SemanticFingerprint,
    corpus: CorpusContentFingerprint,
    orderedSamples: Vector[SampleKey],
    compilerSchema: StableId,
    loweringSchema: StableId,
    numericalPolicy: StableId,
    approximation: Option[ApproximationDescriptor],
    engineVersion: StableId
)

enum FingerprintBlocker:
  case AnonymousProjection(node: PlanNodeId)
  case MissingCodec(node: PlanNodeId, role: String)
  case UnsafeAssumption(node: PlanNodeId, claim: String)

final case class FingerprintBlockers private (
    head: FingerprintBlocker,
    tail: Vector[FingerprintBlocker]
):
  def toVector: Vector[FingerprintBlocker] = head +: tail

object FingerprintBlockers:
  def one(blocker: FingerprintBlocker): FingerprintBlockers =
    FingerprintBlockers(blocker, Vector.empty)

  def fromVector(
      blockers: Vector[FingerprintBlocker]
  ): Option[FingerprintBlockers] =
    blockers.headOption.map(head => FingerprintBlockers(head, blockers.tail))

enum Fingerprintability:
  case Durable(fingerprint: SemanticFingerprint)
  case Ephemeral(blockers: FingerprintBlockers)

object Fingerprintability:
  private[grakern] def combine(
      schema: String,
      atoms: Vector[String],
      children: Vector[Fingerprintability]
  ): Fingerprintability =
    val blockers = children.flatMap {
      case Fingerprintability.Durable(_)          => Vector.empty
      case Fingerprintability.Ephemeral(problems) => problems.toVector
    }
    FingerprintBlockers.fromVector(blockers) match
      case Some(problems) => Fingerprintability.Ephemeral(problems)
      case None           =>
        val fingerprints = children.collect { case Fingerprintability.Durable(value) =>
          value
        }
        Fingerprintability.Durable(
          SemanticFingerprint.compose(schema, atoms, fingerprints)
        )
