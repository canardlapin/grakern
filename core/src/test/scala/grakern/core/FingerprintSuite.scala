package grakern.core

import cats.kernel.Eq
import munit.FunSuite

final class FingerprintSuite extends FunSuite:
  given Eq[String] = Eq.fromUniversalEquals

  test("anonymous projection is executable but blocks durability"):
    val projection = Projection.of[String, Int](_.length)
    val kernel = Kernel.delta[Int].on(projection)

    assertEquals(kernel.evaluate("aa", "bb"), 1.0)
    val blockers = kernel.fingerprintability match
      case Fingerprintability.Durable(_)          => Vector.empty
      case Fingerprintability.Ephemeral(problems) => problems.toVector
    assert(blockers.exists(_.isInstanceOf[FingerprintBlocker.AnonymousProjection]))

  test("named projection and named equality compose a durable fingerprint"):
    val equalityId = StableId.from("scala.string.eq").toOption.get
    val projectionId = StableId.from("molecule.atom.element").toOption.get
    val projection =
      Projection.named[String, String](projectionId, StableVersion.initial)(
        identity
      )
    val kernel = Kernel.deltaNamed[String](equalityId).on(projection)

    assert(kernel.fingerprintability.isInstanceOf[Fingerprintability.Durable])

  test("ordinary delta remains safe but reports its missing equality codec"):
    val kernel = Kernel.delta[String]

    val blockers = kernel.fingerprintability match
      case Fingerprintability.Durable(_)          => Vector.empty
      case Fingerprintability.Ephemeral(problems) => problems.toVector
    assert(
      blockers.exists {
        case FingerprintBlocker.MissingCodec(_, "delta equality") => true
        case _                                                    => false
      }
    )

  test("artifact identity includes corpus content and ordered samples"):
    val equalityId = StableId.from("scala.string.eq").toOption.get
    val semantic =
      Kernel.deltaNamed[String](equalityId).fingerprintability match
        case Fingerprintability.Durable(value) => value
        case Fingerprintability.Ephemeral(_)   => fail("expected durability")
    val corpus = CorpusContentFingerprint.from("sha256:abc").toOption.get
    val schema = StableId.from("schema-v1").toOption.get
    val base =
      ArtifactFingerprintInput(
        semantic,
        corpus,
        Vector(SampleKey("a"), SampleKey("b")),
        schema,
        schema,
        schema,
        None,
        schema
      )
    val reordered = base.copy(orderedSamples = base.orderedSamples.reverse)

    assertNotEquals(
      ArtifactFingerprint.from(base).value,
      ArtifactFingerprint.from(reordered).value
    )
