package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLPlanSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val nodeProjectionId =
    StableId.from("molecule.atom.element").toOption.get
  private val edgeProjectionId =
    StableId.from("molecule.bond.order").toOption.get
  private val nodeCodecId =
    StableId.from("molecule.element").toOption.get
  private val edgeCodecId =
    StableId.from("molecule.bond-order").toOption.get

  private def sample(key: String) =
    val topology =
      Graph
        .fromEdges(Link(AtomId(1), AtomId(2)))
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = _ => Atom("C"),
      edgeValue = _ => Bond(1)
    )

  private def durableRefinement =
    WLRefinement
      .create(
        1,
        Projection.named[Atom, String](
          nodeProjectionId,
          StableVersion.initial
        )(_.element),
        Projection.named[Bond, Int](
          edgeProjectionId,
          StableVersion.initial
        )(_.order),
        WLCodecs(
          Some(
            StableCodec.named[String](
              nodeCodecId,
              StableVersion.initial
            )(identity)
          ),
          Some(
            StableCodec.named[Int](
              edgeCodecId,
              StableVersion.initial
            )(_.toString)
          )
        )
      )
      .toOption
      .get

  test("a durable semantic kernel still requires corpus content identity"):
    val plan =
      WLCompiler.plan(
        Vector(sample("a")),
        durableRefinement.subtree
      )

    assert(
      plan.semanticFingerprint
        .isInstanceOf[Fingerprintability.Durable]
    )
    plan.artifactFingerprint match
      case ArtifactFingerprintability.Ephemeral(blockers) =>
        assertEquals(
          blockers.toVector,
          Vector(
            ArtifactFingerprintBlocker.MissingCorpusContentFingerprint
          )
        )
      case ArtifactFingerprintability.Durable(_) =>
        fail("the corpus content was not identified")

  test("durable artifacts include corpus content and sample ordering"):
    val corpus =
      CorpusContentFingerprint.from("sha256:corpus").toOption.get
    val kernel = durableRefinement.subtree
    val forward =
      WLCompiler.plan(
        Vector(sample("a"), sample("b")),
        kernel,
        WLPlanOptions(corpusContent = Some(corpus))
      )
    val reverse =
      WLCompiler.plan(
        Vector(sample("b"), sample("a")),
        kernel,
        WLPlanOptions(corpusContent = Some(corpus))
      )

    val forwardValue = forward.artifactFingerprint match
      case ArtifactFingerprintability.Durable(value) =>
        value.value
      case ArtifactFingerprintability.Ephemeral(_) =>
        fail("expected a durable artifact")
    val reverseValue = reverse.artifactFingerprint match
      case ArtifactFingerprintability.Durable(value) =>
        value.value
      case ArtifactFingerprintability.Ephemeral(_) =>
        fail("expected a durable artifact")
    assertNotEquals(forwardValue, reverseValue)

  test("artifact identity includes the diagonal tolerance"):
    val corpus =
      CorpusContentFingerprint.from("sha256:corpus").toOption.get
    val kernel = durableRefinement.subtree.normalized
    val exact =
      WLCompiler.plan(
        Vector(sample("a")),
        kernel,
        WLPlanOptions(
          corpusContent = Some(corpus),
          diagonalTolerance = NonNegativeFinite.zero
        )
      )
    val tolerant =
      WLCompiler.plan(
        Vector(sample("a")),
        kernel,
        WLPlanOptions(
          corpusContent = Some(corpus),
          diagonalTolerance = NonNegativeFinite.from(1e-12).toOption.get
        )
      )

    val exactValue = exact.artifactFingerprint match
      case ArtifactFingerprintability.Durable(value) =>
        value.value
      case ArtifactFingerprintability.Ephemeral(_) =>
        fail("expected a durable exact artifact")
    val tolerantValue = tolerant.artifactFingerprint match
      case ArtifactFingerprintability.Durable(value) =>
        value.value
      case ArtifactFingerprintability.Ephemeral(_) =>
        fail("expected a durable tolerant artifact")
    assertNotEquals(exactValue, tolerantValue)

  test("anonymous projections identify the exact refinement blocker"):
    val refinement =
      WLRefinement
        .create(
          1,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    val corpus =
      CorpusContentFingerprint.from("sha256:corpus").toOption.get
    val plan =
      WLCompiler.plan(
        Vector(sample("a")),
        refinement.subtree,
        WLPlanOptions(corpusContent = Some(corpus))
      )

    plan.artifactFingerprint match
      case ArtifactFingerprintability.Ephemeral(blockers) =>
        val values = blockers.toVector
        assert(
          values.contains(
            ArtifactFingerprintBlocker.Semantic(
              FingerprintBlocker.AnonymousProjection(
                refinement.planNodeId
              )
            )
          )
        )
      case ArtifactFingerprintability.Durable(_) =>
        fail("anonymous projections must block artifact durability")
