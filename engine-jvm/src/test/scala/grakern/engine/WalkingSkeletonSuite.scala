package grakern.engine

import cats.Hash
import cats.kernel.Eq
import grakern.core.*
import grakern.graph4s.GraphSample
import graph4s.{Graph, Link}
import munit.FunSuite

final class WalkingSkeletonSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private val equalityId =
    StableId.from("scala.string.eq").toOption.get
  private val labelCodec =
    StableId.from("molecule.element.v1").toOption.get
  private val corpus =
    CorpusContentFingerprint.from("sha256:fixture-a").toOption.get

  private def samples: Vector[GraphSample[AtomId, Atom, Bond]] =
    val carbonPair =
      Graph.fromEdges(Link(AtomId(1), AtomId(2))).toOption.get
    val oxygen =
      Graph.singleton(AtomId(1))
    Vector(
      GraphSample.build(SampleKey("carbon-pair"), carbonPair)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      ),
      GraphSample.build(SampleKey("oxygen"), oxygen)(
        nodeValue = _ => Atom("O"),
        edgeValue = _ => Bond(1)
      )
    )

  test("GraphSample packs into an exact semantic Gram matrix"):
    val projection =
      Projection.named[Atom, String](
        StableId.from("molecule.atom.element").toOption.get,
        StableVersion.initial
      )(_.element)
    val prepared =
      WalkingSkeleton
        .prepare(
          samples,
          projection,
          equalityId,
          labelCodec,
          corpus
        )
        .toOption
        .get

    assertEquals(
      prepared.samples,
      Vector(SampleKey("carbon-pair"), SampleKey("oxygen"))
    )
    assertEquals(
      prepared.gram,
      Vector(Vector(4.0, 0.0), Vector(0.0, 1.0))
    )
    assert(prepared.semantic.isInstanceOf[Fingerprintability.Durable])
    assert(prepared.artifact.isRight)

  test("anonymous projections execute but reject persistent artifacts"):
    val prepared =
      WalkingSkeleton
        .prepare(
          samples,
          Projection.of[Atom, String](_.element),
          equalityId,
          labelCodec,
          corpus
        )
        .toOption
        .get

    assertEquals(
      prepared.gram,
      Vector(Vector(4.0, 0.0), Vector(0.0, 1.0))
    )
    assert(
      prepared.artifact.left.exists(
        _.toVector.exists {
          case FingerprintBlocker.AnonymousProjection(_) => true
          case _                                         => false
        }
      )
    )

  test("vertex reindexing leaves the exact Gram matrix unchanged"):
    val reindexedTopology =
      Graph.fromEdges(Link(AtomId(20), AtomId(10))).toOption.get
    val reindexed =
      GraphSample.build(SampleKey("carbon-pair"), reindexedTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val projection =
      Projection.named[Atom, String](
        StableId.from("molecule.atom.element").toOption.get,
        StableVersion.initial
      )(_.element)
    val original =
      WalkingSkeleton
        .prepare(samples, projection, equalityId, labelCodec, corpus)
        .toOption
        .get
    val changed =
      WalkingSkeleton
        .prepare(
          reindexed +: samples.tail,
          projection,
          equalityId,
          labelCodec,
          corpus
        )
        .toOption
        .get

    assertEquals(changed.gram, original.gram)

  test("artifact identity records sample ordering separately from content"):
    val projection =
      Projection.named[Atom, String](
        StableId.from("molecule.atom.element").toOption.get,
        StableVersion.initial
      )(_.element)
    val forward =
      WalkingSkeleton
        .prepare(samples, projection, equalityId, labelCodec, corpus)
        .toOption
        .get
    val reversed =
      WalkingSkeleton
        .prepare(
          samples.reverse,
          projection,
          equalityId,
          labelCodec,
          corpus
        )
        .toOption
        .get

    assertNotEquals(
      forward.artifact.toOption.map(_.value),
      reversed.artifact.toOption.map(_.value)
    )
