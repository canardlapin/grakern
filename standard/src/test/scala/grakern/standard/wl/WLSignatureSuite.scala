package grakern.standard.wl

import grakern.core.*
import munit.FunSuite

final class WLSignatureSuite extends FunSuite:
  private val nodeCodec =
    StableCodec.named[String](
      StableId.from("molecule.element").toOption.get,
      StableVersion.initial
    )(identity)
  private val edgeCodec =
    StableCodec.named[Int](
      StableId.from("molecule.bond-order").toOption.get,
      StableVersion.initial
    )(_.toString)

  private def durable(
      value: Fingerprintability
  ): SemanticFingerprint =
    value match
      case Fingerprintability.Durable(fingerprint) => fingerprint
      case other => fail(s"expected a durable fingerprint, observed $other")

  test("colour equality is scoped by owning trace"):
    val firstOwner = new ColourOwner
    val secondOwner = new ColourOwner
    val first = WLColour.create(firstOwner, 0, 0)
    val same = WLColour.create(firstOwner, 0, 0)
    val other = WLColour.create(secondOwner, 0, 0)

    assertEquals(first, same)
    assertNotEquals(first, other)
    assert(first.sameTrace(same))
    assert(!first.sameTrace(other))

  test("refined signatures retain their semantic parent and edge-aware bag"):
    val owner = new ColourOwner
    val parent = WLColour.create(owner, 0, 0)
    val neighbour = WLColour.create(owner, 1, 0)
    val refinedColour = WLColour.create(owner, 2, 1)
    val signature =
      WLSignature.refined[String, Int](
        refinedColour,
        parent,
        Vector(new WLNeighbour(2, Orientation.Undirected, neighbour)),
        None
      )

    assertEquals(signature.level, 1)
    assertEquals(signature.parent, Some(parent))
    val refined = signature.asInstanceOf[WLSignature.Refined[String, Int]]
    assertEquals(refined.neighbourhood.head.edgeKey, 2)
    assertEquals(refined.neighbourhood.head.colour, neighbour)

  test("stable refined descriptors are invariant to neighbour order"):
    val carbon =
      WLDescriptor.initial("C", Some(nodeCodec))
    val oxygen =
      WLDescriptor.initial("O", Some(nodeCodec))
    val forward =
      WLDescriptor.refined(
        1,
        carbon,
        Vector((1, Orientation.Undirected, carbon), (2, Orientation.Undirected, oxygen)),
        Some(edgeCodec)
      )
    val reversed =
      WLDescriptor.refined(
        1,
        carbon,
        Vector((2, Orientation.Undirected, oxygen), (1, Orientation.Undirected, carbon)),
        Some(edgeCodec)
      )

    assertEquals(
      forward.map(_.value),
      reversed.map(_.value)
    )

  test("orientation changes refined descriptors and v2 is incompatible with v1"):
    val carbon = WLDescriptor.initial("C", Some(nodeCodec))
    val oxygen = WLDescriptor.initial("O", Some(nodeCodec))
    val outgoing =
      WLDescriptor
        .refined(
          1,
          carbon,
          Vector((1, Orientation.Outgoing, oxygen)),
          Some(edgeCodec)
        )
        .get
    val incoming =
      WLDescriptor
        .refined(
          1,
          carbon,
          Vector((1, Orientation.Incoming, oxygen)),
          Some(edgeCodec)
        )
        .get
    val undirected =
      WLDescriptor
        .refined(
          1,
          carbon,
          Vector((1, Orientation.Undirected, oxygen)),
          Some(edgeCodec)
        )
        .get
    val legacyNeighbour =
      SemanticFingerprint.compose(
        schema = "grakern.wl-signature.neighbour.v1",
        atoms = Vector(
          edgeCodec.id.text,
          edgeCodec.version.number.toString,
          edgeCodec.encode(1)
        ),
        children = Vector(oxygen.get)
      )
    val legacy =
      SemanticFingerprint.compose(
        schema = "grakern.wl-signature.refined.v1",
        atoms = Vector("1"),
        children = Vector(carbon.get, legacyNeighbour)
      )

    assertNotEquals(outgoing.value, incoming.value)
    assertNotEquals(outgoing.value, undirected.value)
    assertNotEquals(incoming.value, undirected.value)
    assertNotEquals(undirected.value, legacy.value)

  test("WL codec and refinement schemas reject undirected-era identities"):
    val codecs = WLCodecs[String, Int](Some(nodeCodec), Some(edgeCodec))
    val currentCodecs = durable(codecs.fingerprintability(PlanNodeId.fresh()))
    val legacyCodecs =
      SemanticFingerprint.compose(
        schema = "grakern.wl-codecs.v1",
        atoms = Vector(
          nodeCodec.id.text,
          nodeCodec.version.number.toString,
          edgeCodec.id.text,
          edgeCodec.version.number.toString
        ),
        children = Vector.empty
      )
    val nodeProjection =
      Projection.named[String, String](
        StableId.from("molecule.node-key").toOption.get,
        StableVersion.initial
      )(identity)
    val edgeProjection =
      Projection.named[Int, Int](
        StableId.from("molecule.edge-key").toOption.get,
        StableVersion.initial
      )(identity)
    val refinement =
      WLRefinement
        .create(1, nodeProjection, edgeProjection, codecs)
        .toOption
        .get
    val legacyRefinement =
      SemanticFingerprint.compose(
        schema = "grakern.wl-refinement.v1",
        atoms = Vector("1"),
        children = Vector(
          durable(nodeProjection.fingerprintAt(refinement.planNodeId)),
          durable(edgeProjection.fingerprintAt(refinement.planNodeId)),
          legacyCodecs
        )
      )

    assertNotEquals(currentCodecs.value, legacyCodecs.value)
    assertNotEquals(durable(refinement.fingerprintability).value, legacyRefinement.value)

  test("missing codecs make the WL identity explicitly ephemeral"):
    val codecs = WLCodecs[String, Int](Some(nodeCodec), None)
    val identity = codecs.fingerprintability(PlanNodeId.fresh())

    assert(
      identity match
        case Fingerprintability.Ephemeral(blockers) =>
          blockers.toVector.exists {
            case FingerprintBlocker.MissingCodec(_, "WL edge key") => true
            case _                                                 => false
          }
        case Fingerprintability.Durable(_) =>
          false
    )

  test("source provenance keeps graph-local identity with the signature"):
    val owner = new ColourOwner
    val colour = WLColour.create(owner, 3, 2)
    val occurrence =
      WLSignatureOccurrence(
        WLSource(SampleKey("molecule-7"), 42),
        level = 2,
        colour
      )

    assertEquals(occurrence.source.sample, SampleKey("molecule-7"))
    assertEquals(occurrence.source.vertex, 42)
    assertEquals(occurrence.level, 2)
