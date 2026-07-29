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
        Vector(new WLNeighbour(2, neighbour)),
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
        Vector(1 -> carbon, 2 -> oxygen),
        Some(edgeCodec)
      )
    val reversed =
      WLDescriptor.refined(
        1,
        carbon,
        Vector(2 -> oxygen, 1 -> carbon),
        Some(edgeCodec)
      )

    assertEquals(
      forward.map(_.value),
      reversed.map(_.value)
    )

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
