package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.*
import munit.FunSuite

final class WLRefinementSuite extends FunSuite:
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

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

  test("negative rounds are rejected"):
    val result =
      WLRefinement.create(
        -1,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs(None, None)
      )

    assertEquals(result, Left(WLRefinementError.NegativeRounds(-1)))

  test("named projections and codecs give the refinement durability"):
    val refinement =
      WLRefinement
        .create(
          3,
          Projection.named[Atom, String](
            StableId.from("molecule.atom.element").toOption.get,
            StableVersion.initial
          )(_.element),
          Projection.named[Bond, Int](
            StableId.from("molecule.bond.order").toOption.get,
            StableVersion.initial
          )(_.order),
          WLCodecs(Some(nodeCodec), Some(edgeCodec))
        )
        .toOption
        .get

    assertEquals(refinement.rounds, 3)
    assert(
      refinement.fingerprintability.isInstanceOf[
        Fingerprintability.Durable
      ]
    )

  test("reusing one refinement value preserves its plan identity"):
    val refinement =
      WLRefinement
        .create(
          1,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs(None, None)
        )
        .toOption
        .get

    assertEquals(
      refinement.planNodeId.number,
      refinement.planNodeId.number
    )
