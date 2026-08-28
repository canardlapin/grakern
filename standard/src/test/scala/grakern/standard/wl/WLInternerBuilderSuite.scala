package grakern.standard.wl

import cats.kernel.Hash
import munit.FunSuite

final class WLInternerBuilderSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  test("mutable preparation builder resolves deliberate hash collisions exactly"):
    val builder =
      WLInternerBuilder.empty(
        WLCodecs[String, Int](None, None),
        WLHashStrategy.constant(7)
      )
    val carbon =
      builder.internInitial("C")
    val oxygen =
      builder.internInitial("O")
    val carbonAgain =
      builder.internInitial("C")

    assertEquals(carbonAgain, carbon)
    assertNotEquals(oxygen, carbon)

    val first =
      builder
        .internRefined(
          carbon,
          Vector(WLIncidence.undirected(1, oxygen))
        )
        .toOption
        .get
    val same =
      builder
        .internRefined(
          carbon,
          Vector(WLIncidence.undirected(1, oxygen))
        )
        .toOption
        .get
    val different =
      builder
        .internRefined(
          carbon,
          Vector(WLIncidence.undirected(2, oxygen))
        )
        .toOption
        .get

    assertEquals(same, first)
    assertNotEquals(different, first)

    val frozen = builder.freeze()
    assertEquals(
      frozen.signature(carbon).isDefined,
      true
    )
    assertEquals(
      frozen.signature(different).isDefined,
      true
    )
