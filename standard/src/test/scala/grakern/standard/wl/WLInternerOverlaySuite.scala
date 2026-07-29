package grakern.standard.wl

import cats.kernel.Hash
import munit.FunSuite

final class WLInternerOverlaySuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val noCodecs =
    WLCodecs[String, Int](None, None)

  private def training: (WLInterner[String, Int], WLColour, WLColour, WLColour) =
    val empty = WLInterner.empty(noCodecs)
    val (withCarbon, carbon) = empty.internInitial("C")
    val (withOxygen, oxygen) = withCarbon.internInitial("O")
    val (complete, refined) =
      withOxygen
        .internRefined(carbon, Vector(1 -> oxygen))
        .toOption
        .get
    (complete, carbon, oxygen, refined)

  test("known initial and refined signatures reuse training colours"):
    val (base, carbon, oxygen, refined) = training
    val empty = WLInternerOverlay(base)
    val (withCarbon, queryCarbon) = empty.internInitial("C")
    val (withOxygen, queryOxygen) = withCarbon.internInitial("O")
    val (complete, queryRefined) =
      withOxygen
        .internRefined(queryCarbon, Vector(1 -> queryOxygen))
        .toOption
        .get

    assertEquals(queryCarbon, carbon)
    assertEquals(queryOxygen, oxygen)
    assertEquals(queryRefined, refined)
    assertEquals(complete.novelSignatureCount, 0)

  test("a novel descendant can retain a known training parent"):
    val (base, carbon, oxygen, _) = training
    val empty = WLInternerOverlay(base)
    val (complete, novel) =
      empty
        .internRefined(
          carbon,
          Vector(1 -> oxygen, 1 -> oxygen)
        )
        .toOption
        .get

    assert(complete.isNovel(novel))
    assertEquals(
      complete.signature(novel).flatMap(_.parent),
      Some(carbon)
    )
    assertEquals(base.size, 3)

  test("concurrent roots allocate distinct but equivalent novel colours"):
    val (base, carbon, oxygen, _) = training
    val firstStart = WLInternerOverlay(base)
    val secondStart = WLInternerOverlay(base)
    val (first, firstNovel) =
      firstStart
        .internRefined(carbon, Vector(2 -> oxygen))
        .toOption
        .get
    val (second, secondNovel) =
      secondStart
        .internRefined(carbon, Vector(2 -> oxygen))
        .toOption
        .get

    assertNotEquals(firstNovel, secondNovel)
    assertEquals(firstNovel.index, secondNovel.index)
    assert(
      first.semanticallyEquivalent(firstNovel, second, secondNovel)
    )
    assertEquals(base.size, 3)

  test("constant hashes still resolve novel overlay signatures exactly"):
    val empty =
      WLInterner.empty(
        noCodecs,
        WLHashStrategy.constant[String, Int](0)
      )
    val (base, _) = empty.internInitial("C")
    val start = WLInternerOverlay(base)
    val (first, oxygen) = start.internInitial("O")
    val (second, nitrogen) = first.internInitial("N")

    assertNotEquals(oxygen, nitrogen)
    assertEquals(second.novelSignatureCount, 2)
    assertEquals(second.signature(oxygen).map(_.level), Some(0))
    assertEquals(second.signature(nitrogen).map(_.level), Some(0))
