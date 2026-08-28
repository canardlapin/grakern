package grakern.standard.wl

import cats.kernel.Hash
import munit.FunSuite

final class WLInternerSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val noCodecs =
    WLCodecs[String, Int](None, None)

  test("equal initial signatures reuse one trace-local colour"):
    val empty = WLInterner.empty(noCodecs)
    val (firstState, first) = empty.internInitial("C")
    val (secondState, second) = firstState.internInitial("C")

    assertEquals(first, second)
    assertEquals(firstState.size, 1)
    assertEquals(secondState.size, 1)

  test("refined equality treats the neighbourhood as an exact multiset"):
    val empty = WLInterner.empty(noCodecs)
    val (withCarbon, carbon) = empty.internInitial("C")
    val (withOxygen, oxygen) = withCarbon.internInitial("O")
    val (firstState, first) =
      withOxygen
        .internRefined(
          carbon,
          Vector(
            WLIncidence.undirected(1, carbon),
            WLIncidence.undirected(2, oxygen),
            WLIncidence.undirected(1, carbon)
          )
        )
        .toOption
        .get
    val (secondState, second) =
      firstState
        .internRefined(
          carbon,
          Vector(
            WLIncidence.undirected(1, carbon),
            WLIncidence.undirected(1, carbon),
            WLIncidence.undirected(2, oxygen)
          )
        )
        .toOption
        .get

    assertEquals(first, second)
    assertEquals(secondState.size, firstState.size)

  test("a deliberately constant hash never becomes semantic identity"):
    val empty =
      WLInterner.empty(
        noCodecs,
        WLHashStrategy.constant[String, Int](0)
      )
    val (firstState, carbon) = empty.internInitial("C")
    val (secondState, oxygen) = firstState.internInitial("O")
    val (thirdState, carbonRefined) =
      secondState
        .internRefined(carbon, Vector(WLIncidence.undirected(1, oxygen)))
        .toOption
        .get
    val (fourthState, oxygenRefined) =
      thirdState
        .internRefined(oxygen, Vector(WLIncidence.undirected(1, carbon)))
        .toOption
        .get

    assertNotEquals(carbon, oxygen)
    assertNotEquals(carbonRefined, oxygenRefined)
    assertEquals(fourthState.size, 4)

  test("colours from another trace are rejected"):
    val first = WLInterner.empty(noCodecs)
    val second = WLInterner.empty(noCodecs)
    val (firstState, firstColour) = first.internInitial("C")
    val (_, secondColour) = second.internInitial("C")

    assert(
      firstState
        .internRefined(firstColour, Vector(WLIncidence.undirected(1, secondColour)))
        .left
        .exists {
          case WLInternerError.ForeignColour(colour) =>
            colour.equals(secondColour)
          case _ =>
            false
        }
    )

  test("independent traces compare by recursive semantics rather than ids"):
    val firstEmpty = WLInterner.empty(noCodecs)
    val (firstA, carbonA) = firstEmpty.internInitial("C")
    val (firstB, oxygenA) = firstA.internInitial("O")
    val (firstFinal, refinedA) =
      firstB
        .internRefined(carbonA, Vector(WLIncidence.undirected(1, oxygenA)))
        .toOption
        .get

    val secondEmpty = WLInterner.empty(noCodecs)
    val (secondA, oxygenB) = secondEmpty.internInitial("O")
    val (secondB, carbonB) = secondA.internInitial("C")
    val (secondC, _) = secondB.internInitial("unused")
    val (secondFinal, refinedB) =
      secondC
        .internRefined(carbonB, Vector(WLIncidence.undirected(1, oxygenB)))
        .toOption
        .get

    assertNotEquals(refinedA.index, refinedB.index)
    assert(
      firstFinal.semanticallyEquivalent(
        refinedA,
        secondFinal,
        refinedB
      )
    )
