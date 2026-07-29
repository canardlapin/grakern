package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

final class WLIdentityLawSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private given ExecutionContext = ExecutionContext.global

  private def specification(
      hashing: WLHashStrategy[String, Int] = WLHashStrategy.default[String, Int],
      rounds: Int = 3
  ) =
    WLRefinement
      .create(
        rounds,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None),
        hashing
      )
      .toOption
      .get

  private def path(
      key: String,
      left: Int,
      centre: Int,
      right: Int
  ): GraphSample[AtomId, Atom, Bond] =
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(left), AtomId(centre)),
          Link(AtomId(centre), AtomId(right))
        )
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = id => Atom(if id.value == centre then "O" else "C"),
      edgeValue = _ => Bond(1)
    )

  private def pair(
      key: String,
      left: Int,
      right: Int
  ): GraphSample[AtomId, Atom, Bond] =
    val topology =
      Graph
        .fromEdges(Link(AtomId(left), AtomId(right)))
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = _ => Atom("C"),
      edgeValue = _ => Bond(1)
    )

  private def carbonPath(
      key: String,
      left: Int,
      centre: Int,
      right: Int
  ): GraphSample[AtomId, Atom, Bond] =
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(left), AtomId(centre)),
          Link(AtomId(centre), AtomId(right))
        )
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = _ => Atom("C"),
      edgeValue = _ => Bond(1)
    )

  private def indexOf(
      trace: WLTrace[AtomId, String, Int],
      vertex: Int
  ): Int =
    (0 until trace.vertexCount).find { index =>
      trace.source(index).exists(_.vertex.value == vertex)
    }.get

  private def queryIndexOf(
      trace: WLQueryTrace[AtomId, String, Int],
      vertex: Int
  ): Int =
    (0 until trace.vertexCount).find { index =>
      trace.source(index).exists(_.vertex.value == vertex)
    }.get

  test("constant hashes preserve the exact recursive trace"):
    val sample = path("path", 1, 2, 3)
    val ordinary =
      WLTrace
        .prepare(Vector(sample), specification())
        .toOption
        .get
    val adversarial =
      WLTrace
        .prepare(
          Vector(sample),
          specification(
            WLHashStrategy.constant[String, Int](0)
          )
        )
        .toOption
        .get

    Vector(1, 2, 3).foreach { vertex =>
      val ordinaryIndex = indexOf(ordinary, vertex)
      val adversarialIndex = indexOf(adversarial, vertex)
      (0 to ordinary.rounds).foreach { level =>
        assert(
          ordinary.semanticallyEquivalent(
            ordinary.colour(level, ordinaryIndex).get,
            adversarial,
            adversarial.colour(level, adversarialIndex).get
          ),
          clues(vertex, level)
        )
      }
    }

  test("vertex reindexing preserves semantics but not trace-owned ids"):
    val refinement = specification()
    val first =
      WLTrace
        .prepare(Vector(path("first", 1, 2, 3)), refinement)
        .toOption
        .get
    val second =
      WLTrace
        .prepare(Vector(path("second", 30, 20, 10)), refinement)
        .toOption
        .get
    val correspondence = Vector(1 -> 30, 2 -> 20, 3 -> 10)

    correspondence.foreach { case (leftVertex, rightVertex) =>
      val leftIndex = indexOf(first, leftVertex)
      val rightIndex = indexOf(second, rightVertex)
      (0 to first.rounds).foreach { level =>
        val leftColour = first.colour(level, leftIndex).get
        val rightColour = second.colour(level, rightIndex).get
        assertNotEquals(leftColour, rightColour)
        assert(
          first.semanticallyEquivalent(
            leftColour,
            second,
            rightColour
          )
        )
      }
    }

  test("every parent chain follows one vertex through adjacent levels"):
    val trace =
      WLTrace
        .prepare(
          Vector(path("parents", 1, 2, 3)),
          specification(rounds = 4)
        )
        .toOption
        .get

    (0 until trace.vertexCount).foreach { vertexIndex =>
      (1 to trace.rounds).foreach { level =>
        val current = trace.colour(level, vertexIndex).get
        val expectedParent =
          trace.colour(level - 1, vertexIndex).get
        assertEquals(
          trace.signature(current).flatMap(_.parent),
          Some(expectedParent)
        )
      }
    }

  test("concurrent query overlays are immutable and semantically deterministic"):
    val refinement = specification(rounds = 2)
    val training =
      WLTrace
        .prepare(
          Vector(pair("training", 1, 2)),
          refinement
        )
        .toOption
        .get
    val trainingCount = training.signatureCount
    val querySample = path("query", 10, 20, 30)
    val traces =
      Await.result(
        Future.sequence(
          Vector.fill(16)(
            Future(
              WLQueryTrace
                .prepare(
                  training,
                  Vector(querySample),
                  refinement
                )
                .toOption
                .get
            )
          )
        ),
        20.seconds
      )
    val centreColours =
      traces.map { trace =>
        val centre = queryIndexOf(trace, 20)
        trace.colour(trace.rounds, centre).get
      }
    val firstTrace = traces.head
    val firstColour = centreColours.head

    assert(firstTrace.isNovel(firstColour))
    traces.tail.zip(centreColours.tail).foreach { case (trace, colour) =>
      assertNotEquals(firstColour, colour)
      assertEquals(firstColour.index, colour.index)
      assert(
        firstTrace.semanticallyEquivalent(
          firstColour,
          trace,
          colour
        )
      )
    }
    assertEquals(training.signatureCount, trainingCount)

  test("novel query descendants can share exact training ancestors"):
    val refinement = specification(rounds = 1)
    val training =
      WLTrace
        .prepare(
          Vector(pair("training", 1, 2)),
          refinement
        )
        .toOption
        .get
    val query =
      WLQueryTrace
        .prepare(
          training,
          Vector(carbonPath("query", 10, 20, 30)),
          refinement
        )
        .toOption
        .get
    val centre = queryIndexOf(query, 20)
    val known = query.colour(0, centre).get
    val novel = query.colour(1, centre).get

    assert(!query.isNovel(known))
    assert(query.isNovel(novel))
    assertEquals(query.signature(novel).flatMap(_.parent), Some(known))
