package grakern.core

import cats.kernel.{Eq, Hash}
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

final class NeighbourhoodSuite extends ScalaCheckSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private val key = SampleKey("n")

  test("undirected builder emits one incidence per endpoint and one for a self-loop"):
    val sample =
      NeighbourhoodSample
        .undirected(
          key,
          Vector(1, 2, 3),
          _ => "v",
          Vector((1, 2, "a"), (2, 3, "b"), (3, 3, "loop"))
        )
        .toOption
        .get
    assertEquals(sample.incidences(1).map(_.other), Vector(2))
    assertEquals(sample.incidences(2).map(_.other), Vector(1, 3))
    assertEquals(sample.incidences(3).map(_.other), Vector(2, 3))
    assert(sample.incidences(3).forall(_.orientation == Orientation.Undirected))
    assertEquals(Neighbourhoods.canonicalEdges(sample).size, 3)

  test("directed builder pairs an Outgoing at the source with an Incoming at the target"):
    val sample =
      NeighbourhoodSample
        .directed(key, Vector(1, 2), _ => "v", Vector((1, 2, "arg0"), (1, 2, "arg1")))
        .toOption
        .get
    assertEquals(
      sample.incidences(1).map(_.orientation),
      Vector(Orientation.Outgoing, Orientation.Outgoing)
    )
    assertEquals(
      sample.incidences(2).map(_.orientation),
      Vector(Orientation.Incoming, Orientation.Incoming)
    )
    val edges = Neighbourhoods.canonicalEdges(sample)
    assertEquals(edges.map(_.value), Vector("arg0", "arg1"))
    assertEquals(edges.map(_.ref.ordinal), Vector(0, 1))
    assert(edges.forall(_.ref.orientation == Orientation.Outgoing))

  test("parallel undirected edges are kept with multiplicity"):
    val sample =
      NeighbourhoodSample
        .undirected(key, Vector(1, 2), _ => "v", Vector((1, 2, "a"), (1, 2, "a"), (2, 1, "b")))
        .toOption
        .get
    assertEquals(sample.incidences(1).size, 3)
    assertEquals(sample.incidences(2).size, 3)
    assertEquals(Neighbourhoods.canonicalEdges(sample).map(_.value).sorted, Vector("a", "a", "b"))

  test("duplicate vertices are rejected"):
    val result =
      NeighbourhoodSample.undirected(key, Vector(1, 1), _ => "v", Vector.empty[(Int, Int, String)])
    assertEquals(result, Left(NeighbourhoodError.DuplicateVertex(key, 1)))

  test("unknown endpoints are rejected"):
    val result =
      NeighbourhoodSample.build[Int, String, String](
        key,
        Vector(1),
        _ => "v",
        _ => Vector(Incidence(9, "x", Orientation.Undirected))
      )
    assertEquals(result, Left(NeighbourhoodError.UnknownEndpoint(key, 1, 9)))

  test("asymmetric undirected incidences are rejected"):
    val result =
      NeighbourhoodSample.build[Int, String, String](
        key,
        Vector(1, 2),
        _ => "v",
        v => if v == 1 then Vector(Incidence(2, "x", Orientation.Undirected)) else Vector.empty
      )
    assertEquals(
      result,
      Left(NeighbourhoodError.UnmatchedIncidence(key, 1, 2, Orientation.Undirected))
    )

  test("an Outgoing incidence without its Incoming partner is rejected"):
    val result =
      NeighbourhoodSample.build[Int, String, String](
        key,
        Vector(1, 2),
        _ => "v",
        v => if v == 1 then Vector(Incidence(2, "x", Orientation.Outgoing)) else Vector.empty
      )
    assertEquals(
      result,
      Left(NeighbourhoodError.UnmatchedIncidence(key, 1, 2, Orientation.Outgoing))
    )

  test("value mismatch across the two appearances of an edge is rejected"):
    val result =
      NeighbourhoodSample.build[Int, String, String](
        key,
        Vector(1, 2),
        _ => "v",
        v =>
          if v == 1 then Vector(Incidence(2, "x", Orientation.Undirected))
          else Vector(Incidence(1, "y", Orientation.Undirected))
      )
    assert(result.isLeft)

  private val genEdges: Gen[Vector[(Int, Int, String)]] =
    for
      n <- Gen.choose(1, 6)
      m <- Gen.choose(0, 10)
      edges <- Gen.listOfN(
        m,
        for
          a <- Gen.choose(0, n - 1)
          b <- Gen.choose(0, n - 1)
          v <- Gen.oneOf("a", "b", "c")
        yield (a, b, v)
      )
    yield edges.toVector

  private given Arbitrary[Vector[(Int, Int, String)]] = Arbitrary(genEdges)

  property("AdapterLaw: undirected incidences are symmetric with multiplicity"):
    forAll { (edges: Vector[(Int, Int, String)]) =>
      val n = (edges.flatMap(e => Vector(e._1, e._2)) :+ 0).max + 1
      val sample =
        NeighbourhoodSample.undirected(key, (0 until n).toVector, _ => "v", edges).toOption.get
      (0 until n).forall { v =>
        sample.incidences(v).forall { inc =>
          val back = sample.incidences(inc.other).count(i => i.other == v && i.value == inc.value)
          val forward =
            sample.incidences(v).count(i => i.other == inc.other && i.value == inc.value)
          if inc.other == v then true else back == forward
        }
      }
    }

  property("AdapterLaw: Outgoing at v iff Incoming at other, with multiplicity"):
    forAll { (arcs: Vector[(Int, Int, String)]) =>
      val n = (arcs.flatMap(e => Vector(e._1, e._2)) :+ 0).max + 1
      val sample =
        NeighbourhoodSample.directed(key, (0 until n).toVector, _ => "v", arcs).toOption.get
      (0 until n).forall { v =>
        sample.incidences(v).filter(_.orientation == Orientation.Outgoing).forall { inc =>
          val out = sample
            .incidences(v)
            .count(i =>
              i.orientation == Orientation.Outgoing && i.other == inc.other && i.value == inc.value
            )
          val in = sample
            .incidences(inc.other)
            .count(i =>
              i.orientation == Orientation.Incoming && i.other == v && i.value == inc.value
            )
          out == in
        }
      }
    }

  property("canonicalEdges enumerates every physical edge exactly once"):
    forAll { (edges: Vector[(Int, Int, String)]) =>
      val n = (edges.flatMap(e => Vector(e._1, e._2)) :+ 0).max + 1
      val undirected =
        NeighbourhoodSample.undirected(key, (0 until n).toVector, _ => "v", edges).toOption.get
      val directed =
        NeighbourhoodSample.directed(key, (0 until n).toVector, _ => "v", edges).toOption.get
      Neighbourhoods.canonicalEdges(undirected).size == edges.size &&
      Neighbourhoods.canonicalEdges(directed).size == edges.size
    }
