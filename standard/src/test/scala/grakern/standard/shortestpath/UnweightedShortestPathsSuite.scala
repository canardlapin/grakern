package grakern.standard.shortestpath

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import graph4s.{Graph, Link}
import munit.FunSuite

final class UnweightedShortestPathsSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode

  private def sample(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int)]
  ): GraphSample[Int, String, Unit] =
    val topology =
      Graph
        .of(
          labels.indices,
          edges.map { case (left, right) => Link(left, right) }
        )
        .toOption
        .get
    GraphSample.build(SampleKey(key), topology)(
      nodeValue = labels.apply,
      edgeValue = _ => ()
    )

  private val recipe =
    UnweightedShortestPaths.parts[String, Unit, String](
      Projection.of(identity)
    )

  private def parts(
      graph: GraphSample[Int, String, Unit]
  ): Vector[ShortestPathPart[String, HopDistance]] =
    recipe
      .materializeReference(graph)
      .toOption
      .get
      .map(_.part)

  test("hop distance rejects zero and negative values"):
    assertEquals(HopDistance.from(0), Left(HopDistanceError.NonPositive(0)))
    assertEquals(HopDistance.from(-2), Left(HopDistanceError.NonPositive(-2)))
    assertEquals(HopDistance.from(3).map(_.toInt), Right(3))

  test("endpoint identity is unordered under universal and Cats equality"):
    val forward = UndirectedEndpoints("C", "O")
    val reverse = UndirectedEndpoints("O", "C")

    assertEquals(forward, reverse)
    assertEquals(forward.hashCode(), reverse.hashCode())

    given Hash[String] with
      def eqv(left: String, right: String): Boolean = left == right
      def hash(value: String): Int = 0

    val endpointHash = Hash[UndirectedEndpoints[String]]
    assert(endpointHash.eqv(forward, reverse))
    assertEquals(endpointHash.hash(forward), endpointHash.hash(reverse))
    assert(!endpointHash.eqv(forward, UndirectedEndpoints("C", "N")))

  test("a three-vertex path emits every reachable unordered pair once"):
    val path =
      sample(
        "path",
        Vector("C", "N", "O"),
        Vector(0 -> 1, 1 -> 2)
      )

    val observed = parts(path).toSet
    val expected = Set(
      ShortestPathPart(
        UndirectedEndpoints("C", "N"),
        HopDistance.from(1).toOption.get
      ),
      ShortestPathPart(
        UndirectedEndpoints("N", "O"),
        HopDistance.from(1).toOption.get
      ),
      ShortestPathPart(
        UndirectedEndpoints("C", "O"),
        HopDistance.from(2).toOption.get
      )
    )

    assertEquals(parts(path).size, 3)
    assertEquals(observed, expected)

  test("self pairs and disconnected pairs are omitted"):
    val disconnected =
      sample(
        "disconnected",
        Vector("C", "N", "O"),
        Vector(0 -> 1)
      )
    val singleton =
      sample("singleton", Vector("C"), Vector.empty)

    assertEquals(parts(disconnected).size, 1)
    assertEquals(parts(singleton), Vector.empty)

  test("equal endpoint labels preserve vertex-pair multiplicity"):
    val triangle =
      sample(
        "triangle",
        Vector("C", "C", "C"),
        Vector(0 -> 1, 1 -> 2, 0 -> 2)
      )
    val observed = parts(triangle)

    assertEquals(observed.size, 3)
    assert(
      observed.forall(part =>
        part.endpoints == UndirectedEndpoints("C", "C") &&
          part.distance.toInt == 1
      )
    )

  test("the fused decomposition agrees with the materialized reference"):
    val graph =
      sample(
        "cycle",
        Vector("A", "B", "C", "D"),
        Vector(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)
      )
    val materialized =
      recipe.materializeReference(graph).toOption.get
    val folded =
      recipe
        .decomposition[Int]
        .fold(graph, Vector.empty[ShortestPathOccurrence[Int, String, HopDistance]]) {
          (current, occurrence, weight) =>
            assertEquals(weight, NonNegativeFinite.one)
            current :+ occurrence
        }
        .toOption
        .get

    assertEquals(folded, materialized)

  test("vertex reindexing does not alter semantic path parts"):
    val original =
      sample(
        "original",
        Vector("C", "N", "O", "H"),
        Vector(0 -> 1, 1 -> 2, 1 -> 3)
      )
    val reindexed =
      sample(
        "reindexed",
        Vector("H", "O", "N", "C"),
        Vector(3 -> 2, 2 -> 1, 2 -> 0)
      )

    assertEquals(
      parts(original).groupMapReduce(identity)(_ => 1)(_ + _),
      parts(reindexed).groupMapReduce(identity)(_ => 1)(_ + _)
    )

  test("all simple graphs through four vertices satisfy the pair contract"):
    (1 to 4).foreach { vertexCount =>
      val possibleEdges =
        (for
          left <- 0 until vertexCount
          right <- (left + 1) until vertexCount
        yield left -> right).toVector
      (0 until (1 << possibleEdges.size)).foreach { edgeMask =>
        val edges =
          possibleEdges.zipWithIndex.collect {
            case (edge, index) if (edgeMask & (1 << index)) != 0 =>
              edge
          }
        val graph =
          sample(
            s"n$vertexCount-e$edgeMask",
            Vector.tabulate(vertexCount)(_.toString),
            edges
          )
        val occurrences =
          recipe.materializeReference(graph).toOption.get
        val witnesses = occurrences.map(_.witness.endpoints)

        assertEquals(witnesses.distinct.size, witnesses.size)
        assert(
          occurrences.forall(_.part.distance.toInt > 0),
          clues(vertexCount, edgeMask, occurrences)
        )
      }
    }
