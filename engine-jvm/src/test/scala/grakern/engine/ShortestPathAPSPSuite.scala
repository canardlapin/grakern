package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class ShortestPathAPSPSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode

  final private case class GraphSpec(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int)]
  )

  private def sample(
      spec: GraphSpec
  ): GraphSample[Int, String, Unit] =
    val topology =
      Graph
        .of(
          spec.labels.indices,
          spec.edges.map { case (left, right) => Link(left, right) }
        )
        .toOption
        .get
    GraphSample.build(SampleKey(spec.key), topology)(
      nodeValue = spec.labels.apply,
      edgeValue = _ => ()
    )

  private val recipe =
    UnweightedShortestPaths.parts[String, Unit, String](
      Projection.of(identity)
    )

  private def partCounts[A](
      parts: Vector[ShortestPathPart[A, HopDistance]]
  ): Map[ShortestPathPart[A, HopDistance], Int] =
    parts.groupMapReduce(identity)(_ => 1)(_ + _)

  test("packed BFS agrees with the Floyd-Warshall reference"):
    val specs = Vector(
      GraphSpec("singleton", Vector("C"), Vector.empty),
      GraphSpec(
        "path",
        Vector("C", "N", "O"),
        Vector(0 -> 1, 1 -> 2)
      ),
      GraphSpec(
        "disconnected",
        Vector("C", "C", "O", "H"),
        Vector(0 -> 1, 2 -> 3)
      ),
      GraphSpec(
        "cycle",
        Vector("A", "B", "C", "D"),
        Vector(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)
      )
    )
    val samples = specs.map(sample)
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val optimized = trace.occurrences(recipe)

    samples.zip(optimized).foreach { case (graph, occurrences) =>
      val reference =
        recipe.materializeReference(graph).toOption.get
      assertEquals(
        partCounts(occurrences.map(_.part)),
        partCounts(reference.map(_.part))
      )
    }
    assertEquals(trace.stats.graphTransforms, samples.size)
    assertEquals(
      trace.stats.sourceSearches,
      samples.map(_.topology.vertexCount).sum
    )

  test("all simple graphs through four vertices agree with the reference"):
    val specs =
      (1 to 4).toVector.flatMap { vertexCount =>
        val possibleEdges =
          (for
            left <- 0 until vertexCount
            right <- (left + 1) until vertexCount
          yield left -> right).toVector
        (0 until (1 << possibleEdges.size)).toVector.map { edgeMask =>
          val edges =
            possibleEdges.zipWithIndex.collect {
              case (edge, index) if (edgeMask & (1 << index)) != 0 =>
                edge
            }
          GraphSpec(
            s"n$vertexCount-e$edgeMask",
            Vector.tabulate(vertexCount)(_.toString),
            edges
          )
        }
      }
    val samples = specs.map(sample)
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val optimized = trace.occurrences(recipe)

    samples.zip(optimized).foreach { case (graph, occurrences) =>
      val reference =
        recipe.materializeReference(graph).toOption.get
      assertEquals(
        partCounts(occurrences.map(_.part)),
        partCounts(reference.map(_.part)),
        graph.key.value
      )
    }

  test("one APSP trace serves multiple endpoint projections"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "path",
            Vector("Carbon", "Nitrogen", "Oxygen"),
            Vector(0 -> 1, 1 -> 2)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val initialStats = trace.stats
    val fullNames =
      trace.occurrences(recipe)
    val initials =
      trace.occurrences(
        UnweightedShortestPaths.parts[String, Unit, Char](
          Projection.of(_.head)
        )
      )

    assertEquals(trace.stats, initialStats)
    assertEquals(trace.stats.graphTransforms, 1)
    assertEquals(trace.stats.sourceSearches, 3)
    assertEquals(fullNames.head.size, 3)
    assertEquals(initials.head.size, 3)
    assertEquals(
      trace.planNodeId.number,
      trace.planNodeId.number
    )

  test("reachable pair accounting excludes self and disconnected pairs"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "two-components",
            Vector("A", "B", "C", "D", "E"),
            Vector(0 -> 1, 1 -> 2, 3 -> 4)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get

    assertEquals(trace.stats.reachableUnorderedPairs, 4L)
    assertEquals(trace.occurrences(recipe).head.size, 4)
    assertEquals(trace.stats.matrixEntries, 25L)
