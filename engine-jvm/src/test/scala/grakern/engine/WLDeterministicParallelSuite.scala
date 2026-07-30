package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLDeterministicParallelSuite extends FunSuite:
  final case class Atom(label: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private def samples: Vector[GraphSample[Int, Atom, Bond]] =
    Vector.tabulate(17) { graphIndex =>
      val size = graphIndex % 5 + 1
      val vertices = (0 until size).toVector
      val edges =
        vertices
          .sliding(2)
          .collect { case Vector(left, right) =>
            Link(left, right)
          }
          .toVector
      val topology =
        Graph.of(vertices, edges).toOption.get
      GraphSample.build(
        SampleKey(s"graph-$graphIndex"),
        topology
      )(
        nodeValue = vertex => Atom(if (vertex + graphIndex) % 2 == 0 then "A" else "B"),
        edgeValue = _ => Bond(1)
      )
    }

  private def kernel =
    val refinement =
      WLRefinement
        .create(
          3,
          Projection.of[Atom, String](_.label),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    (
      refinement.subtree +
        refinement.optimalAssignment
    ).normalized

  test("execution policy rejects invalid scheduling parameters"):
    assertEquals(
      GramExecution.deterministicParallel(0, 4),
      Left(
        GramExecutionError.NonPositiveParallelism(0)
      )
    )
    assertEquals(
      GramExecution.deterministicParallel(2, 0),
      Left(GramExecutionError.NonPositiveTileRows(0))
    )

  test("parallelism and tile boundaries do not change any Gram bit"):
    val sequential =
      WLCompiler.plan(samples, kernel).run().toOption.get
    val schedules =
      Vector(
        GramExecution
          .deterministicParallel(1, 1)
          .toOption
          .get,
        GramExecution
          .deterministicParallel(2, 3)
          .toOption
          .get,
        GramExecution
          .deterministicParallel(4, 8)
          .toOption
          .get
      )

    schedules.foreach { schedule =>
      val parallel =
        WLCompiler
          .plan(
            samples,
            kernel,
            WLPlanOptions(gramExecution = schedule)
          )
          .run()
          .toOption
          .get
      sequential.sampleKeys.indices.foreach { row =>
        sequential.sampleKeys.indices.foreach { column =>
          assertEquals(
            java.lang.Double.doubleToLongBits(
              parallel.gram.values(row, column)
            ),
            java.lang.Double.doubleToLongBits(
              sequential.gram.values(row, column)
            ),
            clues(schedule, row, column)
          )
        }
      }
      assert(
        parallel.planReport.explain.contains(
          schedule.description
        )
      )
    }

  test("repeated parallel execution is bit reproducible"):
    val schedule =
      GramExecution
        .deterministicParallel(3, 2)
        .toOption
        .get
    val options = WLPlanOptions(gramExecution = schedule)
    val first =
      WLCompiler
        .plan(samples, kernel, options)
        .run()
        .toOption
        .get
    val second =
      WLCompiler
        .plan(samples, kernel, options)
        .run()
        .toOption
        .get

    first.sampleKeys.indices.foreach { row =>
      first.sampleKeys.indices.foreach { column =>
        assertEquals(
          java.lang.Double.doubleToLongBits(
            first.gram.values(row, column)
          ),
          java.lang.Double.doubleToLongBits(
            second.gram.values(row, column)
          )
        )
      }
    }
