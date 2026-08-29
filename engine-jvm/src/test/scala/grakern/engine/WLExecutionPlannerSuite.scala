package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLExecutionPlannerSuite extends FunSuite:
  final case class Atom(label: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private def samples: Vector[GraphSample[Int, Atom, Bond]] =
    val topology = Graph.fromEdges(Link(0, 1)).toOption.get
    Vector.tabulate(4) { index =>
      GraphSample.build(SampleKey(s"same-$index"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    }

  private def kernel =
    WLRefinement
      .create(
        2,
        Projection.of[Atom, String](_.label),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get
      .subtree

  private def compile(preference: WLGramPreference) =
    WLCompiler
      .plan(
        samples,
        kernel,
        WLPlanOptions(gramPreference = preference)
      )
      .run()
      .toOption
      .get

  test("every admitted Gram realization preserves every IEEE-754 bit"):
    val prepared =
      Vector(
        compile(WLGramPreference.DenseScratchRows),
        compile(WLGramPreference.SparseFeaturePostings),
        compile(WLGramPreference.DenseRows)
      )

    assertEquals(
      prepared.map(_.strategy.gram),
      Vector(
        WLGramRealization.DenseScratchRows,
        WLGramRealization.SparseFeaturePostings,
        WLGramRealization.DenseRows
      )
    )
    val reference = prepared.head
    prepared.tail.foreach { candidate =>
      reference.sampleKeys.indices.foreach { row =>
        reference.sampleKeys.indices.foreach { column =>
          assertEquals(
            java.lang.Double.doubleToLongBits(candidate.gram.values(row, column)),
            java.lang.Double.doubleToLongBits(reference.gram.values(row, column)),
            clues(candidate.strategy.gram, row, column)
          )
        }
      }
    }

  test("an inadmissible forced strategy has an explicit deterministic fallback"):
    val oneByte = WLMemoryBudget.fromBytes(1L).get
    val compiled =
      WLCompiler
        .plan(
          samples,
          kernel,
          WLPlanOptions(
            gramPreference = WLGramPreference.DenseRows,
            memoryBudget = Some(oneByte)
          )
        )
        .run()
        .toOption
        .get

    assertEquals(compiled.strategy.gram, WLGramRealization.DenseScratchRows)
    assert(compiled.strategy.estimates.forall(estimate => !estimate.admitted))
    assert(compiled.strategy.decision.contains("deterministic fallback"))
    assert(compiled.planReport.explain.contains("working-bytes="))

  test("parallel execution rejects sequential postings and reports the reason"):
    val parallel = GramExecution.deterministicParallel(2, 2).toOption.get
    val compiled =
      WLCompiler
        .plan(
          samples,
          kernel,
          WLPlanOptions(
            gramExecution = parallel,
            gramPreference = WLGramPreference.SparseFeaturePostings
          )
        )
        .run()
        .toOption
        .get

    assertEquals(compiled.strategy.gram, WLGramRealization.DenseScratchRows)
    assert(
      compiled.strategy.estimates
        .find(_.realization == WLGramRealization.SparseFeaturePostings)
        .exists(_.reason.contains("parallel schedule"))
    )

  test("anticipated novelty switches sparse query evaluation away from postings"):
    val novelty = WLQueryNovelty.fromFraction(0.8).get
    val plan = WLCompiler.plan(samples, kernel)
    val sparseRows =
      Vector(
        PortableSparseFeatureRow(Vector(0), Vector(1.0)),
        PortableSparseFeatureRow(Vector(1), Vector(1.0))
      )
    val strategy =
      WLExecutionPlanner.select(
        sparseRows,
        featureCount = 100,
        graph = plan.graph,
        options = WLPlanOptions(
          requestedOutput = WLRequestedOutput.Query,
          anticipatedQueryNovelty = novelty
        )
      )

    assertEquals(strategy.query, WLQueryRealization.PairwiseSparseRows)
    assertEquals(strategy.requestedOutput, WLRequestedOutput.Query)
    assertEquals(strategy.anticipatedQueryNovelty, novelty)

  test("query-only preparation preserves cross values without constructing all views"):
    val full = WLCompiler.plan(samples, kernel).run().toOption.get
    val queryOnly =
      WLCompiler
        .queryPlan(samples, kernel)
        .runForQueries()
        .toOption
        .get
    val querySamples = samples.take(2)
    val expected = full.cross(querySamples).toOption.get.cross.values
    val observed = queryOnly.cross(querySamples).toOption.get.cross.values

    assertEquals(queryOnly.strategy.requestedOutput, WLRequestedOutput.Query)
    assertEquals(queryOnly.executionStats, full.executionStats)
    assertEquals(observed.rows, expected.rows)
    assertEquals(observed.cols, expected.cols)
    var row = 0
    while row < observed.rows do
      var column = 0
      while column < observed.cols do
        assertEquals(
          java.lang.Double.doubleToLongBits(observed(row, column)),
          java.lang.Double.doubleToLongBits(expected(row, column))
        )
        column += 1
      row += 1

    WLCompiler.plan(samples, kernel).runForQueries() match
      case Left(WLCompileError.RequestedOutput(expectedOutput, actualOutput)) =>
        assertEquals(expectedOutput, WLRequestedOutput.Query)
        assertEquals(actualOutput, WLRequestedOutput.AllViews)
      case other => fail(s"expected requested-output mismatch, observed $other")
