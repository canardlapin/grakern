package grakern.engine

import cats.kernel.Hash
import gale.sparse.{DuplicatePolicy, Sparse}
import grakern.core.*
import grakern.laws.GraphGens
import grakern.standard.wl.*
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.forAll

final class SparseFeatureMatrixSuite extends FunSuite:
  test("a million-column CSR row exposes only its stored entries"):
    val builder = Sparse.coo(1, 1_000_000)
    builder.add(0, 999_999, 2.5)
    val matrix = builder.tryToCSR(DuplicatePolicy.Error).toOption.get
    val row = SparseCSRIndex(matrix).row(0).get

    assertEquals(row.indices, Vector(999_999))
    assertEquals(row.values, Vector(2.5))
    assertEquals(row.valueAt(0), 0.0)
    assertEquals(row.valueAt(999_999), 2.5)

  test("SampleIndex preserves first-match semantics for duplicate keys"):
    val first = SampleKey("first")
    val second = SampleKey("second")
    val index = SampleIndex(Vector(first, second, first))

    assertEquals(index.indexOf(first), Some(0))
    assertEquals(index.indexOf(second), Some(1))
    assertEquals(index.indexOf(SampleKey("missing")), None)

final class WLQueryOverlayPropertySuite extends ScalaCheckSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  private val refinement =
    WLRefinement
      .create(
        2,
        Projection.of[String, String](identity),
        Projection.of[String, String](identity),
        WLCodecs[String, String](None, None)
      )
      .toOption
      .get

  private val kernel = (refinement.subtree + refinement.optimalAssignment).normalized

  property("single-query sparse cross equals batch and joint-corpus recompilation"):
    forAll(GraphGens.graphSet(directed = true, maxGraphs = 4), GraphGens.directed) {
      (trainingGraphs, queryGraph) =>
        val training = trainingGraphs.zipWithIndex.map { case (graph, index) =>
          graph.sample(s"training-$index")
        }
        val query = queryGraph.sample("query")
        val prepared = WLCompiler.plan(training, kernel).run().toOption.get
        val single = prepared.query(query).toOption.get
        val batch = prepared.cross(Vector(query)).toOption.get
        val joint = WLCompiler.plan(training :+ query, kernel).run().toOption.get
        val queryIndex = training.size

        training.indices.forall { referenceIndex =>
          val fromSingle = single.cross.valueAt(referenceIndex)
          val fromBatch = batch.cross.values(0, referenceIndex)
          val fromJoint = joint.kernel(queryIndex, referenceIndex).get
          java.lang.Double.doubleToLongBits(fromSingle) ==
            java.lang.Double.doubleToLongBits(fromBatch) &&
            math.abs(fromSingle - fromJoint) <= 1e-12
        }
    }
