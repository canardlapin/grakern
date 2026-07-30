package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite
import scala.io.Source
import scala.util.Using

final class GraKeLParitySuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val fixture =
    Using.resource(
      Source.fromResource(
        "grakel/0.1.10/wl_pair_path.json"
      )
    )(_.mkString)

  private def flatValues(key: String): Vector[Double] =
    val marker = s"\"$key\": ["
    val start = fixture.indexOf(marker)
    assert(start >= 0, clues(key))
    val contentStart = start + marker.length
    val end = fixture.indexOf("]", contentStart)
    assert(end >= 0, clues(key))
    fixture
      .substring(contentStart, end)
      .split(",")
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toDouble)
      .toVector

  private val refinement =
    WLRefinement
      .create(
        2,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get

  private def trainingSamples: Vector[GraphSample[AtomId, Atom, Bond]] =
    val pairTopology =
      Graph
        .fromEdges(Link(AtomId(0), AtomId(1)))
        .toOption
        .get
    val pathTopology =
      Graph
        .fromEdges(
          Link(AtomId(0), AtomId(1)),
          Link(AtomId(1), AtomId(2))
        )
        .toOption
        .get
    val pair =
      GraphSample.build(SampleKey("pair"), pairTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val path =
      GraphSample.build(SampleKey("path"), pathTopology)(
        nodeValue = id => Atom(if id.value == 1 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    Vector(pair, path)

  private def querySamples: Vector[GraphSample[AtomId, Atom, Bond]] =
    val single =
      GraphSample.build(
        SampleKey("single"),
        Graph.singleton(AtomId(20))
      )(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val triangleTopology =
      Graph
        .fromEdges(
          Link(AtomId(30), AtomId(31)),
          Link(AtomId(31), AtomId(32)),
          Link(AtomId(30), AtomId(32))
        )
        .toOption
        .get
    val triangle =
      GraphSample.build(
        SampleKey("triangle"),
        triangleTopology
      )(
        nodeValue = id => Atom(if id.value == 32 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    Vector(single, triangle)

  private def trace: WLTrace[AtomId, String, Int] =
    WLTrace
      .prepare(trainingSamples, refinement)
      .toOption
      .get

  private def gram(
      graphCount: Int
  )(
      evaluate: (Int, Int) => Double
  ): Vector[Double] =
    Vector.tabulate(graphCount * graphCount) { flatIndex =>
      evaluate(
        flatIndex / graphCount,
        flatIndex % graphCount
      )
    }

  private def assertParity(
      observed: Vector[Double],
      expected: Vector[Double]
  ): Unit =
    assertEquals(observed.size, expected.size)
    observed.zip(expected).zipWithIndex.foreach { case ((actual, target), index) =>
      assertEqualsDouble(
        actual,
        target,
        1e-12,
        clues(index)
      )
    }

  test("fixture records the pinned compatibility contract"):
    assert(
      fixture.contains(
        "\"distribution_version\": \"0.1.10\""
      )
    )
    assert(
      fixture.contains(
        "\"authority\": \"compatibility evidence, not semantic oracle\""
      )
    )
    assert(fixture.contains("\"rounds\": 2"))
    assert(
      fixture.contains(
        "\"schema_version\": \"grakern.grakel-fixture.v2\""
      )
    )
    assert(fixture.contains("\"query\": ["))
    assert(
      fixture.contains(
        "\"view\": \"wl_optimal_assignment_cross\""
      )
    )

  test("WL subtree Gram agrees with aligned GraKeL 0.1.10"):
    val source = trace
    val features = WLSubtreeFeatures.unitWeighted(source)
    val observed =
      gram(features.graphCount) { (left, right) =>
        features.kernel(left, right).get
      }

    assertParity(
      observed,
      flatValues("wl_subtree_gram_flat")
    )

  test("WL optimal-assignment Gram agrees with aligned GraKeL 0.1.10"):
    val source = trace
    val observedHierarchy =
      WLObservedHierarchy.fromTrace(source).toOption.get
    val features =
      WLOAThresholdFeatures.fromObserved(observedHierarchy)
    val observed =
      gram(features.graphCount) { (left, right) =>
        features.kernel(left, right).get
      }

    assertParity(
      observed,
      flatValues("wl_optimal_assignment_gram_flat")
    )

  test("normalized WL Gram views agree with aligned GraKeL 0.1.10"):
    val subtree =
      WLCompiler
        .plan(trainingSamples, refinement.subtree.normalized)
        .run()
        .toOption
        .get
    val assignment =
      WLCompiler
        .plan(
          trainingSamples,
          refinement.optimalAssignment.normalized
        )
        .run()
        .toOption
        .get

    assertParity(
      gram(subtree.graphCount) { (left, right) =>
        subtree.gram.values(left, right)
      },
      flatValues("wl_subtree_normalized_gram_flat")
    )
    assertParity(
      gram(assignment.graphCount) { (left, right) =>
        assignment.gram.values(left, right)
      },
      flatValues(
        "wl_optimal_assignment_normalized_gram_flat"
      )
    )

  test("unnormalized WL subtree cross agrees with aligned GraKeL 0.1.10"):
    val unnormalized =
      WLCompiler
        .plan(trainingSamples, refinement.subtree)
        .run()
        .toOption
        .get
        .cross(querySamples)
        .toOption
        .get
    assertParity(
      flattenCross(unnormalized),
      flatValues("wl_subtree_cross_flat")
    )

  test("normalized query parity is excluded when GraKeL drops novel norms"):
    val grakern =
      WLCompiler
        .plan(trainingSamples, refinement.subtree.normalized)
        .run()
        .toOption
        .get
        .cross(querySamples)
        .toOption
        .get
    val grakel =
      flatValues("wl_subtree_normalized_cross_flat")

    assert(
      fixture.contains(
        "\"view\": \"wl_subtree_normalized_cross\""
      )
    )
    assertNotEquals(
      grakern.cross.values(0, 0),
      grakel.head
    )
    assertEqualsDouble(
      grakern.features.completeSquaredNorm(0).get,
      1.0,
      1e-12
    )

  test("OA query parity is explicitly excluded rather than silently imitated"):
    val grakern =
      WLCompiler
        .plan(trainingSamples, refinement.optimalAssignment)
        .run()
        .toOption
        .get
        .cross(querySamples)
        .toOption
        .get
    val grakel =
      flatValues("wl_optimal_assignment_cross_flat")

    assertEqualsDouble(grakel.head, 0.0, 0.0)
    assert(
      grakern.cross.values(0, 0) > 0.0,
      "Grakern retains shared initial-label ancestry for the query singleton"
    )

  private def flattenCross(
      result: WLQueryResult
  ): Vector[Double] =
    val rowCount = result.cross.queryRows.keys.size
    val columnCount =
      result.cross.referenceColumns.keys.size
    Vector.tabulate(rowCount * columnCount) { index =>
      result.cross.values(
        index / columnCount,
        index % columnCount
      )
    }
