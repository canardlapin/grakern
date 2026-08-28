package grakern.graph4s

import cats.kernel.Hash
import grakern.core.*
import graph4s.{Graph, Link}
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/** Protocol laws for the graph4s adapter: every graph4s edge is an `Undirected` incidence at both
  * endpoints, vertices are distinct, and node/edge values are total.
  */
final class AdapterLawSuite extends ScalaCheckSuite:
  given Hash[Int] = Hash.fromUniversalHashCode

  private val genGraph: Gen[Graph[Int]] =
    for
      n <- Gen.choose(1, 7)
      m <- Gen.choose(0, 10)
      links <- Gen.listOfN(
        m,
        for
          a <- Gen.choose(0, n - 1)
          b <- Gen.choose(0, n - 1)
        yield (a, b)
      )
    yield
      val base = (0 until n).foldLeft(Graph.empty[Int])(_.addVertex(_))
      links.filter(l => l._1 != l._2).foldLeft(base) { case (graph, (a, b)) =>
        graph.connect(a, b).getOrElse(graph)
      }

  private given Arbitrary[Graph[Int]] = Arbitrary(genGraph)

  private def sample(graph: Graph[Int]): GraphSample[Int, String, Int] =
    GraphSample.build(SampleKey("g"), graph)(v => s"v$v", edge => edge.first + edge.second)

  property("vertices are distinct and total"):
    forAll { (graph: Graph[Int]) =>
      val s = sample(graph)
      s.vertices.distinct.size == s.vertices.size &&
      s.vertices.forall(v => s.nodeValue(v) == s"v$v")
    }

  property("every incidence is Undirected and symmetric with an equal value"):
    forAll { (graph: Graph[Int]) =>
      val s = sample(graph)
      s.vertices.forall { v =>
        s.incidences(v).forall { inc =>
          inc.orientation == Orientation.Undirected &&
          s.incidences(inc.other).exists(back => back.other == v && back.value == inc.value)
        }
      }
    }

  property("canonical edges enumerate each graph4s edge exactly once"):
    forAll { (graph: Graph[Int]) =>
      val s = sample(graph)
      val canonical = Neighbourhoods.canonicalEdges(s)
      canonical.size.toLong == graph.edgeCount &&
      canonical.forall(edge => graph.containsEdge(edge.ref.first, edge.ref.second))
    }

  test("NeighbourhoodSample reproduces a graph4s sample's incidences"):
    val graph = Graph.fromEdges(Link(1, 2), Link(2, 3)).toOption.get
    val fromGraph = sample(graph)
    given cats.kernel.Eq[Int] = cats.kernel.Eq.fromUniversalEquals
    val direct =
      NeighbourhoodSample
        .undirected(SampleKey("g"), Vector(1, 2, 3), v => s"v$v", Vector((1, 2, 3), (2, 3, 5)))
        .toOption
        .get
    assertEquals(
      fromGraph.vertices.sorted.map(v =>
        fromGraph.incidences(v).map(i => (i.other, i.value)).sorted
      ),
      direct.vertices.map(v => direct.incidences(v).map(i => (i.other, i.value)).sorted)
    )
