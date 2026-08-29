package grakern.standard

import cats.kernel.{Eq, Hash}
import grakern.core.*
import grakern.standard.wl.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class ReifySuite extends ScalaCheckSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  test("parallel incidence multiplicity changes WL colour and enumeration order does not"):
    val single =
      NeighbourhoodSample
        .undirected(SampleKey("single"), Vector(0, 1), _ => "node", Vector((0, 1, "edge")))
        .toOption
        .get
    val parallel =
      NeighbourhoodSample
        .undirected(
          SampleKey("parallel"),
          Vector(0, 1),
          _ => "node",
          Vector((0, 1, "edge"), (0, 1, "edge"), (0, 1, "other"))
        )
        .toOption
        .get
    val shuffled =
      NeighbourhoodSample
        .undirected(
          SampleKey("shuffled"),
          Vector(0, 1),
          _ => "node",
          Vector((0, 1, "other"), (0, 1, "edge"), (0, 1, "edge"))
        )
        .toOption
        .get
    val histograms = roundOneHistograms(Vector(single, parallel, shuffled))

    assertNotEquals(histograms(0), histograms(1))
    assertEquals(histograms(1), histograms(2))

  test("one undirected self-loop changes the round-one WL colour"):
    val without =
      NeighbourhoodSample
        .undirected(
          SampleKey("without"),
          Vector(0),
          _ => "node",
          Vector.empty[(Int, Int, String)]
        )
        .toOption
        .get
    val withLoop =
      NeighbourhoodSample
        .undirected(SampleKey("with"), Vector(0), _ => "node", Vector((0, 0, "loop")))
        .toOption
        .get
    val histograms = roundOneHistograms(Vector(without, withLoop))

    assertNotEquals(histograms(0), histograms(1))

  test(
    "parallel relations become distinct labelled nodes in the documented reference construction"
  ):
    val source =
      NeighbourhoodSample
        .directed(
          SampleKey("relations"),
          Vector(0, 1),
          vertex => s"node-$vertex",
          Vector((0, 1, "ARG0"), (0, 1, "ARG1"))
        )
        .toOption
        .get
    val observed = Reify.relationsAsVertices(source).toOption.get
    val firstRef = EdgeRef(0, 1, Orientation.Outgoing, 0)
    val secondRef = EdgeRef(0, 1, Orientation.Outgoing, 1)
    val entity0 = ReifiedVertex.Entity(0)
    val entity1 = ReifiedVertex.Entity(1)
    val relation0 = ReifiedVertex.Relation(firstRef)
    val relation1 = ReifiedVertex.Relation(secondRef)
    given Hash[ReifiedVertex[Int]] = observed.vertexHash
    given Eq[ReifiedEndpoint] = Eq.fromUniversalEquals
    val expected =
      NeighbourhoodSample
        .undirected(
          source.key,
          Vector(entity0, entity1, relation0, relation1),
          {
            case ReifiedVertex.Entity(0)                           => ReifiedNode.Entity("node-0")
            case ReifiedVertex.Entity(1)                           => ReifiedNode.Entity("node-1")
            case ReifiedVertex.Relation(edge) if edge.ordinal == 0 =>
              ReifiedNode.Relation("ARG0")
            case ReifiedVertex.Relation(edge) if edge.ordinal == 1 =>
              ReifiedNode.Relation("ARG1")
            case other => fail(s"unexpected reference vertex $other")
          },
          Vector(
            (relation0, entity0, ReifiedEndpoint.Source),
            (relation0, entity1, ReifiedEndpoint.Target),
            (relation1, entity0, ReifiedEndpoint.Source),
            (relation1, entity1, ReifiedEndpoint.Target)
          )
        )
        .toOption
        .get

    assertSameNeighbourhood(observed, expected, identity)

  test("an undirected self-loop has one reified endpoint incidence"):
    val source =
      NeighbourhoodSample
        .undirected(SampleKey("loop"), Vector(0), _ => "node", Vector((0, 0, "loop")))
        .toOption
        .get
    val reified = Reify.relationsAsVertices(source).toOption.get
    val entity = reified.vertices.collectFirst { case value @ ReifiedVertex.Entity(_) => value }.get
    val relation =
      reified.vertices.collectFirst { case value @ ReifiedVertex.Relation(_) => value }.get

    assertEquals(reified.incidences(entity).map(_.other), Vector(relation))
    assertEquals(reified.incidences(entity).map(_.value), Vector(ReifiedEndpoint.Undirected))
    assertEquals(reified.incidences(relation).map(_.other), Vector(entity))

  test("a directed self-loop preserves distinct source and target roles"):
    val source =
      NeighbourhoodSample
        .directed(SampleKey("loop"), Vector(0), _ => "node", Vector((0, 0, "loop")))
        .toOption
        .get
    val reified = Reify.relationsAsVertices(source).toOption.get
    val entity = reified.vertices.collectFirst { case value @ ReifiedVertex.Entity(_) => value }.get

    assertEquals(
      reified.incidences(entity).map(_.value).toSet,
      Set(ReifiedEndpoint.Source, ReifiedEndpoint.Target)
    )

  private val graphAndRenaming =
    for
      vertices <- Gen.choose(1, 6)
      edgeCount <- Gen.choose(0, 10)
      edges <- Gen.listOfN(
        edgeCount,
        for
          source <- Gen.choose(0, vertices - 1)
          target <- Gen.choose(0, vertices - 1)
          label <- Gen.oneOf("ARG0", "ARG1", "mod")
        yield (source, target, label)
      )
      permutation <- Gen.pick(vertices, 0 until vertices).map(_.toVector)
    yield (vertices, edges.toVector, permutation)

  property("relation reification is deterministic and alpha-invariant"):
    forAll(graphAndRenaming) { case (vertexCount, edges, permutation) =>
      val vertices = Vector.tabulate(vertexCount)(identity)
      val original =
        NeighbourhoodSample
          .directed(
            SampleKey("original"),
            vertices,
            vertex => s"kind-${vertex % 2}",
            edges
          )
          .toOption
          .get
      val inverse = permutation.zipWithIndex.map { case (renamed, old) => renamed -> old }.toMap
      val renamed =
        NeighbourhoodSample
          .directed(
            SampleKey("renamed"),
            vertices,
            vertex => s"kind-${inverse(vertex) % 2}",
            edges.map { case (source, target, label) =>
              (permutation(source), permutation(target), label)
            }
          )
          .toOption
          .get
      val first = Reify.relationsAsVertices(original).toOption.get
      val second = Reify.relationsAsVertices(renamed).toOption.get

      assertSameNeighbourhood(first, second, renameVertex(_, permutation))
      true
    }

  private def renameVertex(
      vertex: ReifiedVertex[Int],
      permutation: Vector[Int]
  ): ReifiedVertex[Int] =
    vertex match
      case ReifiedVertex.Entity(value)  => ReifiedVertex.Entity(permutation(value))
      case ReifiedVertex.Relation(edge) =>
        ReifiedVertex.Relation(
          edge.copy(first = permutation(edge.first), second = permutation(edge.second))
        )

  private def assertSameNeighbourhood(
      first: LabelledNeighbourhood[
        ReifiedVertex[Int],
        ReifiedNode[String, String],
        ReifiedEndpoint
      ],
      second: LabelledNeighbourhood[
        ReifiedVertex[Int],
        ReifiedNode[String, String],
        ReifiedEndpoint
      ],
      rename: ReifiedVertex[Int] => ReifiedVertex[Int]
  ): Unit =
    assertEquals(first.vertices.size, second.vertices.size)
    first.vertices.foreach { vertex =>
      val renamed = rename(vertex)
      val counterpart = second.vertices.find(second.vertexHash.eqv(_, renamed)).getOrElse {
        fail(s"missing reified vertex $renamed")
      }
      assertEquals(first.nodeValue(vertex), second.nodeValue(counterpart))
      first.incidences(vertex).foreach { incidence =>
        val renamedOther = rename(incidence.other)
        val expectedMultiplicity = first
          .incidences(vertex)
          .count(other =>
            first.vertexHash.eqv(other.other, incidence.other) && other.value == incidence.value
          )
        val observedMultiplicity = second
          .incidences(counterpart)
          .count(other =>
            second.vertexHash.eqv(other.other, renamedOther) && other.value == incidence.value
          )
        assertEquals(observedMultiplicity, expectedMultiplicity)
      }
    }

  private def roundOneHistograms(
      samples: Vector[LabelledNeighbourhood[Int, String, String]]
  ): Vector[Map[Int, Int]] =
    val builder =
      WLInternerBuilder.empty(
        WLCodecs[String, String](None, None),
        WLHashStrategy.default[String, String]
      )
    val initial = samples.map(sample =>
      sample.vertices.toVector.map(vertex => builder.internInitial(sample.nodeValue(vertex)))
    )
    samples.zipWithIndex.map { case (sample, sampleIndex) =>
      sample.vertices.toVector.zipWithIndex
        .map { case (vertex, vertexIndex) =>
          val neighbourhood = sample.incidences(vertex).toVector.map { incidence =>
            val other = sample.vertices.indexWhere(sample.vertexHash.eqv(_, incidence.other))
            WLIncidence(incidence.value, incidence.orientation, initial(sampleIndex)(other))
          }
          builder.internRefined(initial(sampleIndex)(vertexIndex), neighbourhood).toOption.get
        }
        .groupMapReduce(_.index)(_ => 1)(_ + _)
    }
