package grakern.laws

import cats.kernel.{Eq, Hash}
import grakern.core.*
import org.scalacheck.{Arbitrary, Gen}

/** A small labelled graph in a shape ScalaCheck can shrink and the laws can rename or reorder.
  *
  * Vertices are `0 until labels.size`; `edges` are `(source, target, edgeLabel)` and are read as
  * arcs when `directed` is true and as undirected edges otherwise. Parallel edges and self-loops
  * are allowed so the generators exercise multigraph semantics.
  */
final case class LabelledGraph(
    labels: Vector[String],
    edges: Vector[(Int, Int, String)],
    directed: Boolean
) derives CanEqual:
  def vertexCount: Int = labels.size

  def sample(key: String): NeighbourhoodSample[Int, String, String] =
    val vertices = Vector.tabulate(vertexCount)(identity)
    val built =
      if directed then
        NeighbourhoodSample.directed(SampleKey(key), vertices, labels(_), edges)(using
          GraphGens.intHash,
          GraphGens.stringEq
        )
      else
        NeighbourhoodSample.undirected(SampleKey(key), vertices, labels(_), edges)(using
          GraphGens.intHash,
          GraphGens.stringEq
        )
    built match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalStateException(s"generated graph failed protocol laws: $error")

  /** Alpha-rename vertices through a permutation `p`: old vertex `i` becomes `p(i)`. */
  def renamed(permutation: Vector[Int]): LabelledGraph =
    val inverse = Array.fill(permutation.size)(0)
    permutation.zipWithIndex.foreach { case (target, source) => inverse(target) = source }
    LabelledGraph(
      Vector.tabulate(vertexCount)(v => labels(inverse(v))),
      edges.map { case (s, t, e) => (permutation(s), permutation(t), e) },
      directed
    )

  /** Reorder the edge enumeration without changing the graph. */
  def shuffled(order: Vector[Int]): LabelledGraph =
    copy(edges = order.map(edges(_)))

  /** Reverse one arc (directed graphs only). */
  def reversedArc(index: Int): LabelledGraph =
    val (s, t, e) = edges(index)
    copy(edges = edges.updated(index, (t, s, e)))

  /** Brute-force isomorphism test for the small graphs the generators produce. */
  def isomorphicTo(that: LabelledGraph): Boolean =
    if vertexCount != that.vertexCount || edges.size != that.edges.size || directed != that.directed
    then false
    else
      val thatBag = that.edgeBag
      Vector.tabulate(vertexCount)(identity).permutations.exists { permutation =>
        labels.indices.forall(v => labels(v) == that.labels(permutation(v))) &&
        renamed(permutation).edgeBag == thatBag
      }

  /** Independent round-one WL signature histogram, expressed directly in graph data rather than
    * through the production interner. It is used to generate arc reversals that 1-WL is actually
    * capable of distinguishing; non-isomorphism alone is not sufficient for that claim.
    */
  def roundOneSignatureHistogram: Map[(String, Vector[(String, String, String)]), Int] =
    Vector
      .tabulate(vertexCount) { vertex =>
        val incidences = edges.flatMap { case (source, target, edgeLabel) =>
          if directed then
            Vector(
              Option.when(source == vertex)((edgeLabel, "out", labels(target))),
              Option.when(target == vertex)((edgeLabel, "in", labels(source)))
            ).flatten
          else if source == vertex && target == vertex then
            Vector((edgeLabel, "undirected", labels(vertex)))
          else
            Vector(
              Option.when(source == vertex)((edgeLabel, "undirected", labels(target))),
              Option.when(target == vertex)((edgeLabel, "undirected", labels(source)))
            ).flatten
        }.sorted
        labels(vertex) -> incidences
      }
      .groupMapReduce(identity)(_ => 1)(_ + _)

  private def edgeBag: Map[(Int, Int, String), Int] =
    val normalized =
      if directed then edges
      else edges.map { case (s, t, e) => if s <= t then (s, t, e) else (t, s, e) }
    normalized.groupMapReduce(identity)(_ => 1)(_ + _)

object GraphGens:
  given intHash: Hash[Int] = Hash.fromUniversalHashCode
  given stringHash: Hash[String] = Hash.fromUniversalHashCode
  given stringEq: Eq[String] = Eq.fromUniversalEquals

  val nodeLabels: Vector[String] = Vector("a", "b", "c")
  val edgeLabels: Vector[String] = Vector("e", "f")

  def graph(directed: Boolean, maxVertices: Int = 6, maxEdges: Int = 8): Gen[LabelledGraph] =
    for
      n <- Gen.choose(1, maxVertices)
      labels <- Gen.listOfN(n, Gen.oneOf(nodeLabels))
      m <- Gen.choose(0, maxEdges)
      edges <- Gen.listOfN(
        m,
        for
          s <- Gen.choose(0, n - 1)
          t <- Gen.choose(0, n - 1)
          e <- Gen.oneOf(edgeLabels)
        yield (s, t, e)
      )
    yield LabelledGraph(labels.toVector, edges.toVector, directed)

  val undirected: Gen[LabelledGraph] = graph(directed = false)
  val directed: Gen[LabelledGraph] = graph(directed = true)

  /** A uniformly random permutation of `0 until n`. */
  def permutation(n: Int): Gen[Vector[Int]] =
    Gen.const(Vector.tabulate(n)(identity)).flatMap(shuffle)

  private def shuffle(values: Vector[Int]): Gen[Vector[Int]] =
    if values.size <= 1 then Gen.const(values)
    else
      Gen.choose(0, values.size - 1).flatMap { index =>
        val picked = values(index)
        shuffle(values.patch(index, Nil, 1)).map(picked +: _)
      }

  /** A graph together with a vertex permutation and an edge-order permutation for it. */
  final case class Relabelling(
      graph: LabelledGraph,
      vertexPermutation: Vector[Int],
      edgeOrder: Vector[Int]
  ) derives CanEqual:
    def relabelled: LabelledGraph = graph.renamed(vertexPermutation).shuffled(edgeOrder)

  def relabelling(base: Gen[LabelledGraph]): Gen[Relabelling] =
    for
      g <- base
      p <- permutation(g.vertexCount)
      o <- permutation(g.edges.size)
    yield Relabelling(g, p, o)

  /** A directed graph and an arc whose reversal changes the independent round-one WL histogram. */
  final case class ArcReversal(graph: LabelledGraph, arc: Int) derives CanEqual:
    def reversed: LabelledGraph = graph.reversedArc(arc)

  val arcReversal: Gen[ArcReversal] =
    graph(directed = true, maxVertices = 5, maxEdges = 6)
      .suchThat(_.edges.nonEmpty)
      .flatMap { g =>
        val candidates =
          g.edges.indices.filter { i =>
            val (s, t, _) = g.edges(i)
            val reversed = g.reversedArc(i)
            s != t &&
            !g.isomorphicTo(reversed) &&
            g.roundOneSignatureHistogram != reversed.roundOneSignatureHistogram
          }
        if candidates.isEmpty then Gen.fail
        else Gen.oneOf(candidates).map(ArcReversal(g, _))
      }

  /** A non-empty set of graphs sharing one directedness, for Gram-matrix laws. */
  def graphSet(directed: Boolean, maxGraphs: Int = 5): Gen[Vector[LabelledGraph]] =
    Gen.choose(1, maxGraphs).flatMap(k => Gen.listOfN(k, graph(directed)).map(_.toVector))

  given arbitraryUndirected: Arbitrary[LabelledGraph] = Arbitrary(undirected)
