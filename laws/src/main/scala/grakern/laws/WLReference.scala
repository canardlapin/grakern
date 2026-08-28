package grakern.laws

import cats.data.NonEmptyVector
import cats.kernel.Hash
import grakern.core.*
import grakern.standard.wl.*

/** A portable reference evaluator for WL kernel programs.
  *
  * This is a law oracle, not the feature emitter: it traces samples jointly through the exact
  * [[WLInternerBuilder]] so colours are comparable across graphs, then evaluates subtree kernels as
  * weighted histogram dot products and optimal-assignment kernels through the core [[Hierarchy]]
  * built exactly as the JVM engine builds it (root, one node per colour, parent = previous colour,
  * height = level + 1, leaves = final-round colours). The JVM parity suite ties these values to
  * `WLCompiledKernel.kernel`.
  */
final class WLReference[V, N, E, NodeKey, EdgeKey] private (
    val rounds: Int,
    val sampleKeys: Vector[SampleKey],
    /** graph -> round -> vertex position -> colour index */
    val colours: Vector[Vector[Vector[Int]]],
    /** graph -> round -> colour index -> multiplicity */
    val histograms: Vector[Vector[Map[Int, Int]]],
    private val hierarchy: Hierarchy[Int, WLReference.Node],
    private val finalBags: Vector[FiniteBag[Int]]
):
  def graphCount: Int = sampleKeys.size

  /** Colour partition of one graph at one round, canonically relabelled by first occurrence so it
    * can be compared across traces.
    */
  def canonicalPartition(graph: Int, round: Int): Vector[Int] =
    WLReference.canonical(colours(graph)(round))

  def subtree(weights: Vector[NonNegativeFinite])(left: Int, right: Int): Double =
    var total = 0.0
    var round = 0
    while round <= rounds do
      val w = weights(round).toDouble
      if w != 0.0 then
        val a = histograms(left)(round)
        val b = histograms(right)(round)
        val (small, large) = if a.size <= b.size then (a, b) else (b, a)
        var dot = 0.0
        small.foreach { case (colour, count) =>
          large.get(colour).foreach(other => dot += count.toDouble * other.toDouble)
        }
        total += w * dot
      round += 1
    total

  def optimalAssignment(left: Int, right: Int): Double =
    hierarchy.optimalAssignment(finalBags(left), finalBags(right))

  /** Evaluate a WL kernel program on two traced graphs. */
  def evaluate(kernel: WLKernel[N, E, NodeKey, EdgeKey])(left: Int, right: Int): Double =
    WLReference.evaluateExpr(this, kernel.expression, left, right)

  def gram(kernel: WLKernel[N, E, NodeKey, EdgeKey]): Array[Array[Double]] =
    Array.tabulate(graphCount, graphCount)((i, j) => evaluate(kernel)(i, j))

object WLReference:
  enum Node derives CanEqual:
    case Root
    case Colour(index: Int)

  given nodeHash: Hash[Node] = Hash.fromUniversalHashCode
  given intEq: cats.kernel.Eq[Int] = cats.kernel.Eq.fromUniversalEquals

  /** Trace all samples jointly for the refinement's round count. */
  def trace[V, N, E, NodeKey, EdgeKey](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  )(using Hash[NodeKey], Hash[EdgeKey]): WLReference[V, N, E, NodeKey, EdgeKey] =
    val builder = WLInternerBuilder.empty(refinement.codecs, refinement.hashing)
    val rounds = refinement.rounds
    // graph -> vertex position -> colour, per round
    val initial: Vector[Vector[WLColour]] =
      samples.map(s =>
        s.vertices.toVector.map(v => builder.internInitial(refinement.nodeKey(s.nodeValue(v))))
      )
    val perRound = Vector.newBuilder[Vector[Vector[WLColour]]]
    perRound += initial
    var previous = initial
    var round = 1
    while round <= rounds do
      val next =
        samples.zipWithIndex.map { case (s, g) =>
          given Hash[V] = s.vertexHash
          val positions = s.vertices.iterator.zipWithIndex.map { case (v, i) =>
            HashKey(v) -> i
          }.toMap
          s.vertices.toVector.zipWithIndex.map { case (v, i) =>
            val parent = previous(g)(i)
            val neighbourhood =
              s.incidences(v).toVector.map { inc =>
                WLIncidence(
                  refinement.edgeKey(inc.value),
                  inc.orientation,
                  previous(g)(positions(HashKey(inc.other)))
                )
              }
            builder.internRefined(parent, neighbourhood) match
              case Right(colour) => colour
              case Left(error)   =>
                throw new IllegalStateException(s"reference trace failed: $error")
          }
        }
      perRound += next
      previous = next
      round += 1
    val byRound = perRound.result() // round -> graph -> position -> colour
    val interner = builder.freeze()
    val colours: Vector[Vector[Vector[Int]]] =
      samples.indices.toVector.map(g => byRound.map(r => r(g).map(_.index)))
    val histograms =
      colours.map(_.map(_.groupMapReduce(identity)(_ => 1)(_ + _)))
    val allColours = interner.colours
    val nodes = NonEmptyVector(Node.Root, allColours.map(c => Node.Colour(c.index)))
    val parentOf: Map[Int, Option[Int]] =
      allColours.map(c => c.index -> interner.signature(c).flatMap(_.parent).map(_.index)).toMap
    val levelOf: Map[Int, Int] = allColours.map(c => c.index -> c.level).toMap
    val finalBags = colours.map(g => FiniteBag(g(rounds)))
    val leaves = finalBags.flatMap(_.items).distinct.map(i => i -> Node.Colour(i))
    val hierarchy =
      Hierarchy
        .build[Node, Int](
          nodes,
          Node.Root,
          {
            case Node.Root          => None
            case Node.Colour(index) =>
              parentOf.getOrElse(index, None) match
                case Some(p) => Some(Node.Colour(p))
                case None    => Some(Node.Root)
          },
          {
            case Node.Root          => NonNegativeFinite.zero
            case Node.Colour(index) =>
              NonNegativeFinite.from(levelOf.getOrElse(index, 0).toDouble + 1.0) match
                case Right(h) => h
                case Left(_)  => NonNegativeFinite.zero
          },
          leaves
        ) match
        case Right(h)    => h
        case Left(error) => throw new IllegalStateException(s"reference hierarchy failed: $error")
    new WLReference(rounds, samples.map(_.key), colours, histograms, hierarchy, finalBags)

  private[laws] def canonical(colours: Vector[Int]): Vector[Int] =
    val seen = scala.collection.mutable.HashMap.empty[Int, Int]
    colours.map(c => seen.getOrElseUpdate(c, seen.size))

  private def evaluateExpr[V, N, E, NodeKey, EdgeKey](
      reference: WLReference[V, N, E, NodeKey, EdgeKey],
      expr: WLKernelExpr[N, E, NodeKey, EdgeKey],
      left: Int,
      right: Int
  ): Double =
    expr match
      case WLKernelExpr.Subtree(_, _, weights)  => reference.subtree(weights)(left, right)
      case WLKernelExpr.OptimalAssignment(_, _) => reference.optimalAssignment(left, right)
      case WLKernelExpr.Scale(_, child, weight) =>
        weight.toDouble * evaluateExpr(reference, child, left, right)
      case WLKernelExpr.Sum(_, l, r) =>
        evaluateExpr(reference, l, left, right) + evaluateExpr(reference, r, left, right)
      case WLKernelExpr.Normalize(_, child) =>
        val ll = evaluateExpr(reference, child, left, left)
        val rr = evaluateExpr(reference, child, right, right)
        if ll <= 0.0 || rr <= 0.0 then 0.0
        else evaluateExpr(reference, child, left, right) / math.sqrt(ll * rr)
