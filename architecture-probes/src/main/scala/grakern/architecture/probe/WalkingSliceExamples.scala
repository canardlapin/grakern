package grakern.architecture.probe

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.histogram.*
import grakern.standard.wl.*
import graph4s.{Graph, Link}

/** Compiling examples for the portable semantic modules.
  *
  * JVM preparation is deliberately absent: the architecture-probes module does not depend on Gale
  * or grakern-engine-jvm.
  */
object WalkingSliceExamples:
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  val sample: GraphSample[Int, Atom, Bond] =
    val topology =
      Graph
        .fromEdges(Link(1, 2), Link(2, 3))
        .toOption
        .get
    GraphSample.build(SampleKey("molecule"), topology)(
      nodeValue = vertex => Atom(if vertex == 2 then "O" else "C"),
      edgeValue = _ => Bond(1)
    )

  val wl: WLKernel[Atom, Bond, String, Int] =
    val refinement =
      WLRefinement
        .create(
          2,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    (
      refinement.subtree +
        refinement.optimalAssignment
    ).normalized

  val vertexHistogram: HistogramRecipe[Atom, Bond, String] =
    Histograms.vertices(
      Projection.of[Atom, String](_.element)
    )
