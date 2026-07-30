package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class CategoricalShortestPathFeaturesSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

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

  private def referenceKernel(
      left: GraphSample[Int, String, Unit],
      right: GraphSample[Int, String, Unit]
  ): Double =
    val leftParts =
      recipe.materializeReference(left).toOption.get.map(_.part)
    val rightParts =
      recipe.materializeReference(right).toOption.get.map(_.part)
    leftParts.iterator
      .map { part =>
        rightParts.count(candidate =>
          CategoricalShortestPathFeatures.sameFeature(
            part,
            candidate
          )
        )
      }
      .sum
      .toDouble

  test("exact feature dots reconstruct the reference convolution"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "pair",
            Vector("C", "O"),
            Vector(0 -> 1)
          )
        ),
        sample(
          GraphSpec(
            "path",
            Vector("C", "O", "C"),
            Vector(0 -> 1, 1 -> 2)
          )
        ),
        sample(
          GraphSpec(
            "disconnected",
            Vector("C", "O", "C"),
            Vector(0 -> 1)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val features =
      CategoricalShortestPathFeatures
        .fromTrace(trace, recipe)
        .toOption
        .get

    samples.indices.foreach { left =>
      samples.indices.foreach { right =>
        assertEquals(
          features.kernel(left, right),
          Some(referenceKernel(samples(left), samples(right)))
        )
      }
    }
    assertEquals(features.apspPlanNodeId.number, trace.planNodeId.number)
    assertEquals(features.recipePlanNodeId.number, recipe.planNodeId.number)

  test("Gale CSR rows retain exact categorical counts"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "triangle",
            Vector("C", "C", "C"),
            Vector(0 -> 1, 1 -> 2, 0 -> 2)
          )
        ),
        sample(
          GraphSpec(
            "pair",
            Vector("C", "C"),
            Vector(0 -> 1)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val features =
      CategoricalShortestPathFeatures
        .fromTrace(trace, recipe)
        .toOption
        .get

    assertEquals(features.stats.graphCount, 2)
    assertEquals(features.stats.featureCount, 1)
    assertEquals(features.stats.nonZeros, 2)
    assertEquals(features.stats.emittedPatterns, 4)
    assertEquals(features.matrix.values(0, 0), 3.0)
    assertEquals(features.matrix.values(1, 0), 1.0)
    assertEquals(features.kernel(0, 1), Some(3.0))

  test("provenance retains every contributing graph-local vertex pair"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "path",
            Vector("C", "N", "C"),
            Vector(0 -> 1, 1 -> 2)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val features =
      CategoricalShortestPathFeatures
        .fromTrace(trace, recipe)
        .toOption
        .get
    val distanceOne =
      features.provenance.find(value => value.feature.distance.toInt == 1)
    val explanation =
      features.explain(0, 0).get

    assertEquals(distanceOne.map(_.witnesses.size), Some(2))
    assert(
      distanceOne.toVector
        .flatMap(_.witnesses)
        .forall(_.sample.value == "path")
    )
    assertEquals(
      explanation.map(_.value).sum,
      features.kernel(0, 0).get
    )

  test("Cats equality resolves endpoint hash collisions exactly"):
    val samples =
      Vector(
        sample(
          GraphSpec(
            "path",
            Vector("C", "N", "O"),
            Vector(0 -> 1, 1 -> 2)
          )
        )
      )
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val collisionHash =
      new Hash[String]:
        def eqv(left: String, right: String): Boolean =
          left == right
        def hash(value: String): Int =
          0
    val features =
      CategoricalShortestPathFeatures
        .fromTrace(trace, recipe)(using collisionHash)
        .toOption
        .get

    assertEquals(features.stats.featureCount, 3)
    assertEquals(features.stats.emittedPatterns, 3)
    assertEquals(features.kernel(0, 0), Some(3.0))

  test("exhaustive small-graph feature dots agree with the reference"):
    val specs =
      (1 to 3).toVector.flatMap { vertexCount =>
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
            Vector.tabulate(vertexCount)(index => if index % 2 == 0 then "A" else "B"),
            edges
          )
        }
      }
    val samples = specs.map(sample)
    val trace =
      ShortestPathAPSP.prepare(samples).toOption.get
    val features =
      CategoricalShortestPathFeatures
        .fromTrace(trace, recipe)
        .toOption
        .get

    samples.indices.foreach { left =>
      samples.indices.foreach { right =>
        assertEquals(
          features.kernel(left, right),
          Some(referenceKernel(samples(left), samples(right))),
          clues(samples(left).key, samples(right).key)
        )
      }
    }
