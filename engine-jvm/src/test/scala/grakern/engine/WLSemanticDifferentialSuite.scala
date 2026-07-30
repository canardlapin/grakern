package grakern.engine

import cats.kernel.Hash
import gale.linalg.DVec
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLSemanticDifferentialSuite extends FunSuite:
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  final private case class GraphSpec(
      key: String,
      labels: Vector[String],
      edges: Vector[(Int, Int)]
  )

  private enum ReferenceSignature derives CanEqual:
    case Initial(key: String)
    case Refined(
        previous: ReferenceSignature,
        incident: Vector[(Int, ReferenceSignature)]
    )

  final private case class ReferenceTrace(
      levels: Vector[Vector[ReferenceSignature]]
  ):
    def finalBag: Vector[ReferenceSignature] = levels.last

  private val rounds = 2
  private val subtreeWeight =
    NonNegativeFinite.from(0.7).toOption.get
  private val assignmentWeight =
    NonNegativeFinite.from(0.3).toOption.get
  private val tolerance = 1e-12

  private val specs: Vector[GraphSpec] =
    (1 to 3).toVector.flatMap { vertexCount =>
      val possibleEdges =
        (for
          left <- 0 until vertexCount
          right <- (left + 1) until vertexCount
        yield left -> right).toVector
      val edgeMasks = 0 until (1 << possibleEdges.size)
      val labelMasks = 0 until (1 << vertexCount)
      for
        edgeMask <- edgeMasks.toVector
        labelMask <- labelMasks.toVector
      yield
        val edges =
          possibleEdges.zipWithIndex.collect {
            case (edge, index) if (edgeMask & (1 << index)) != 0 =>
              edge
          }
        val labels =
          Vector.tabulate(vertexCount) { vertex =>
            if (labelMask & (1 << vertex)) == 0 then "A"
            else "B"
          }
        GraphSpec(
          s"n$vertexCount-e$edgeMask-l$labelMask",
          labels,
          edges
        )
    }

  private def samples(
      selected: Vector[GraphSpec]
  ): Vector[GraphSample[Int, Atom, Bond]] =
    selected.map { spec =>
      val topology =
        Graph
          .of(
            spec.labels.indices,
            spec.edges.map { case (left, right) =>
              Link(left, right)
            }
          )
          .toOption
          .get
      GraphSample.build(SampleKey(spec.key), topology)(
        nodeValue = vertex => Atom(spec.labels(vertex)),
        edgeValue = _ => Bond(1)
      )
    }

  private def refinement =
    WLRefinement
      .create(
        rounds,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get

  private def referenceTrace(spec: GraphSpec): ReferenceTrace =
    val incident =
      Vector.tabulate(spec.labels.size) { vertex =>
        spec.edges.flatMap {
          case (`vertex`, right) => Vector(right)
          case (left, `vertex`)  => Vector(left)
          case _                 => Vector.empty
        }
      }
    var levels =
      Vector(
        spec.labels.map(ReferenceSignature.Initial.apply)
      )
    var previous = levels.head
    var round = 1
    while round <= rounds do
      val current =
        Vector.tabulate(spec.labels.size) { vertex =>
          val neighbourhood =
            incident(vertex)
              .map(neighbour => 1 -> previous(neighbour))
              .sortBy { case (edge, signature) =>
                edge -> render(signature)
              }
          ReferenceSignature.Refined(
            previous(vertex),
            neighbourhood
          )
        }
      levels = levels :+ current
      previous = current
      round += 1
    ReferenceTrace(levels)

  private def render(signature: ReferenceSignature): String =
    signature match
      case ReferenceSignature.Initial(key) =>
        s"I(${key.length}:$key)"
      case ReferenceSignature.Refined(previous, incident) =>
        val neighbours =
          incident
            .map { case (edge, neighbour) =>
              s"$edge:${render(neighbour)}"
            }
            .mkString("[", ",", "]")
        s"R(${render(previous)},$neighbours)"

  private def counts[A](values: Vector[A]): Vector[(A, Int)] =
    values.foldLeft(Vector.empty[(A, Int)]) { (current, value) =>
      val index = current.indexWhere(_._1.equals(value))
      if index < 0 then current :+ (value -> 1)
      else
        current.updated(
          index,
          value -> (current(index)._2 + 1)
        )
    }

  private def subtree(
      left: ReferenceTrace,
      right: ReferenceTrace
  ): Double =
    left.levels
      .zip(right.levels)
      .map { case (leftLevel, rightLevel) =>
        val rightCounts = counts(rightLevel)
        counts(leftLevel).map { case (signature, count) =>
          val matching =
            rightCounts
              .find(_._1.equals(signature))
              .fold(0)(_._2)
          count.toDouble * matching.toDouble
        }.sum
      }
      .sum

  private def ancestors(
      signature: ReferenceSignature
  ): Vector[ReferenceSignature] =
    signature match
      case value @ ReferenceSignature.Initial(_) =>
        Vector(value)
      case value @ ReferenceSignature.Refined(previous, _) =>
        value +: ancestors(previous)

  private def height(signature: ReferenceSignature): Double =
    signature match
      case ReferenceSignature.Initial(_) =>
        1.0
      case ReferenceSignature.Refined(previous, _) =>
        height(previous) + 1.0

  private def baseSimilarity(
      left: ReferenceSignature,
      right: ReferenceSignature
  ): Double =
    val rightAncestors = ancestors(right)
    ancestors(left)
      .filter(leftAncestor => rightAncestors.exists(_.equals(leftAncestor)))
      .map(height)
      .maxOption
      .getOrElse(0.0)

  private def assignment(
      left: ReferenceTrace,
      right: ReferenceTrace
  ): Double =
    val size = math.max(left.finalBag.size, right.finalBag.size)
    val leftSlots =
      left.finalBag.map(Some(_)).padTo(size, None)
    val rightSlots =
      right.finalBag.map(Some(_)).padTo(size, None)
    rightSlots.permutations.map { permutation =>
      leftSlots
        .zip(permutation)
        .map {
          case (Some(leftValue), Some(rightValue)) =>
            baseSimilarity(leftValue, rightValue)
          case _ =>
            0.0
        }
        .sum
    }.max

  private def intersection(
      left: ReferenceTrace,
      right: ReferenceTrace
  ): Double =
    val leftCounts =
      counts(left.finalBag.flatMap(ancestors))
    val rightCounts =
      counts(right.finalBag.flatMap(ancestors))
    leftCounts.map { case (signature, leftCount) =>
      val rightCount =
        rightCounts
          .find(_._1.equals(signature))
          .fold(0)(_._2)
      math.min(leftCount, rightCount).toDouble
    }.sum

  private def rawKernel(
      left: ReferenceTrace,
      right: ReferenceTrace
  ): Double =
    subtreeWeight.toDouble * subtree(left, right) +
      assignmentWeight.toDouble * assignment(left, right)

  private def normalizedKernel(
      left: ReferenceTrace,
      right: ReferenceTrace
  ): Double =
    val leftDiagonal = rawKernel(left, left)
    val rightDiagonal = rawKernel(right, right)
    if leftDiagonal == 0.0 || rightDiagonal == 0.0 then 0.0
    else
      rawKernel(left, right) /
        math.sqrt(leftDiagonal * rightDiagonal)

  test("compiled component kernels agree with independent denotations"):
    val corpus = samples(specs)
    val traces = specs.map(referenceTrace)
    val shared = refinement
    val subtreeCompiled =
      WLCompiler.plan(corpus, shared.subtree).run().toOption.get
    val assignmentCompiled =
      WLCompiler
        .plan(corpus, shared.optimalAssignment)
        .run()
        .toOption
        .get

    traces.indices.foreach { left =>
      traces.indices.foreach { right =>
        val referenceAssignment =
          assignment(traces(left), traces(right))
        assertEqualsDouble(
          intersection(traces(left), traces(right)),
          referenceAssignment,
          tolerance,
          clues(specs(left).key, specs(right).key)
        )
        assertEqualsDouble(
          subtreeCompiled.kernel(left, right).get,
          subtree(traces(left), traces(right)),
          tolerance,
          clues(specs(left).key, specs(right).key)
        )
        assertEqualsDouble(
          assignmentCompiled.kernel(left, right).get,
          referenceAssignment,
          tolerance,
          clues(specs(left).key, specs(right).key)
        )
      }
    }

  test("combined Gram and operator views agree with the reference kernel"):
    val corpus = samples(specs)
    val traces = specs.map(referenceTrace)
    val shared = refinement
    val kernel =
      (
        shared.subtree.weighted(subtreeWeight) +
          shared.optimalAssignment.weighted(assignmentWeight)
      ).normalized
    val prepared =
      WLCompiler.plan(corpus, kernel).run().toOption.get

    traces.indices.foreach { left =>
      traces.indices.foreach { right =>
        val expected =
          normalizedKernel(traces(left), traces(right))
        assertEqualsDouble(
          prepared.kernel(left, right).get,
          expected,
          tolerance,
          clues(specs(left).key, specs(right).key)
        )
        assertEqualsDouble(
          prepared.gram.values(left, right),
          expected,
          tolerance,
          clues(specs(left).key, specs(right).key)
        )
      }
    }

    val input =
      DVec.fromSeq(
        traces.indices.map(index => (index % 7).toDouble / 7.0 - 0.25)
      )
    val observed = prepared.operator.values(input)
    traces.indices.foreach { row =>
      val expected =
        traces.indices.map { column =>
          normalizedKernel(traces(row), traces(column)) *
            input(column)
        }.sum
      assertEqualsDouble(
        observed(row),
        expected,
        1e-10,
        clues(specs(row).key)
      )
    }

  test("query alignment agrees with independent normalized cross semantics"):
    val trainingSpecs = specs.zipWithIndex.collect {
      case (spec, index) if index % 3 != 0 => spec
    }
    val querySpecs = specs.zipWithIndex.collect {
      case (spec, index) if index % 3 == 0 => spec
    }
    val trainingTraces = trainingSpecs.map(referenceTrace)
    val queryTraces = querySpecs.map(referenceTrace)
    val shared = refinement
    val kernel =
      (
        shared.subtree.weighted(subtreeWeight) +
          shared.optimalAssignment.weighted(assignmentWeight)
      ).normalized
    val prepared =
      WLCompiler
        .plan(samples(trainingSpecs), kernel)
        .run()
        .toOption
        .get
    val observed =
      prepared.cross(samples(querySpecs)).toOption.get

    queryTraces.indices.foreach { query =>
      trainingTraces.indices.foreach { training =>
        assertEqualsDouble(
          observed.cross.values(query, training),
          normalizedKernel(
            queryTraces(query),
            trainingTraces(training)
          ),
          tolerance,
          clues(
            querySpecs(query).key,
            trainingSpecs(training).key
          )
        )
      }
      assertEqualsDouble(
        observed.features.completeSquaredNorm(query).get,
        1.0,
        tolerance,
        clues(querySpecs(query).key)
      )
    }
    assert(
      observed.features.novelSquaredNorm.toSeq.exists(_ > 0.0),
      "the partitioned court should exercise query-only WL coordinates"
    )
