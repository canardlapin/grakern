package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.standard.wl.*
import munit.FunSuite

final class WLFeatureEmitterParitySuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private val subtreeWeight = NonNegativeFinite.from(0.75).toOption.get
  private val assignmentWeight = NonNegativeFinite.from(0.25).toOption.get

  private def refinement =
    WLRefinement
      .create(
        2,
        Projection.named[String, String](
          StableId.from("portable.node-projection").toOption.get,
          StableVersion.initial
        )(identity),
        Projection.named[Int, Int](
          StableId.from("portable.edge-projection").toOption.get,
          StableVersion.initial
        )(identity),
        WLCodecs(
          Some(
            StableCodec.named[String](
              StableId.from("portable.node-codec").toOption.get,
              StableVersion.initial
            )(identity)
          ),
          Some(
            StableCodec.named[Int](
              StableId.from("portable.edge-codec").toOption.get,
              StableVersion.initial
            )(_.toString)
          )
        )
      )
      .toOption
      .get

  private def samples: Vector[NeighbourhoodSample[Int, String, Int]] =
    Vector(
      NeighbourhoodSample
        .undirected(
          SampleKey("pair"),
          Vector(0, 1),
          vertex => if vertex == 0 then "C" else "O",
          Vector((0, 1, 1))
        )
        .toOption
        .get,
      NeighbourhoodSample
        .undirected(
          SampleKey("path"),
          Vector(0, 1, 2),
          vertex => if vertex == 1 then "O" else "C",
          Vector((0, 1, 1), (1, 2, 2))
        )
        .toOption
        .get,
      NeighbourhoodSample
        .undirected(
          SampleKey("isolated"),
          Vector(0),
          _ => "N",
          Vector.empty[(Int, Int, Int)]
        )
        .toOption
        .get
    )

  private def pairedKernel(source: WLRefinement[String, Int, String, Int]) =
    source.subtree.weighted(subtreeWeight) +
      source.optimalAssignment.weighted(assignmentWeight)

  private def asCompiledCoordinate(feature: WLCombinedFeature): WLFeatureCoordinate =
    feature match
      case WLCombinedFeature.Subtree(value) =>
        WLFeatureCoordinate(Vector(0), WLCompiledFeature.Subtree(value))
      case WLCombinedFeature.OptimalAssignment(value) =>
        WLFeatureCoordinate(Vector(1), WLCompiledFeature.OptimalAssignment(value))

  private def asPortableCoordinate[V](
      coordinate: WLFeatureCoordinate,
      trace: WLTrace[V, String, Int]
  ): PortableWLFeatureCoordinate =
    def durable(colour: WLColour): PortableWLSignature =
      PortableWLSignature.Durable(
        trace.signature(colour).flatMap(_.stableDescriptor).get.value
      )

    val portable =
      coordinate.feature match
        case WLCompiledFeature.Subtree(feature) =>
          PortableWLFeature.Subtree(feature.round, durable(feature.signature))
        case WLCompiledFeature.OptimalAssignment(feature) =>
          feature.node match
            case WLHierarchyNode.Signature(colour) =>
              PortableWLFeature.OptimalAssignment(
                colour.level,
                durable(colour),
                feature.multiplicityThreshold
              )
            case WLHierarchyNode.Root =>
              fail("the zero-height hierarchy root must not become an OA coordinate")
    PortableWLFeatureCoordinate(coordinate.blockPath, portable)

  test("portable lowering is bit-identical to the previous engine realization"):
    val shared = refinement
    val kernel = pairedKernel(shared).normalized
    val portable = WLFeatureEmitter.emit(samples, kernel).toOption.get
    val compiled = WLCompiler.plan(samples, kernel).run().toOption.get
    val trace = compiled.trainingTrace(shared.planNodeId).get
    val previous =
      WLCombinedFeatures
        .fromTrace(
          trace,
          Vector.fill(shared.rounds + 1)(NonNegativeFinite.one),
          subtreeWeight,
          assignmentWeight,
          NonNegativeFinite.zero
        )
        .toOption
        .get
    val previousRows = previous.normalizedRows.map(_.mapKeys(asCompiledCoordinate))
    val previousMatrix =
      WLFeatureMatrix.materialize(samples.map(_.key), previousRows).toOption.get

    assertEquals(
      compiled.features.columns.size,
      previousMatrix.columns.size
    )
    compiled.features.columns.coordinates
      .zip(previousMatrix.columns.coordinates)
      .zipWithIndex
      .foreach { case ((observed, expected), column) =>
        assert(
          WLCompiler.sameCoordinate(observed, expected),
          clues(column, observed, expected)
        )
      }
    samples.indices.foreach { rowIndex =>
      val observed = compiled.features.sparseRow(rowIndex).get
      val expected = previousMatrix.sparseRow(rowIndex).get
      assertEquals(observed.indices, expected.indices, clues(rowIndex))
      assertEquals(
        observed.values.map(java.lang.Double.doubleToLongBits),
        expected.values.map(java.lang.Double.doubleToLongBits),
        clues(rowIndex)
      )
    }

    assertEquals(
      compiled.features.columns.coordinates.map(asPortableCoordinate(_, trace)),
      portable.dictionary.coordinates
    )
    portable.rows.indices.foreach { rowIndex =>
      val lowered = compiled.features.sparseRow(rowIndex).get
      val expected = portable.rows(rowIndex)
      assertEquals(lowered.indices, expected.indices)
      assertEquals(
        lowered.values.map(java.lang.Double.doubleToLongBits),
        expected.values.map(java.lang.Double.doubleToLongBits)
      )
    }

  test("portable row cosine equals the normalized engine kernel"):
    val shared = refinement
    val rawKernel = pairedKernel(shared)
    val portable = WLFeatureEmitter.emit(samples, rawKernel).toOption.get
    val normalized = WLCompiler.plan(samples, rawKernel.normalized).run().toOption.get

    samples.indices.foreach { left =>
      samples.indices.foreach { right =>
        assertEqualsDouble(
          portable.rows(left).cosine(portable.rows(right)),
          normalized.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }
