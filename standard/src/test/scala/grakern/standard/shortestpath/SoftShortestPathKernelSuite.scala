package grakern.standard.shortestpath

import cats.kernel.Eq
import grakern.core.*
import munit.FunSuite

final class SoftShortestPathKernelSuite extends FunSuite:
  final case class Atom(element: String)
  final case class Bond(length: Double)

  given Eq[String] = Eq.fromUniversalEquals

  private val endpointEquality =
    StableId.from("molecule.element.equality").toOption.get
  private val nodeProjection =
    StableId.from("molecule.atom.element").toOption.get
  private val edgeProjection =
    StableId.from("molecule.bond.length").toOption.get

  private val gamma =
    PositiveFinite.from(2.0).toOption.get

  test("named semantic components make the soft kernel durable"):
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.named[Bond, Double](
          edgeProjection,
          StableVersion.initial
        )(_.length)
      )
    val recipe =
      WeightedShortestPaths.parts(
        Projection.named[Atom, String](
          nodeProjection,
          StableVersion.initial
        )(_.element),
        lengths
      )
    val kernel =
      SoftShortestPath.kernel(
        recipe,
        Kernel.deltaNamed[String](endpointEquality),
        Kernel.rbf(gamma)
      )

    assert(
      kernel.fingerprintability
        .isInstanceOf[Fingerprintability.Durable]
    )

  test("normalization is an immutable semantic node"):
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    val kernel =
      SoftShortestPath.kernel(
        WeightedShortestPaths.parts(
          Projection.of[Atom, String](_.element),
          lengths
        ),
        Kernel.delta[String],
        Kernel.rbf(gamma)
      )
    val normalized = kernel.normalized

    assert(
      kernel.planNodeId.number != normalized.planNodeId.number
    )
    assert(
      normalized.fingerprintability
        .isInstanceOf[Fingerprintability.Ephemeral]
    )
