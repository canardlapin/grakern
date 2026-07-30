package grakern.architecture.probe

import cats.instances.int.given
import cats.instances.string.given
import compiletime.testing.typeCheckErrors
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.Graph
import munit.FunSuite

final class ContractProbeSuite extends FunSuite:
  private val id =
    StableId.from("probe").toOption.get
  private val version = StableVersion.initial

  test("ADR-001 keeps vertex identity separate from node data"):
    val topology = Graph.empty[Int]
    val sample =
      GraphSample.build(SampleKey("molecule-1"), topology)(
        nodeValue = vertex => s"atom-$vertex",
        edgeValue = _ => "bond"
      )

    assertEquals(sample.topology.vertexCount, 0)

    val errors = typeCheckErrors(
      """
        import cats.instances.string.given
        import grakern.core.SampleKey
        import grakern.graph4s.GraphSample
        import graph4s.Graph

        val topology = Graph.empty[String]
        val invalid: GraphSample[Int, String, String] =
          GraphSample.build(SampleKey("bad"), topology)(
            nodeValue = _.toString,
            edgeValue = _ => "bond"
          )
      """
    )
    assert(errors.nonEmpty)

  test("ADR-002 restricts lawful optimal assignment to StrongKernel"):
    val lawful = OptimalAssignment(Kernel.delta[String])
    assertEquals(
      lawful.evaluate(
        FiniteBag.one("C"),
        FiniteBag.one("C")
      ),
      1.0
    )

    val errors = typeCheckErrors(
      """
        import grakern.core.*

        val gamma = PositiveFinite.from(0.5).toOption.get
        val ordinary: Kernel[Double] = Kernel.rbf(gamma)
        val invalid = OptimalAssignment(ordinary)
      """
    )
    assert(errors.nonEmpty)

  test("ADR-003 durable identity requires named projections and codecs"):
    val durable =
      WLRefinement
        .create(
          1,
          Projection.named[Int, String](id, version)(
            _.toString
          ),
          Projection.named[Int, Int](id, version)(identity),
          WLCodecs(
            Some(
              StableCodec.named[String](id, version)(identity)
            ),
            Some(
              StableCodec.named[Int](id, version)(
                _.toString
              )
            )
          )
        )
        .toOption
        .get
    val anonymous =
      WLRefinement
        .create(
          1,
          Projection.of[Int, String](_.toString),
          Projection.of[Int, Int](identity),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get

    assert(
      durable.fingerprintability
        .isInstanceOf[Fingerprintability.Durable]
    )
    assert(
      anonymous.fingerprintability
        .isInstanceOf[Fingerprintability.Ephemeral]
    )

  test("ADR-004 decomposition receives validated nonnegative weights"):
    val decomposition =
      Decomposition.single(
        "pattern",
        NonNegativeFinite.from(2.0).toOption.get
      )
    val total =
      decomposition.fold((), 0.0) { (result, _, weight) =>
        result + weight.toDouble
      }

    assertEquals(total, Right(2.0))
    assert(NonNegativeFinite.from(-1.0).isLeft)
    assert(FiniteDouble.from(Double.NaN).isLeft)

    val errors = typeCheckErrors(
      """
        import grakern.core.NonNegativeFinite

        val invalid: NonNegativeFinite = -1.0
      """
    )
    assert(errors.nonEmpty)

  test("ADR-005 keeps Gale out of semantic modules"):
    val errors = typeCheckErrors(
      """
        import gale.sparse.CSR
      """
    )
    assert(errors.nonEmpty)

  test("ADR-006 colour construction is not a public escape hatch"):
    val errors = typeCheckErrors(
      """
        import grakern.standard.wl.WLColour

        val invalid = new WLColour(null, 1, 0)
      """
    )
    assert(errors.nonEmpty)

  test("the portable walking examples compile against production APIs"):
    assertEquals(
      WalkingSliceExamples.sample.topology.vertexCount,
      3
    )
    assert(
      WalkingSliceExamples.wl.fingerprintability
        .isInstanceOf[Fingerprintability.Ephemeral]
    )
