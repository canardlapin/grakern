package grakern.architecture.probe

import cats.instances.int.given
import cats.instances.string.given
import compiletime.testing.typeCheckErrors
import graph4s.Graph
import munit.FunSuite

final class ContractProbeSuite extends FunSuite:
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
        import graph4s.Graph
        import grakern.architecture.probe.*

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
      lawful.evaluate(FiniteBag(Vector("C")), FiniteBag(Vector("C"))),
      1.0
    )

    val errors = typeCheckErrors(
      """
        import grakern.architecture.probe.*

        val gamma = PositiveFinite.from(0.5).toOption.get
        val ordinary: Kernel[Double] = Kernel.rbf(gamma)
        val invalid = OptimalAssignment(ordinary)
      """
    )
    assert(errors.nonEmpty)

  test("ADR-003 anonymous projections cannot obtain durable fingerprints"):
    val durable =
      Projection.named[Int, String](StableId("sample.label"), stableVersion = 1)(
        _.toString
      )
    val fingerprint = SemanticFingerprint.fromProjection(durable)
    assertEquals(fingerprint.value, "sample.label:1")

    val errors = typeCheckErrors(
      """
        import grakern.architecture.probe.*

        val anonymous = Projection.of[Int, String](_.toString)
        val invalid = SemanticFingerprint.fromProjection(anonymous)
      """
    )
    assert(errors.nonEmpty)

  test("ADR-004 decomposition steps receive validated nonnegative weights"):
    val weight = NonNegativeFinite.from(2.0)
    assertEquals(weight.map(_.toDouble), Right(2.0))
    assert(NonNegativeFinite.from(-1.0).isLeft)
    assert(FiniteDouble.from(Double.NaN).isLeft)

    val errors = typeCheckErrors(
      """
        import grakern.architecture.probe.*

        val invalid: NonNegativeFinite = -1.0
      """
    )
    assert(errors.nonEmpty)

  test("ADR-005 exact features are a runtime capability"):
    val prepared = PreparedKernel.pairwiseOnly(4)
    assertEquals(prepared.operator.dimension, 4)
    assert(prepared.features.isLeft)

    val errors = typeCheckErrors(
      """
        import grakern.architecture.probe.*

        val prepared = PreparedKernel.pairwiseOnly(4)
        val invalid: FeatureMatrix = prepared.features
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

  test("ADR-006 colour identifiers are owned by one trace"):
    val training = new RefinementTrace
    val query = new RefinementTrace
    val colour: training.Colour = training.colour(7)

    assertEquals(colour.index, 7)
    assert(training.queryOverlay.owner eq training)
    assert(query.queryOverlay.owner eq query)

    val errors = typeCheckErrors(
      """
        import grakern.architecture.probe.*

        val left = new RefinementTrace
        val right = new RefinementTrace
        val leftColour: left.Colour = left.colour(1)
        val invalid: right.Colour = leftColour
      """
    )
    assert(errors.nonEmpty)
