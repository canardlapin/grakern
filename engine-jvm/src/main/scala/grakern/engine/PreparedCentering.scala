package grakern.engine

import gale.linalg.{DMat, DVec, LinAlgError}
import gale.sparse.{DuplicatePolicy, Sparse}
import grakern.core.LabelledNeighbourhood

sealed trait PreparedCenteringError

object PreparedCenteringError:
  case object EmptyReference extends PreparedCenteringError

  final case class GramReferenceMismatch(
      expected: SampleIndex,
      observed: SampleIndex
  ) extends PreparedCenteringError

  final case class GramShapeMismatch(
      expected: Int,
      observedRows: Int,
      observedColumns: Int
  ) extends PreparedCenteringError

  final case class QueryReferenceMismatch(
      expected: SampleIndex,
      observed: SampleIndex
  ) extends PreparedCenteringError

  final case class QueryRowsMismatch(
      features: SampleIndex,
      cross: SampleIndex
  ) extends PreparedCenteringError

  final case class QueryShapeMismatch(
      expectedRows: Int,
      expectedColumns: Int,
      observedRows: Int,
      observedColumns: Int
  ) extends PreparedCenteringError

  case object QueryDictionaryMismatch extends PreparedCenteringError

  final case class FeatureMaterialization(
      error: LinAlgError
  ) extends PreparedCenteringError

/** Statistics frozen against one ordered training corpus.
  *
  * This value is deliberately not a context-free `Kernel`: both the feature and kernel means are
  * properties of `reference` and can only be applied to cross matrices whose reference columns have
  * that exact identity and order.
  */
final case class PreparedCenteringStatistics(
    reference: SampleIndex,
    featureMeans: DVec,
    kernelRowMeans: DVec,
    kernelGrandMean: Double
):
  def centerCross(
      raw: CrossKernelMatrix
  ): Either[PreparedCenteringError, CrossKernelMatrix] =
    PreparedCentering.centerCross(this, raw)

final case class CenteredPreparedViews[Coordinate, +Metadata](
    features: FeatureMatrix[Coordinate, Metadata],
    gram: GramMatrix,
    operator: KernelOperator,
    statistics: PreparedCenteringStatistics
)

final case class CenteredWLQueryResult(
    features: AlignedFeatures[WLFeatureCoordinate],
    cross: CrossKernelMatrix,
    auditEvents: Vector[grakern.core.NumericalAuditEvent],
    executionStats: WLQueryExecutionStats
)

sealed trait CenteredWLQueryError[+V]

object CenteredWLQueryError:
  final case class Query[V](error: WLQueryCompileError[V]) extends CenteredWLQueryError[V]
  final case class Centering(error: PreparedCenteringError) extends CenteredWLQueryError[Nothing]

final class CenteredWLPlanReport private[engine] (
    val source: WLPlanReport,
    val statistics: PreparedCenteringStatistics
):
  def explain: String =
    Vector(
      source.explain,
      "centering:",
      "  reference-bound prepared centering",
      s"  reference samples: ${statistics.reference.size}",
      s"  feature means: ${statistics.featureMeans.length}",
      "  query cross uses frozen training means"
    ).mkString("\n")

/** A centered view of one immutable WL preparation.
  *
  * Training statistics are computed once and reused unchanged for every query. Query-only
  * coordinates retain their norm contribution because their training mean is exactly zero.
  */
final class CenteredWLCompiledKernel[V, N, E, NodeKey, EdgeKey] private[engine] (
    val source: WLCompiledKernel[V, N, E, NodeKey, EdgeKey],
    val views: CenteredPreparedViews[WLFeatureCoordinate, WLFeatureMetadata]
):
  val sampleKeys = source.sampleKeys
  val features = views.features
  val gram = views.gram
  val operator = views.operator
  val capabilities = source.capabilities
  val statistics = views.statistics
  val planReport = new CenteredWLPlanReport(source.planReport, statistics)

  def graphCount: Int = sampleKeys.size

  def kernel(leftGraph: Int, rightGraph: Int): Option[Double] =
    if leftGraph < 0 || rightGraph < 0 then None
    else if leftGraph >= graphCount || rightGraph >= graphCount then None
    else Some(gram.values(leftGraph, rightGraph))

  def cross[QueryV](
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]]
  ): Either[CenteredWLQueryError[QueryV], CenteredWLQueryResult] =
    source
      .cross(samples)
      .left
      .map(CenteredWLQueryError.Query(_))
      .flatMap { raw =>
        PreparedCentering
          .centerQuery(views, raw)
          .left
          .map(CenteredWLQueryError.Centering(_))
      }

private[engine] object PreparedCentering:
  def wl[V, N, E, NodeKey, EdgeKey](
      source: WLCompiledKernel[V, N, E, NodeKey, EdgeKey]
  ): Either[
    PreparedCenteringError,
    CenteredWLCompiledKernel[V, N, E, NodeKey, EdgeKey]
  ] =
    training(source.features, source.gram).map { views =>
      new CenteredWLCompiledKernel(source, views)
    }

  private def training[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      gram: GramMatrix
  ): Either[PreparedCenteringError, CenteredPreparedViews[Coordinate, Metadata]] =
    val size = features.rows.size
    if size == 0 then Left(PreparedCenteringError.EmptyReference)
    else if !sameSamples(gram.samples, features.rows) then
      Left(
        PreparedCenteringError.GramReferenceMismatch(
          features.rows,
          gram.samples
        )
      )
    else if gram.values.rows != size || gram.values.cols != size then
      Left(
        PreparedCenteringError.GramShapeMismatch(
          size,
          gram.values.rows,
          gram.values.cols
        )
      )
    else
      val featureMeans = means(features)
      val kernelRowMeans =
        DVec.tabulate(size) { row =>
          var total = 0.0
          var column = 0
          while column < size do
            total += gram.values(row, column)
            column += 1
          total / size.toDouble
        }
      var grandTotal = 0.0
      var row = 0
      while row < size do
        grandTotal += kernelRowMeans(row)
        row += 1
      val statistics =
        PreparedCenteringStatistics(
          features.rows,
          featureMeans,
          kernelRowMeans,
          grandTotal / size.toDouble
        )
      for centeredFeatures <- centerTrainingFeatures(features, featureMeans)
      yield
        val centeredGram = centerGram(gram, statistics)
        CenteredPreparedViews(
          centeredFeatures,
          centeredGram,
          ExactFeatureNumericalViews.operatorFromGram(centeredGram),
          statistics
        )

  private def means[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata]
  ): DVec =
    val sums = new Array[Double](features.values.cols)
    features.values.foreachStoredEntry { (_, column, value) =>
      sums(column) += value
    }
    DVec.tabulate(sums.length)(column => sums(column) / features.values.rows.toDouble)

  private def centerTrainingFeatures[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      featureMeans: DVec
  ): Either[PreparedCenteringError, FeatureMatrix[Coordinate, Metadata]] =
    val raw = features.values.toDense()
    val builder = Sparse.coo(raw.rows, raw.cols)
    var row = 0
    while row < raw.rows do
      var column = 0
      while column < raw.cols do
        val centered = raw(row, column) - featureMeans(column)
        if centered != 0.0 then builder.add(row, column, centered): Unit
        column += 1
      row += 1
    builder
      .tryToCSR(DuplicatePolicy.Error)
      .left
      .map(PreparedCenteringError.FeatureMaterialization(_))
      .map { values =>
        FeatureMatrix(
          values,
          features.rows,
          features.columns,
          features.metadata
        )
      }

  private def centerGram(
      raw: GramMatrix,
      statistics: PreparedCenteringStatistics
  ): GramMatrix =
    val size = statistics.reference.size
    val builder = DMat.newBuilder(size, size)
    var row = 0
    while row < size do
      var column = 0
      while column < size do
        builder(row, column) = raw.values(row, column) -
          statistics.kernelRowMeans(row) -
          statistics.kernelRowMeans(column) +
          statistics.kernelGrandMean
        column += 1
      row += 1
    GramMatrix(builder.result(), statistics.reference)

  private[engine] def centerCross(
      statistics: PreparedCenteringStatistics,
      raw: CrossKernelMatrix
  ): Either[PreparedCenteringError, CrossKernelMatrix] =
    if !sameSamples(raw.referenceColumns, statistics.reference) then
      Left(
        PreparedCenteringError.QueryReferenceMismatch(
          statistics.reference,
          raw.referenceColumns
        )
      )
    else if raw.values.rows != raw.queryRows.size ||
      raw.values.cols != statistics.reference.size
    then
      Left(
        PreparedCenteringError.QueryShapeMismatch(
          raw.queryRows.size,
          statistics.reference.size,
          raw.values.rows,
          raw.values.cols
        )
      )
    else
      val builder = DMat.newBuilder(raw.values.rows, raw.values.cols)
      var query = 0
      while query < raw.values.rows do
        var queryMean = 0.0
        var reference = 0
        while reference < raw.values.cols do
          queryMean += raw.values(query, reference)
          reference += 1
        queryMean /= raw.values.cols.toDouble
        reference = 0
        while reference < raw.values.cols do
          builder(query, reference) = raw.values(query, reference) -
            queryMean -
            statistics.kernelRowMeans(reference) +
            statistics.kernelGrandMean
          reference += 1
        query += 1
      Right(
        CrossKernelMatrix(
          builder.result(),
          raw.queryRows,
          raw.referenceColumns
        )
      )

  private[engine] def centerQuery(
      training: CenteredPreparedViews[WLFeatureCoordinate, WLFeatureMetadata],
      raw: WLQueryResult
  ): Either[PreparedCenteringError, CenteredWLQueryResult] =
    if !sameSamples(raw.features.rows, raw.cross.queryRows) then
      Left(
        PreparedCenteringError.QueryRowsMismatch(
          raw.features.rows,
          raw.cross.queryRows
        )
      )
    else if !(raw.features.columns eq training.features.columns) then
      Left(PreparedCenteringError.QueryDictionaryMismatch)
    else if raw.features.shared.rows != raw.features.rows.size ||
      raw.features.shared.cols != training.statistics.featureMeans.length ||
      raw.features.novelSquaredNorm.length != raw.features.rows.size
    then
      Left(
        PreparedCenteringError.QueryShapeMismatch(
          raw.features.rows.size,
          training.statistics.featureMeans.length,
          raw.features.shared.rows,
          raw.features.shared.cols
        )
      )
    else
      for
        features <- centerQueryFeatures(training, raw.features)
        cross <- training.statistics.centerCross(raw.cross)
      yield CenteredWLQueryResult(
        features,
        cross,
        raw.auditEvents,
        raw.executionStats
      )

  private def centerQueryFeatures(
      training: CenteredPreparedViews[WLFeatureCoordinate, WLFeatureMetadata],
      raw: AlignedFeatures[WLFeatureCoordinate]
  ): Either[PreparedCenteringError, AlignedFeatures[WLFeatureCoordinate]] =
    val dense = raw.shared.toDense()
    val builder = Sparse.coo(dense.rows, dense.cols)
    var row = 0
    while row < dense.rows do
      var column = 0
      while column < dense.cols do
        val centered = dense(row, column) - training.statistics.featureMeans(column)
        if centered != 0.0 then builder.add(row, column, centered): Unit
        column += 1
      row += 1
    builder
      .tryToCSR(DuplicatePolicy.Error)
      .left
      .map(PreparedCenteringError.FeatureMaterialization(_))
      .map { values =>
        AlignedFeatures(
          values,
          raw.novelSquaredNorm,
          raw.rows,
          training.features.columns
        )
      }

  private def sameSamples(
      left: SampleIndex,
      right: SampleIndex
  ): Boolean =
    left.size == right.size &&
      left.keys.indices.forall(index => left.keys(index).value == right.keys(index).value)
