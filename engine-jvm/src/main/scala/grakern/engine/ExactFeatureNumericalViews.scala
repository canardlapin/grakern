package grakern.engine

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinearOperator, MutableDVec}
import grakern.standard.wl.PortableSparseFeatureRow
import java.util.concurrent.{Callable, ExecutionException, Executors}
import scala.jdk.CollectionConverters.*

final case class GramMatrix(
    values: DMat,
    samples: SampleIndex
)

enum KernelOperatorRealization derives CanEqual:
  case FeatureBacked
  case GramBacked

final case class KernelOperator(
    values: DoubleLinearOperator,
    samples: SampleIndex,
    realization: KernelOperatorRealization
)

private[grakern] object ExactFeatureNumericalViews:
  def gramPortable[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      rows: Vector[PortableSparseFeatureRow],
      execution: GramExecution,
      realization: WLGramRealization
  ): Either[GramExecutionFailure, GramMatrix] =
    realization match
      case WLGramRealization.DenseScratchRows =>
        gramIndexed(
          features,
          rows.map(NumericIndexedRow.fromPortable),
          execution
        )
      case WLGramRealization.SparseFeaturePostings =>
        Right(gramPostings(features, rows))
      case WLGramRealization.DenseRows =>
        Right(gramDenseRows(features, rows))

  def gram[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      rows: Vector[ExactFeatureRow[Coordinate]],
      execution: GramExecution
  ): Either[GramExecutionFailure, GramMatrix] =
    val indexedRows =
      rows.map(
        NumericIndexedRow(
          _,
          features.columns
        )
      )
    gramIndexed(features, indexedRows, execution)

  private def gramIndexed[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      indexedRows: Vector[NumericIndexedRow],
      execution: GramExecution
  ): Either[GramExecutionFailure, GramMatrix] =
    val size = indexedRows.size
    val evaluated =
      execution match
        case GramExecution.Sequential =>
          Right(
            evaluateRows(
              features.columns.size,
              indexedRows,
              0 until size
            )
          )
        case policy: GramExecution.DeterministicParallel =>
          evaluateParallel(
            size,
            features.columns.size,
            indexedRows,
            policy
          )
    evaluated.map { cells =>
      val builder = DMat.newBuilder(size, size)
      cells.foreach { cell =>
        builder(cell.left, cell.right) = cell.value
        if cell.left != cell.right then builder(cell.right, cell.left) = cell.value
      }
      GramMatrix(builder.result(), features.rows)
    }

  final private case class GramCell(
      left: Int,
      right: Int,
      value: Double
  )

  private def gramPostings[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      rows: Vector[PortableSparseFeatureRow]
  ): GramMatrix =
    val size = rows.size
    val rowIndices = new Array[Array[Int]](size)
    val rowValues = new Array[Array[Double]](size)
    val counts = new Array[Int](features.columns.size)
    var nonZeros = 0
    var rowIndex = 0
    while rowIndex < rows.size do
      val row = rows(rowIndex)
      val indices = row.indices.toArray
      val values = row.values.toArray
      rowIndices(rowIndex) = indices
      rowValues(rowIndex) = values
      var entryIndex = 0
      while entryIndex < indices.length do
        counts(indices(entryIndex)) += 1
        nonZeros += 1
        entryIndex += 1
      rowIndex += 1
    val offsets = new Array[Int](features.columns.size + 1)
    var column = 0
    while column < counts.length do
      offsets(column + 1) = offsets(column) + counts(column)
      column += 1
    val cursors = offsets.clone()
    val postingRows = new Array[Int](nonZeros)
    val postingValues = new Array[Double](nonZeros)
    rowIndex = 0
    while rowIndex < rows.size do
      val indices = rowIndices(rowIndex)
      val values = rowValues(rowIndex)
      var entryIndex = 0
      while entryIndex < indices.length do
        val feature = indices(entryIndex)
        val destination = cursors(feature)
        postingRows(destination) = rowIndex
        postingValues(destination) = values(entryIndex)
        cursors(feature) += 1
        entryIndex += 1
      rowIndex += 1
    val cells = Array.fill(size * size)(0.0)
    column = 0
    while column < counts.length do
      val start = offsets(column)
      val end = offsets(column + 1)
      var right = start
      while right < end do
        var left = start
        while left <= right do
          val offset = postingRows(left) * size + postingRows(right)
          cells(offset) += postingValues(left) * postingValues(right)
          left += 1
        right += 1
      column += 1
    val builder = DMat.newBuilder(size, size)
    var matrixRow = 0
    while matrixRow < size do
      var matrixColumn = 0
      while matrixColumn < size do
        val destination = matrixRow * size + matrixColumn
        val source =
          if matrixRow <= matrixColumn then destination
          else matrixColumn * size + matrixRow
        builder.updateRowMajor(destination, cells(source))
        matrixColumn += 1
      matrixRow += 1
    GramMatrix(builder.result(), features.rows)

  private def gramDenseRows[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      rows: Vector[PortableSparseFeatureRow]
  ): GramMatrix =
    val dense = rows.map { row =>
      val values = new Array[Double](features.columns.size)
      var entry = 0
      while entry < row.indices.size do
        values(row.indices(entry)) = row.values(entry)
        entry += 1
      values
    }
    val builder = DMat.newBuilder(rows.size, rows.size)
    var right = 0
    while right < dense.size do
      var left = 0
      while left <= right do
        var column = 0
        var value = 0.0
        while column < features.columns.size do
          value += dense(left)(column) * dense(right)(column)
          column += 1
        builder(left, right) = value
        if left != right then builder(right, left) = value
        left += 1
      right += 1
    GramMatrix(builder.result(), features.rows)

  private def evaluateRows(
      coordinateCount: Int,
      indexedRows: Vector[NumericIndexedRow],
      rightRows: Iterable[Int]
  ): Vector[GramCell] =
    val lookup =
      new Array[Double](coordinateCount)
    val result =
      Vector.newBuilder[GramCell]
    rightRows.foreach { right =>
      indexedRows(right).writeTo(lookup)
      var left = 0
      while left <= right do
        result +=
          GramCell(
            left,
            right,
            indexedRows(left).dot(lookup)
          )
        left += 1
      indexedRows(right).clearFrom(lookup)
    }
    result.result()

  private def evaluateParallel(
      size: Int,
      coordinateCount: Int,
      indexedRows: Vector[NumericIndexedRow],
      policy: GramExecution.DeterministicParallel
  ): Either[GramExecutionFailure, Vector[GramCell]] =
    val tiles =
      (0 until size)
        .grouped(policy.tileRows)
        .map(_.toVector)
        .toVector
    val executor =
      Executors.newFixedThreadPool(policy.parallelism)
    try
      val tasks =
        tiles.map { tile =>
          new Callable[Vector[GramCell]]:
            def call(): Vector[GramCell] =
              evaluateRows(
                coordinateCount,
                indexedRows,
                tile
              )
        }
      Right(
        executor
          .invokeAll(tasks.asJava)
          .asScala
          .toVector
          .flatMap(_.get())
      )
    catch
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        Left(GramExecutionFailure.Interrupted)
      case error: ExecutionException =>
        Left(
          GramExecutionFailure.WorkerFailed(
            Option(error.getCause()).getOrElse(error)
          )
        )
    finally executor.shutdown()

  final private class NumericIndexedRow private (
      val columns: Array[Int],
      val values: Array[Double]
  ):
    def dot(lookup: Array[Double]): Double =
      var index = 0
      var result = 0.0
      while index < columns.length do
        result += values(index) *
          lookup(columns(index))
        index += 1
      result

    def writeTo(lookup: Array[Double]): Unit =
      var index = 0
      while index < columns.length do
        lookup(columns(index)) = values(index)
        index += 1

    def clearFrom(lookup: Array[Double]): Unit =
      var index = 0
      while index < columns.length do
        lookup(columns(index)) = 0.0
        index += 1

  private object NumericIndexedRow:
    def fromPortable(row: PortableSparseFeatureRow): NumericIndexedRow =
      new NumericIndexedRow(row.indices.toArray, row.values.toArray)

    def apply[Coordinate](
        row: ExactFeatureRow[Coordinate],
        dictionary: FeatureDictionary[Coordinate]
    ): NumericIndexedRow =
      val columns =
        new Array[Int](row.entries.size)
      val values =
        new Array[Double](row.entries.size)
      var index = 0
      while index < row.entries.size do
        val entry = row.entries(index)
        val column =
          dictionary.indexOrNegative(entry._1)
        columns(index) = column
        values(index) = entry._2
        index += 1
      // Row entry order follows feature discovery, which in turn follows graph enumeration.
      // Canonicalize by the corpus dictionary before any dot reduction so isomorphic samples use
      // the same floating-point operation order, including on Gram diagonals.
      val order = columns.indices.toArray.sortBy(columns)
      val sortedColumns = new Array[Int](columns.length)
      val sortedValues = new Array[Double](values.length)
      index = 0
      while index < order.length do
        val source = order(index)
        sortedColumns(index) = columns(source)
        sortedValues(index) = values(source)
        index += 1
      new NumericIndexedRow(
        sortedColumns,
        sortedValues
      )

  def operator[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata]
  ): KernelOperator =
    val values = features.values
    val operator =
      LinearOperator.fromFunction(values.rows, values.rows) { (input: DVec, output: MutableDVec) =>
        val featureWeights = MutableDVec.zeros(values.cols)
        values.transposeApplyTo(input, featureWeights)
        values.applyTo(featureWeights.toVec, output)
      }
    KernelOperator(
      operator,
      features.rows,
      KernelOperatorRealization.FeatureBacked
    )

  def operatorFromGram(
      gram: GramMatrix
  ): KernelOperator =
    val values = gram.values
    val operator =
      LinearOperator.fromFunction(
        values.rows,
        values.cols
      ) { (input: DVec, output: MutableDVec) =>
        values.applyTo(input, output)
      }
    KernelOperator(
      operator,
      gram.samples,
      KernelOperatorRealization.GramBacked
    )
