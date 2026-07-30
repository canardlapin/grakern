package grakern.engine

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinearOperator, MutableDVec}
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
  def gram[Coordinate, Metadata](
      features: FeatureMatrix[Coordinate, Metadata],
      rows: Vector[ExactFeatureRow[Coordinate]],
      execution: GramExecution
  ): Either[GramExecutionFailure, GramMatrix] =
    val size = rows.size
    val indexedRows =
      rows.map(
        NumericIndexedRow(
          _,
          features.columns
        )
      )
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
      new NumericIndexedRow(
        columns,
        values
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
