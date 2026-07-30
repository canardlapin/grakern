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
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean,
      execution: GramExecution
  ): Either[GramExecutionFailure, GramMatrix] =
    val size = rows.size
    val evaluated =
      execution match
        case GramExecution.Sequential =>
          Right(evaluateRows(rows, sameCoordinate, 0 until size))
        case policy: GramExecution.DeterministicParallel =>
          evaluateParallel(rows, sameCoordinate, policy)
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

  private def evaluateRows[Coordinate](
      rows: Vector[ExactFeatureRow[Coordinate]],
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean,
      leftRows: Iterable[Int]
  ): Vector[GramCell] =
    val size = rows.size
    leftRows.iterator.flatMap { left =>
      (left until size).iterator.map { right =>
        GramCell(
          left,
          right,
          rows(left).dot(rows(right))(
            sameCoordinate
          )
        )
      }
    }.toVector

  private def evaluateParallel[Coordinate](
      rows: Vector[ExactFeatureRow[Coordinate]],
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean,
      policy: GramExecution.DeterministicParallel
  ): Either[GramExecutionFailure, Vector[GramCell]] =
    val tiles =
      (0 until rows.size)
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
              evaluateRows(rows, sameCoordinate, tile)
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
