package grakern.engine

sealed trait GramExecution derives CanEqual:
  def description: String
  private[engine] def stableDescriptor: String

object GramExecution:
  case object Sequential extends GramExecution:
    val description: String =
      "sequential deterministic upper-triangle evaluation"
    private[engine] val stableDescriptor: String =
      "sequential"

  final case class DeterministicParallel private[engine] (
      parallelism: Int,
      tileRows: Int
  ) extends GramExecution:
    val description: String =
      s"deterministic parallel upper-triangle tiles; parallelism=$parallelism; tileRows=$tileRows"
    private[engine] val stableDescriptor: String =
      s"deterministic-parallel:$parallelism:$tileRows"

  def deterministicParallel(
      parallelism: Int,
      tileRows: Int
  ): Either[GramExecutionError, GramExecution] =
    if parallelism <= 0 then
      Left(
        GramExecutionError.NonPositiveParallelism(
          parallelism
        )
      )
    else if tileRows <= 0 then Left(GramExecutionError.NonPositiveTileRows(tileRows))
    else
      Right(
        DeterministicParallel(parallelism, tileRows)
      )

enum GramExecutionError derives CanEqual:
  case NonPositiveParallelism(value: Int)
  case NonPositiveTileRows(value: Int)

enum GramExecutionFailure:
  case Interrupted
  case WorkerFailed(cause: Throwable)
