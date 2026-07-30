package grakern.engine

import gale.linalg.{DMat, DVec, LinAlgError}
import gale.sparse.{CSR, DuplicatePolicy, Sparse}
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*

final case class AlignedFeatures[Coordinate](
    shared: CSR,
    novelSquaredNorm: DVec,
    rows: SampleIndex,
    columns: FeatureDictionary[Coordinate]
):
  def completeSquaredNorm(row: Int): Option[Double] =
    if row < 0 || row >= shared.rows then None
    else
      var sharedNorm = 0.0
      shared.foreachStoredEntry { (entryRow, _, value) =>
        if entryRow == row then sharedNorm += value * value
      }
      Some(sharedNorm + novelSquaredNorm(row))

final case class CrossKernelMatrix(
    values: DMat,
    queryRows: SampleIndex,
    referenceColumns: SampleIndex
)

final case class WLQueryExecutionStats(
    refinementExecutions: Int
)

final case class WLQueryResult(
    features: AlignedFeatures[WLFeatureCoordinate],
    cross: CrossKernelMatrix,
    auditEvents: Vector[NumericalAuditEvent],
    executionStats: WLQueryExecutionStats
)

sealed trait WLQueryCompileError[+V]

object WLQueryCompileError:
  final case class MissingTrainingTrace(node: PlanNodeId) extends WLQueryCompileError[Nothing]

  final case class Trace[V](error: WLQueryTraceError[V]) extends WLQueryCompileError[V]

  final case class Normalization(
      error: ExactFeatureNormalizationError
  ) extends WLQueryCompileError[Nothing]

  final case class FeatureMaterialization(error: LinAlgError) extends WLQueryCompileError[Nothing]

private[grakern] object WLQueryAlignment:
  def prepare[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLCompiledKernel[
        TrainingV,
        N,
        E,
        NodeKey,
        EdgeKey
      ],
      samples: Vector[GraphSample[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], WLQueryResult] =
    var queryTraces = Vector.empty[
      (
          PlanNodeId,
          WLQueryTrace[QueryV, NodeKey, EdgeKey]
      )
    ]
    var refinementExecutions = 0
    var auditEvents = Vector.empty[NumericalAuditEvent]

    def queryTraceFor(
        refinement: WLRefinement[N, E, NodeKey, EdgeKey]
    ): Either[
      WLQueryCompileError[QueryV],
      WLQueryTrace[QueryV, NodeKey, EdgeKey]
    ] =
      queryTraces
        .find(_._1.number == refinement.planNodeId.number)
        .map(_._2) match
        case Some(trace) =>
          Right(trace)
        case None =>
          training
            .trainingTrace(refinement.planNodeId)
            .toRight(
              WLQueryCompileError.MissingTrainingTrace(
                refinement.planNodeId
              )
            )
            .flatMap { trainingTrace =>
              WLQueryTrace
                .prepare(
                  trainingTrace,
                  samples,
                  refinement
                )
                .left
                .map(WLQueryCompileError.Trace(_))
            }
            .map { trace =>
              queryTraces = queryTraces :+ (refinement.planNodeId -> trace)
              refinementExecutions += 1
              trace
            }

    def compile(
        expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
    ): Either[
      WLQueryCompileError[QueryV],
      Vector[ExactFeatureRow[WLFeatureCoordinate]]
    ] =
      expression match
        case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
          queryTraceFor(node.refinement).map { trace =>
            subtreeRows(trace, node.roundWeights)
          }

        case node: WLKernelExpr.OptimalAssignment[
              N,
              E,
              NodeKey,
              EdgeKey
            ] =>
          queryTraceFor(node.refinement).map(assignmentRows)

        case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] =>
          compile(node.child).map { rows =>
            val factor = math.sqrt(node.weight.toDouble)
            if factor == 0.0 then
              Vector.fill(rows.size)(
                ExactFeatureRow(Vector.empty)
              )
            else rows.map(_.scaled(factor))
          }

        case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey] =>
          for
            left <- compile(node.left)
            right <- compile(node.right)
          yield left.zip(right).map { case (leftRow, rightRow) =>
            leftRow.mapKeys(coordinate =>
              coordinate.copy(
                blockPath = 0 +: coordinate.blockPath
              )
            ) ++
              rightRow.mapKeys(coordinate =>
                coordinate.copy(
                  blockPath = 1 +: coordinate.blockPath
                )
              )
          }

        case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] =>
          compile(node.child).flatMap { rows =>
            ExactFeatureRows
              .normalize(
                samples.map(_.key),
                rows,
                training.diagonalTolerance
              )
              .left
              .map(WLQueryCompileError.Normalization(_))
              .map { case (normalized, events) =>
                auditEvents = auditEvents ++ events
                normalized
              }
          }

    for
      rows <- compile(training.expression)
      aligned <- align(
        samples.map(_.key),
        rows,
        training.features.columns
      ).left.map(WLQueryCompileError.FeatureMaterialization(_))
    yield WLQueryResult(
      aligned,
      cross(aligned, training.features),
      auditEvents,
      WLQueryExecutionStats(refinementExecutions)
    )

  private def subtreeRows[V, NodeKey, EdgeKey](
      trace: WLQueryTrace[V, NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite]
  ): Vector[ExactFeatureRow[WLFeatureCoordinate]] =
    Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
      val entries =
        (0 to trace.rounds).flatMap { round =>
          val scale = math.sqrt(roundWeights(round).toDouble)
          if scale == 0.0 then Vector.empty
          else
            counts(
              trace
                .graphColours(graphIndex, round)
                .getOrElse(Vector.empty)
            ).map { case (colour, count) =>
              WLFeatureCoordinate(
                Vector.empty,
                WLCompiledFeature.Subtree(
                  WLSubtreeFeature(round, colour)
                )
              ) -> (scale * count.toDouble)
            }
        }.toVector
      ExactFeatureRow(entries)
    }

  private def assignmentRows[V, NodeKey, EdgeKey](
      trace: WLQueryTrace[V, NodeKey, EdgeKey]
  ): Vector[ExactFeatureRow[WLFeatureCoordinate]] =
    Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
      val finalColours =
        trace
          .graphColours(graphIndex, trace.rounds)
          .getOrElse(Vector.empty)
      val ancestorCounts =
        finalColours.foldLeft(Vector.empty[(WLColour, Int)]) { (current, leaf) =>
          var updated = current
          var ancestor = Option(leaf)
          while ancestor.nonEmpty do
            val colour = ancestor.get
            updated = increment(updated, colour)
            ancestor = trace.signature(colour).flatMap(_.parent)
          updated
        }
      val entries =
        ancestorCounts.flatMap { case (colour, count) =>
          (1 to count).map { threshold =>
            WLFeatureCoordinate(
              Vector.empty,
              WLCompiledFeature.OptimalAssignment(
                HierarchyFeature(
                  WLHierarchyNode.Signature(colour),
                  threshold
                )
              )
            ) -> 1.0
          }
        }
      ExactFeatureRow(entries)
    }

  private def counts(
      values: Vector[WLColour]
  ): Vector[(WLColour, Int)] =
    values.foldLeft(Vector.empty[(WLColour, Int)])(
      increment
    )

  private def increment(
      current: Vector[(WLColour, Int)],
      colour: WLColour
  ): Vector[(WLColour, Int)] =
    val index =
      current.indexWhere(_._1.equals(colour))
    if index < 0 then current :+ (colour -> 1)
    else
      current.updated(
        index,
        colour -> (current(index)._2 + 1)
      )

  private def align(
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[WLFeatureCoordinate]],
      dictionary: FeatureDictionary[WLFeatureCoordinate]
  ): Either[
    LinAlgError,
    AlignedFeatures[WLFeatureCoordinate]
  ] =
    val builder = Sparse.coo(rows.size, dictionary.size)
    val novelNorms = DVec.newBuilder(rows.size)
    rows.zipWithIndex.foreach { case (row, rowIndex) =>
      var novelSquaredNorm = 0.0
      row.entries.foreach { case (coordinate, value) =>
        dictionary.indexOf(coordinate) match
          case Some(columnIndex) =>
            builder.add(rowIndex, columnIndex, value)
          case None =>
            novelSquaredNorm += value * value
      }
      novelNorms(rowIndex) = novelSquaredNorm
    }
    builder
      .tryToCSR(DuplicatePolicy.Error)
      .map { shared =>
        AlignedFeatures(
          shared,
          novelNorms.result(),
          SampleIndex(sampleKeys),
          dictionary
        )
      }

  private def cross(
      query: AlignedFeatures[WLFeatureCoordinate],
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ]
  ): CrossKernelMatrix =
    val builder =
      DMat.newBuilder(query.shared.rows, training.values.rows)
    var queryIndex = 0
    while queryIndex < query.shared.rows do
      val values =
        training.values(query.shared.row(queryIndex))
      var trainingIndex = 0
      while trainingIndex < training.values.rows do
        builder(queryIndex, trainingIndex) = values(trainingIndex)
        trainingIndex += 1
      queryIndex += 1
    CrossKernelMatrix(
      builder.result(),
      query.rows,
      training.rows
    )
