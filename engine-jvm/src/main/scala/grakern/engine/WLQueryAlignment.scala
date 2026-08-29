package grakern.engine

import gale.linalg.{DMat, DVec, LinAlgError}
import gale.sparse.{CSR, CSRPattern}
import grakern.core.*
import grakern.standard.wl.*

final case class AlignedFeatures[Coordinate](
    shared: CSR,
    novelSquaredNorm: DVec,
    rows: SampleIndex,
    columns: FeatureDictionary[Coordinate]
):
  // Query-aligned rows never need a column-postings index. Building the full training-oriented
  // index here allocated O(training feature columns) storage for every sparse single-row query.
  private lazy val sparseRows = SparseCSRRows(shared)

  def sparseRow(row: Int): Option[SparseFeatureRow] =
    sparseRows.row(row)

  def completeSquaredNorm(row: Int): Option[Double] =
    sparseRow(row).map { sparse =>
      var sharedNorm = 0.0
      var index = 0
      while index < sparse.values.size do
        val value = sparse.values(index)
        sharedNorm += value * value
        index += 1
      sharedNorm + novelSquaredNorm(row)
    }

final case class SparseAlignedFeatures[Coordinate](
    shared: SparseFeatureRow,
    novelSquaredNorm: Double,
    row: SampleKey,
    columns: FeatureDictionary[Coordinate]
):
  def completeSquaredNorm: Double =
    var result = 0.0
    var index = 0
    while index < shared.values.size do
      val value = shared.values(index)
      result += value * value
      index += 1
    result + novelSquaredNorm

final case class CrossKernelMatrix(
    values: DMat,
    queryRows: SampleIndex,
    referenceColumns: SampleIndex
)

final case class SparseCrossKernelRow(
    indices: Vector[Int],
    values: Vector[Double],
    referenceColumns: SampleIndex
):
  require(indices.size == values.size, "sparse indices and values must have equal length")

  private lazy val sparse = SparseFeatureRow(indices, values)

  def activeSize: Int = indices.size

  def valueAt(referenceIndex: Int): Double = sparse.valueAt(referenceIndex)

final case class WLQueryExecutionStats(
    refinementExecutions: Int
)

final case class WLQueryResult(
    features: AlignedFeatures[WLFeatureCoordinate],
    cross: CrossKernelMatrix,
    auditEvents: Vector[NumericalAuditEvent],
    executionStats: WLQueryExecutionStats
)

final case class WLQueryRowResult(
    features: SparseAlignedFeatures[WLFeatureCoordinate],
    cross: SparseCrossKernelRow,
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
  final private case class PreparedQuery(
      features: AlignedFeatures[WLFeatureCoordinate],
      sparseCross: Vector[SparseCrossKernelRow],
      auditEvents: Vector[NumericalAuditEvent],
      executionStats: WLQueryExecutionStats
  )

  def prepare[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLQueryTraining[
        TrainingV,
        N,
        E,
        NodeKey,
        EdgeKey
      ],
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], WLQueryResult] =
    prepareSparse(training, samples).map { prepared =>
      WLQueryResult(
        prepared.features,
        denseCross(prepared.sparseCross, prepared.features.rows, training.features.rows),
        prepared.auditEvents,
        prepared.executionStats
      )
    }

  def prepareSingle[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLQueryTraining[
        TrainingV,
        N,
        E,
        NodeKey,
        EdgeKey
      ],
      sample: LabelledNeighbourhood[QueryV, N, E]
  ): Either[WLQueryCompileError[QueryV], WLQueryRowResult] =
    prepareSparse(training, Vector(sample)).map { prepared =>
      val shared =
        prepared.features.sparseRow(0).getOrElse(SparseFeatureRow(Vector.empty, Vector.empty))
      WLQueryRowResult(
        SparseAlignedFeatures(
          shared,
          prepared.features.novelSquaredNorm(0),
          sample.key,
          prepared.features.columns
        ),
        prepared.sparseCross.head,
        prepared.auditEvents,
        prepared.executionStats
      )
    }

  private def prepareSparse[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLQueryTraining[
        TrainingV,
        N,
        E,
        NodeKey,
        EdgeKey
      ],
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], PreparedQuery] =
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
    yield PreparedQuery(
      aligned,
      sparseCross(aligned, training.features, training.strategy.query),
      auditEvents,
      WLQueryExecutionStats(refinementExecutions)
    )

  private def subtreeRows[V, NodeKey, EdgeKey](
      trace: WLQueryTrace[V, NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite]
  ): Vector[ExactFeatureRow[WLFeatureCoordinate]] =
    // One graph/round can touch no more colours than it has vertices. Keep the counter workspace
    // proportional to the query, not to the potentially huge training signature dictionary.
    val counts = new Array[Int](trace.vertexCount)
    val touchedSlots = new Array[Int](trace.vertexCount)
    val touchedColours = new Array[WLColour](trace.vertexCount)
    Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
      val entries = Vector.newBuilder[(WLFeatureCoordinate, Double)]
      var round = 0
      while round <= trace.rounds do
        val scale = math.sqrt(roundWeights(round).toDouble)
        if scale > 0.0 then
          var touchedSize = 0
          trace.foreachGraphColour(graphIndex, round) { colour =>
            val slot =
              if trace.isNovel(colour) then trace.trainingSignatureCount + colour.index
              else colour.index
            var touchedIndex = 0
            while touchedIndex < touchedSize && touchedSlots(touchedIndex) != slot do
              touchedIndex += 1
            if touchedIndex == touchedSize then
              touchedSlots(touchedSize) = slot
              touchedColours(touchedSize) = colour
              counts(touchedSize) = 1
              touchedSize += 1
            else counts(touchedIndex) += 1
          }
          var touchedIndex = 0
          while touchedIndex < touchedSize do
            val colour = touchedColours(touchedIndex)
            entries += (
              WLFeatureCoordinate(
                Vector.empty,
                WLCompiledFeature.Subtree(
                  WLSubtreeFeature(round, colour)
                )
              ) -> (scale * counts(touchedIndex).toDouble)
            )
            touchedIndex += 1
        round += 1
      ExactFeatureRow(entries.result())
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
    val rowColumns = new Array[Array[Int]](rows.size)
    val rowValues = new Array[Array[Double]](rows.size)
    val rowOffsets = new Array[Int](rows.size + 1)
    val novelNorms = DVec.newBuilder(rows.size)
    var rowIndex = 0
    while rowIndex < rows.size do
      val entries = rows(rowIndex).entries
      val columns = new Array[Int](entries.size)
      val values = new Array[Double](entries.size)
      val novelSquares = new Array[Double](entries.size)
      var sharedCount = 0
      var novelCount = 0
      var entryIndex = 0
      while entryIndex < entries.size do
        val entry = entries(entryIndex)
        val column = dictionary.indexOrNegative(entry._1)
        if column >= 0 then
          columns(sharedCount) = column
          values(sharedCount) = entry._2
          sharedCount += 1
        else
          novelSquares(novelCount) = entry._2 * entry._2
          novelCount += 1
        entryIndex += 1
      sortPairs(columns, values, sharedCount)
      rowColumns(rowIndex) = java.util.Arrays.copyOf(columns, sharedCount)
      rowValues(rowIndex) = java.util.Arrays.copyOf(values, sharedCount)
      rowOffsets(rowIndex + 1) = rowOffsets(rowIndex) + sharedCount
      java.util.Arrays.sort(novelSquares, 0, novelCount)
      var novelSquaredNorm = 0.0
      var squareIndex = 0
      while squareIndex < novelCount do
        novelSquaredNorm += novelSquares(squareIndex)
        squareIndex += 1
      novelNorms(rowIndex) = novelSquaredNorm
      rowIndex += 1
    val columnIndices = new Array[Int](rowOffsets.last)
    var destination = 0
    rowIndex = 0
    while rowIndex < rowColumns.length do
      val columns = rowColumns(rowIndex)
      Array.copy(columns, 0, columnIndices, destination, columns.length)
      destination += columns.length
      rowIndex += 1
    CSRPattern
      .checked(rows.size, dictionary.size, rowOffsets, columnIndices)
      .map { pattern =>
        val values = pattern.valuesBuilder()
        destination = 0
        rowIndex = 0
        while rowIndex < rowValues.length do
          val source = rowValues(rowIndex)
          var sourceIndex = 0
          while sourceIndex < source.length do
            values(destination) = source(sourceIndex)
            destination += 1
            sourceIndex += 1
          rowIndex += 1
        AlignedFeatures(
          values.result(),
          novelNorms.result(),
          SampleIndex(sampleKeys),
          dictionary
        )
      }

  private def sortPairs(indices: Array[Int], values: Array[Double], size: Int): Unit =
    var index = 1
    while index < size do
      val key = indices(index)
      val value = values(index)
      var insertion = index - 1
      while insertion >= 0 && indices(insertion) > key do
        indices(insertion + 1) = indices(insertion)
        values(insertion + 1) = values(insertion)
        insertion -= 1
      indices(insertion + 1) = key
      values(insertion + 1) = value
      index += 1

  private def sparseCross(
      query: AlignedFeatures[WLFeatureCoordinate],
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ],
      realization: WLQueryRealization
  ): Vector[SparseCrossKernelRow] =
    Vector.tabulate(query.shared.rows) { queryIndex =>
      val row = query.sparseRow(queryIndex).getOrElse(SparseFeatureRow(Vector.empty, Vector.empty))
      sparseCrossRow(row, training, realization)
    }

  private def sparseCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ],
      realization: WLQueryRealization
  ): SparseCrossKernelRow =
    realization match
      case WLQueryRealization.FeaturePostings =>
        postingCrossRow(query, training)
      case WLQueryRealization.PairwiseSparseRows =>
        pairwiseCrossRow(query, training)
      case WLQueryRealization.DenseRows =>
        denseCrossRow(query, training)

  private def postingCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ]
  ): SparseCrossKernelRow =
    var contributionCount = 0L
    var queryIndex = 0
    while queryIndex < query.indices.size do
      val column = query.indices(queryIndex)
      contributionCount += training.postingEnd(column) - training.postingStart(column)
      queryIndex += 1
    if contributionCount <= 64L then sparsePostingCrossRow(query, training, contributionCount.toInt)
    else densePostingCrossRow(query, training)

  private def sparsePostingCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ],
      contributionCount: Int
  ): SparseCrossKernelRow =
    val rows = new Array[Int](contributionCount)
    val sums = new Array[Double](contributionCount)
    var touchedSize = 0
    var queryIndex = 0
    while queryIndex < query.indices.size do
      val column = query.indices(queryIndex)
      val queryValue = query.values(queryIndex)
      var postingIndex = training.postingStart(column)
      val postingEnd = training.postingEnd(column)
      while postingIndex < postingEnd do
        val trainingRow = training.postingRow(postingIndex)
        var touchedIndex = 0
        while touchedIndex < touchedSize && rows(touchedIndex) != trainingRow do touchedIndex += 1
        if touchedIndex == touchedSize then
          rows(touchedSize) = trainingRow
          sums(touchedSize) = queryValue * training.postingValue(postingIndex)
          touchedSize += 1
        else sums(touchedIndex) += queryValue * training.postingValue(postingIndex)
        postingIndex += 1
      queryIndex += 1
    sortPairs(rows, sums, touchedSize)
    val indices = Vector.newBuilder[Int]
    val values = Vector.newBuilder[Double]
    var touchedIndex = 0
    while touchedIndex < touchedSize do
      if sums(touchedIndex) != 0.0 then
        indices += rows(touchedIndex)
        values += sums(touchedIndex)
      touchedIndex += 1
    SparseCrossKernelRow(indices.result(), values.result(), training.rows)

  private def densePostingCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ]
  ): SparseCrossKernelRow =
    val sums = new Array[Double](training.rows.size)
    var queryIndex = 0
    while queryIndex < query.indices.size do
      val column = query.indices(queryIndex)
      val queryValue = query.values(queryIndex)
      var postingIndex = training.postingStart(column)
      val postingEnd = training.postingEnd(column)
      while postingIndex < postingEnd do
        val trainingRow = training.postingRow(postingIndex)
        sums(trainingRow) += queryValue * training.postingValue(postingIndex)
        postingIndex += 1
      queryIndex += 1
    val indices = Vector.newBuilder[Int]
    val values = Vector.newBuilder[Double]
    var trainingRow = 0
    while trainingRow < sums.length do
      val value = sums(trainingRow)
      if value != 0.0 then
        indices += trainingRow
        values += value
      trainingRow += 1
    SparseCrossKernelRow(
      indices.result(),
      values.result(),
      training.rows
    )

  private def pairwiseCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ]
  ): SparseCrossKernelRow =
    val indices = Vector.newBuilder[Int]
    val values = Vector.newBuilder[Double]
    var trainingRow = 0
    while trainingRow < training.rows.size do
      val reference =
        training.sparseRow(trainingRow).getOrElse(SparseFeatureRow(Vector.empty, Vector.empty))
      var queryEntry = 0
      var referenceEntry = 0
      var value = 0.0
      while queryEntry < query.indices.size && referenceEntry < reference.indices.size do
        val queryColumn = query.indices(queryEntry)
        val referenceColumn = reference.indices(referenceEntry)
        if queryColumn < referenceColumn then queryEntry += 1
        else if queryColumn > referenceColumn then referenceEntry += 1
        else
          value += query.values(queryEntry) * reference.values(referenceEntry)
          queryEntry += 1
          referenceEntry += 1
      if value != 0.0 then
        indices += trainingRow
        values += value
      trainingRow += 1
    SparseCrossKernelRow(indices.result(), values.result(), training.rows)

  private def denseCrossRow(
      query: SparseFeatureRow,
      training: FeatureMatrix[
        WLFeatureCoordinate,
        WLFeatureMetadata
      ]
  ): SparseCrossKernelRow =
    val lookup = new Array[Double](training.columns.size)
    var entry = 0
    while entry < query.indices.size do
      lookup(query.indices(entry)) = query.values(entry)
      entry += 1
    val indices = Vector.newBuilder[Int]
    val values = Vector.newBuilder[Double]
    var trainingRow = 0
    while trainingRow < training.rows.size do
      val reference =
        training.sparseRow(trainingRow).getOrElse(SparseFeatureRow(Vector.empty, Vector.empty))
      var referenceEntry = 0
      var value = 0.0
      while referenceEntry < reference.indices.size do
        value += reference.values(referenceEntry) * lookup(reference.indices(referenceEntry))
        referenceEntry += 1
      if value != 0.0 then
        indices += trainingRow
        values += value
      trainingRow += 1
    SparseCrossKernelRow(indices.result(), values.result(), training.rows)

  private def denseCross(
      rows: Vector[SparseCrossKernelRow],
      queryRows: SampleIndex,
      referenceColumns: SampleIndex
  ): CrossKernelMatrix =
    val builder = DMat.newBuilder(rows.size, referenceColumns.size)
    var queryIndex = 0
    while queryIndex < rows.size do
      val row = rows(queryIndex)
      var entryIndex = 0
      while entryIndex < row.indices.size do
        builder(queryIndex, row.indices(entryIndex)) = row.values(entryIndex)
        entryIndex += 1
      queryIndex += 1
    CrossKernelMatrix(builder.result(), queryRows, referenceColumns)
