package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.*
import scala.collection.mutable
import scala.util.Sorting

enum PortableWLSignature:
  case Durable(fingerprint: String)
  case Ephemeral(colour: WLColour)

enum PortableWLFeature:
  case Subtree(
      round: Int,
      signature: PortableWLSignature
  )
  case OptimalAssignment(
      level: Int,
      signature: PortableWLSignature,
      multiplicityThreshold: Int
  )

final case class PortableWLFeatureCoordinate(
    blockPath: Vector[Int],
    feature: PortableWLFeature
)

final case class PortableSparseFeatureRow(
    indices: Vector[Int],
    values: Vector[Double]
):
  require(indices.size == values.size, "sparse indices and values must have equal length")

  def activeSize: Int = indices.size

  def squaredNorm: Double =
    val squares = values.iterator.map(value => value * value).toArray
    Sorting.quickSort(squares)
    squares.sum

  def dot(that: PortableSparseFeatureRow): Double =
    var left = 0
    var right = 0
    var result = 0.0
    while left < indices.size && right < that.indices.size do
      val leftIndex = indices(left)
      val rightIndex = that.indices(right)
      if leftIndex < rightIndex then left += 1
      else if leftIndex > rightIndex then right += 1
      else
        result += values(left) * that.values(right)
        left += 1
        right += 1
    result

  def cosine(that: PortableSparseFeatureRow): Double =
    val leftNorm = squaredNorm
    val rightNorm = that.squaredNorm
    if leftNorm <= 0.0 || rightNorm <= 0.0 then 0.0
    else dot(that) / math.sqrt(leftNorm * rightNorm)

enum WLFeatureDictionaryDurability derives CanEqual:
  case Durable
  case Ephemeral

final case class WLSerializedFeatureCoordinate(
    blockPath: Vector[Int],
    component: String,
    round: Int,
    signatureFingerprint: String,
    multiplicityThreshold: Option[Int]
)

enum WLFeatureDictionaryError derives CanEqual:
  case EphemeralCoordinate(index: Int)
  case UnknownComponent(index: Int, component: String)
  case InvalidMultiplicityThreshold(index: Int, value: Option[Int])
  case DuplicateCoordinate(index: Int, previousIndex: Int)

final class WLFeatureDictionary private (
    val coordinates: Vector[PortableWLFeatureCoordinate],
    private[grakern] val origins: Vector[WLColour]
):
  // Engine preparation consumes dictionary columns positionally. Build the reverse lookup only
  // for callers that actually align an external portable row against this dictionary.
  private lazy val indices = coordinates.zipWithIndex.toMap

  val durability: WLFeatureDictionaryDurability =
    if coordinates.forall(WLFeatureDictionary.isDurable) then WLFeatureDictionaryDurability.Durable
    else WLFeatureDictionaryDurability.Ephemeral

  def size: Int = coordinates.size

  def coordinate(index: Int): Option[PortableWLFeatureCoordinate] = coordinates.lift(index)

  def indexOf(coordinate: PortableWLFeatureCoordinate): Option[Int] = indices.get(coordinate)

  def serializableCoordinates
      : Either[WLFeatureDictionaryError, Vector[WLSerializedFeatureCoordinate]] =
    val result = Vector.newBuilder[WLSerializedFeatureCoordinate]
    var index = 0
    while index < coordinates.size do
      WLFeatureDictionary.serialize(coordinates(index)) match
        case Some(value) => result += value
        case None        => return Left(WLFeatureDictionaryError.EphemeralCoordinate(index))
      index += 1
    Right(result.result())

  private[grakern] def origin(index: Int): Option[WLColour] =
    origins.lift(index)

object WLFeatureDictionary:
  def fromSerialized(
      serialized: Vector[WLSerializedFeatureCoordinate]
  ): Either[WLFeatureDictionaryError, WLFeatureDictionary] =
    val coordinates = Vector.newBuilder[PortableWLFeatureCoordinate]
    val observed = mutable.HashMap.empty[PortableWLFeatureCoordinate, Int]
    var index = 0
    while index < serialized.size do
      deserialize(index, serialized(index)) match
        case Left(error)       => return Left(error)
        case Right(coordinate) =>
          observed.get(coordinate) match
            case Some(previous) =>
              return Left(WLFeatureDictionaryError.DuplicateCoordinate(index, previous))
            case None =>
              observed.update(coordinate, index)
              coordinates += coordinate
      index += 1
    val values = coordinates.result()
    Right(new WLFeatureDictionary(values, Vector.empty))

  private[wl] def build(
      coordinates: Vector[PortableWLFeatureCoordinate],
      origins: Vector[WLColour]
  ): WLFeatureDictionary =
    new WLFeatureDictionary(coordinates, origins)

  private def isDurable(coordinate: PortableWLFeatureCoordinate): Boolean =
    coordinate.feature match
      case PortableWLFeature.Subtree(_, PortableWLSignature.Durable(_))              => true
      case PortableWLFeature.OptimalAssignment(_, PortableWLSignature.Durable(_), _) => true
      case _                                                                         => false

  private def serialize(
      coordinate: PortableWLFeatureCoordinate
  ): Option[WLSerializedFeatureCoordinate] =
    coordinate.feature match
      case PortableWLFeature.Subtree(round, PortableWLSignature.Durable(fingerprint)) =>
        Some(
          WLSerializedFeatureCoordinate(
            coordinate.blockPath,
            "subtree",
            round,
            fingerprint,
            None
          )
        )
      case PortableWLFeature.OptimalAssignment(
            level,
            PortableWLSignature.Durable(fingerprint),
            threshold
          ) =>
        Some(
          WLSerializedFeatureCoordinate(
            coordinate.blockPath,
            "optimal-assignment",
            level,
            fingerprint,
            Some(threshold)
          )
        )
      case _ => None

  private def deserialize(
      index: Int,
      serialized: WLSerializedFeatureCoordinate
  ): Either[WLFeatureDictionaryError, PortableWLFeatureCoordinate] =
    serialized.component match
      case "subtree" =>
        serialized.multiplicityThreshold match
          case None =>
            Right(
              PortableWLFeatureCoordinate(
                serialized.blockPath,
                PortableWLFeature.Subtree(
                  serialized.round,
                  PortableWLSignature.Durable(serialized.signatureFingerprint)
                )
              )
            )
          case value =>
            Left(WLFeatureDictionaryError.InvalidMultiplicityThreshold(index, value))
      case "optimal-assignment" =>
        serialized.multiplicityThreshold match
          case Some(value) if value > 0 =>
            Right(
              PortableWLFeatureCoordinate(
                serialized.blockPath,
                PortableWLFeature.OptimalAssignment(
                  serialized.round,
                  PortableWLSignature.Durable(serialized.signatureFingerprint),
                  value
                )
              )
            )
          case value =>
            Left(WLFeatureDictionaryError.InvalidMultiplicityThreshold(index, value))
      case other => Left(WLFeatureDictionaryError.UnknownComponent(index, other))

final case class WLAlignedFeatureRows(
    rows: Vector[PortableSparseFeatureRow],
    novelSquaredNorm: Vector[Double]
)

final case class WLFeatureEmission(
    sampleKeys: Vector[SampleKey],
    rows: Vector[PortableSparseFeatureRow],
    dictionary: WLFeatureDictionary,
    auditEvents: Vector[NumericalAuditEvent]
):
  def alignTo(reference: WLFeatureDictionary): WLAlignedFeatureRows =
    val alignedRows = Vector.newBuilder[PortableSparseFeatureRow]
    val novelNorms = Vector.newBuilder[Double]
    rows.foreach { row =>
      val shared = mutable.ArrayBuffer.empty[(Int, Double)]
      val novelSquares = mutable.ArrayBuffer.empty[Double]
      var entry = 0
      while entry < row.indices.size do
        val coordinate = dictionary.coordinates(row.indices(entry))
        val value = row.values(entry)
        reference.indexOf(coordinate) match
          case Some(referenceIndex) => shared += (referenceIndex -> value)
          case None                 => novelSquares += value * value
        entry += 1
      val sorted = shared.sortBy(_._1).toVector
      val squares = novelSquares.toArray
      Sorting.quickSort(squares)
      alignedRows += PortableSparseFeatureRow(sorted.map(_._1), sorted.map(_._2))
      novelNorms += squares.sum
    }
    WLAlignedFeatureRows(alignedRows.result(), novelNorms.result())

enum WLFeatureEmissionError[+V]:
  case UnknownEndpoint(
      sample: SampleKey,
      vertex: V,
      other: V
  )
  case Interning(
      sample: SampleKey,
      round: Int,
      vertex: V,
      error: WLInternerError
  )
  case MissingPreparedTrace(node: PlanNodeId)
  case NonFiniteDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double
  )
  case NegativeDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double,
      tolerance: NonNegativeFinite
  )

private[grakern] trait WLFeatureTrace[NodeKey, EdgeKey]:
  def sampleKeys: Vector[SampleKey]
  def rounds: Int
  def allColours: Vector[WLColour]
  def graphColours(graphIndex: Int, round: Int): Option[Vector[WLColour]]
  def foreachGraphColour(graphIndex: Int, round: Int)(f: WLColour => Unit): Boolean =
    graphColours(graphIndex, round) match
      case Some(colours) =>
        colours.foreach(f)
        true
      case None => false
  def signature(colour: WLColour): Option[WLSignature[NodeKey, EdgeKey]]

object WLFeatureEmitter:
  def emit[V, N, E, NodeKey: Hash, EdgeKey: Hash](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      diagonalTolerance: NonNegativeFinite = NonNegativeFinite.zero
  ): Either[WLFeatureEmissionError[V], WLFeatureEmission] =
    val prepared = Vector.newBuilder[(PlanNodeId, WLFeatureTrace[NodeKey, EdgeKey])]
    val required = refinements(kernel.expression)
    var index = 0
    while index < required.size do
      trace(samples, required(index)) match
        case Left(error)  => return Left(error)
        case Right(value) => prepared += (required(index).planNodeId -> value)
      index += 1
    emitPrepared(
      samples.map(_.key),
      kernel,
      diagonalTolerance,
      prepared.result(),
      durableCoordinates = true
    )

  private[grakern] def emitPrepared[N, E, NodeKey, EdgeKey](
      sampleKeys: Vector[SampleKey],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      diagonalTolerance: NonNegativeFinite,
      prepared: Vector[(PlanNodeId, WLFeatureTrace[NodeKey, EdgeKey])]
  ): Either[WLFeatureEmissionError[Nothing], WLFeatureEmission] =
    emitPrepared(
      sampleKeys,
      kernel,
      diagonalTolerance,
      prepared,
      durableCoordinates = false
    )

  private def emitPrepared[N, E, NodeKey, EdgeKey](
      sampleKeys: Vector[SampleKey],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      diagonalTolerance: NonNegativeFinite,
      prepared: Vector[(PlanNodeId, WLFeatureTrace[NodeKey, EdgeKey])],
      durableCoordinates: Boolean
  ): Either[WLFeatureEmissionError[Nothing], WLFeatureEmission] =
    val featureIds = new FeatureIdAllocator

    def traceFor(
        refinement: WLRefinement[N, E, NodeKey, EdgeKey]
    ): Either[WLFeatureEmissionError[Nothing], WLFeatureTrace[NodeKey, EdgeKey]] =
      prepared
        .find(_._1.number == refinement.planNodeId.number)
        .map(_._2)
        .toRight(WLFeatureEmissionError.MissingPreparedTrace(refinement.planNodeId))

    compile(
      kernel.expression,
      traceFor,
      sampleKeys,
      diagonalTolerance,
      durableCoordinates,
      featureIds
    ).map { case (rawRows, events) =>
      materialize(sampleKeys, rawRows, events, featureIds.size)
    }

  final private class FeatureIdAllocator:
    private var allocated = 0

    def size: Int = allocated

    def next(): Int =
      val result = allocated
      allocated += 1
      result

  final private case class RawEntry(
      coordinate: PortableWLFeatureCoordinate,
      origin: WLColour,
      value: Double,
      featureId: Int
  )

  private type RawRows = Vector[Vector[RawEntry]]

  final private class PortableTrace[NodeKey, EdgeKey](
      val sampleKeys: Vector[SampleKey],
      val rounds: Int,
      private val levels: Vector[Vector[Vector[WLColour]]],
      private val interner: WLInterner[NodeKey, EdgeKey]
  ) extends WLFeatureTrace[NodeKey, EdgeKey]:
    def allColours: Vector[WLColour] = interner.colours
    def graphColours(graphIndex: Int, round: Int): Option[Vector[WLColour]] =
      levels.lift(graphIndex).flatMap(_.lift(round))
    override def foreachGraphColour(graphIndex: Int, round: Int)(
        f: WLColour => Unit
    ): Boolean =
      levels.lift(graphIndex).flatMap(_.lift(round)) match
        case Some(colours) =>
          var index = 0
          while index < colours.size do
            f(colours(index))
            index += 1
          true
        case None => false
    def signature(colour: WLColour): Option[WLSignature[NodeKey, EdgeKey]] =
      interner.signature(colour)

  private def trace[V, N, E, NodeKey: Hash, EdgeKey: Hash](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Either[WLFeatureEmissionError[V], WLFeatureTrace[NodeKey, EdgeKey]] =
    val builder = WLInternerBuilder.empty(refinement.codecs, refinement.hashing)
    val positions = samples.map { sample =>
      given Hash[V] = sample.vertexHash
      sample.vertices.iterator.zipWithIndex.map { case (vertex, index) =>
        HashKey(vertex) -> index
      }.toMap
    }
    val initial = samples.map(sample =>
      sample.vertices.toVector.map(vertex =>
        builder.internInitial(refinement.nodeKey(sample.nodeValue(vertex)))
      )
    )
    var levels = initial.map(Vector(_))
    var round = 1
    while round <= refinement.rounds do
      val next = Vector.newBuilder[Vector[WLColour]]
      var graph = 0
      while graph < samples.size do
        val sample = samples(graph)
        given Hash[V] = sample.vertexHash
        val previous = levels(graph).last
        val colours = Vector.newBuilder[WLColour]
        var vertexIndex = 0
        while vertexIndex < sample.vertices.size do
          val vertex = sample.vertices(vertexIndex)
          val neighbourhood = Vector.newBuilder[WLIncidence[EdgeKey]]
          val incidences = sample.incidences(vertex)
          var incidenceIndex = 0
          while incidenceIndex < incidences.size do
            val incidence = incidences(incidenceIndex)
            positions(graph).get(HashKey(incidence.other)) match
              case None =>
                return Left(
                  WLFeatureEmissionError.UnknownEndpoint(sample.key, vertex, incidence.other)
                )
              case Some(other) =>
                neighbourhood += WLIncidence(
                  refinement.edgeKey(incidence.value),
                  incidence.orientation,
                  previous(other)
                )
            incidenceIndex += 1
          builder.internRefined(previous(vertexIndex), neighbourhood.result()) match
            case Left(error) =>
              return Left(WLFeatureEmissionError.Interning(sample.key, round, vertex, error))
            case Right(colour) => colours += colour
          vertexIndex += 1
        next += colours.result()
        graph += 1
      val completed = next.result()
      levels = levels.zip(completed).map { case (observed, current) => observed :+ current }
      round += 1
    Right(
      new PortableTrace(
        samples.map(_.key),
        refinement.rounds,
        levels,
        builder.freeze()
      )
    )

  private def refinements[N, E, NodeKey, EdgeKey](
      expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
  ): Vector[WLRefinement[N, E, NodeKey, EdgeKey]] =
    val result = mutable.LinkedHashMap.empty[Long, WLRefinement[N, E, NodeKey, EdgeKey]]
    def visit(current: WLKernelExpr[N, E, NodeKey, EdgeKey]): Unit =
      current match
        case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
          result.getOrElseUpdate(node.refinement.planNodeId.number, node.refinement): Unit
        case node: WLKernelExpr.OptimalAssignment[N, E, NodeKey, EdgeKey] =>
          result.getOrElseUpdate(node.refinement.planNodeId.number, node.refinement): Unit
        case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] => visit(node.child)
        case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey]   =>
          visit(node.left)
          visit(node.right)
        case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] => visit(node.child)
    visit(expression)
    result.values.toVector

  private def compile[N, E, NodeKey, EdgeKey](
      expression: WLKernelExpr[N, E, NodeKey, EdgeKey],
      traceFor: WLRefinement[N, E, NodeKey, EdgeKey] => Either[
        WLFeatureEmissionError[Nothing],
        WLFeatureTrace[NodeKey, EdgeKey]
      ],
      sampleKeys: Vector[SampleKey],
      diagonalTolerance: NonNegativeFinite,
      durableCoordinates: Boolean,
      featureIds: FeatureIdAllocator
  ): Either[WLFeatureEmissionError[Nothing], (RawRows, Vector[NumericalAuditEvent])] =
    expression match
      case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
        traceFor(node.refinement).map(trace =>
          subtreeRows(
            trace,
            node.roundWeights,
            durableCoordinates,
            featureIds
          ) -> Vector.empty
        )
      case node: WLKernelExpr.OptimalAssignment[N, E, NodeKey, EdgeKey] =>
        traceFor(node.refinement).map(trace =>
          assignmentRows(trace, durableCoordinates, featureIds) -> Vector.empty
        )
      case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] =>
        compile(
          node.child,
          traceFor,
          sampleKeys,
          diagonalTolerance,
          durableCoordinates,
          featureIds
        ).map { case (rows, events) =>
          val factor = math.sqrt(node.weight.toDouble)
          val scaled =
            if factor == 0.0 then Vector.fill(rows.size)(Vector.empty)
            else rows.map(_.map(entry => entry.copy(value = factor * entry.value)))
          scaled -> events
        }
      case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey] =>
        for
          left <- compile(
            node.left,
            traceFor,
            sampleKeys,
            diagonalTolerance,
            durableCoordinates,
            featureIds
          )
          right <- compile(
            node.right,
            traceFor,
            sampleKeys,
            diagonalTolerance,
            durableCoordinates,
            featureIds
          )
        yield
          val rows = left._1.zip(right._1).map { case (leftRow, rightRow) =>
            leftRow.map(entry =>
              entry.copy(coordinate =
                entry.coordinate.copy(blockPath = 0 +: entry.coordinate.blockPath)
              )
            ) ++ rightRow.map(entry =>
              entry.copy(coordinate =
                entry.coordinate.copy(blockPath = 1 +: entry.coordinate.blockPath)
              )
            )
          }
          rows -> (left._2 ++ right._2)
      case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] =>
        compile(
          node.child,
          traceFor,
          sampleKeys,
          diagonalTolerance,
          durableCoordinates,
          featureIds
        ).flatMap { case (rows, previousEvents) =>
          normalize(sampleKeys, rows, diagonalTolerance).map { case (normalized, events) =>
            normalized -> (previousEvents ++ events)
          }
        }

  private def subtreeRows[NodeKey, EdgeKey](
      trace: WLFeatureTrace[NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite],
      durableCoordinates: Boolean,
      featureIds: FeatureIdAllocator
  ): RawRows =
    val allColours = trace.allColours
    val signatures = allColours.map(colour => signature(trace, colour, durableCoordinates))
    val coordinates = allColours.map { colour =>
      PortableWLFeatureCoordinate(
        Vector.empty,
        PortableWLFeature.Subtree(colour.level, signatures(colour.index))
      )
    }
    val colourFeatureIds = new Array[Int](allColours.size)
    var featureIndex = 0
    while featureIndex < colourFeatureIds.length do
      colourFeatureIds(featureIndex) = featureIds.next()
      featureIndex += 1
    val counts = Array.fill(allColours.size)(0)
    val touched = new Array[Int](allColours.size)
    Vector.tabulate(trace.sampleKeys.size) { graph =>
      val entries = Vector.newBuilder[RawEntry]
      var round = 0
      while round <= trace.rounds do
        val scale = math.sqrt(roundWeights(round).toDouble)
        if scale > 0.0 then
          var touchedSize = 0
          trace.foreachGraphColour(graph, round) { colour =>
            if counts(colour.index) == 0 then
              touched(touchedSize) = colour.index
              touchedSize += 1
            counts(colour.index) += 1
          }
          var touchedIndex = 0
          while touchedIndex < touchedSize do
            val colourIndex = touched(touchedIndex)
            val colour = allColours(colourIndex)
            entries += RawEntry(
              coordinates(colourIndex),
              colour,
              scale * counts(colourIndex).toDouble,
              colourFeatureIds(colourIndex)
            )
            counts(colourIndex) = 0
            touchedIndex += 1
        round += 1
      entries.result()
    }

  private def assignmentRows[NodeKey, EdgeKey](
      trace: WLFeatureTrace[NodeKey, EdgeKey],
      durableCoordinates: Boolean,
      featureIds: FeatureIdAllocator
  ): RawRows =
    val allColours = trace.allColours
    val signatures = allColours.map(colour => signature(trace, colour, durableCoordinates))
    val coordinates =
      allColours.map(_ => mutable.ArrayBuffer.empty[(PortableWLFeatureCoordinate, Int)])
    def coordinate(
        colour: WLColour,
        threshold: Int
    ): (PortableWLFeatureCoordinate, Int) =
      val observed = coordinates(colour.index)
      while observed.size < threshold do
        observed += (
          PortableWLFeatureCoordinate(
            Vector.empty,
            PortableWLFeature.OptimalAssignment(
              colour.level,
              signatures(colour.index),
              observed.size + 1
            )
          ) -> featureIds.next()
        )
      observed(threshold - 1)

    Vector.tabulate(trace.sampleKeys.size) { graph =>
      val counts = Array.fill(allColours.size)(0)
      trace.foreachGraphColour(graph, trace.rounds) { leaf =>
        var current = Option(leaf)
        while current.nonEmpty do
          val colour = current.get
          counts(colour.index) += 1
          current = trace.signature(colour).flatMap(_.parent)
      }
      val entries = Vector.newBuilder[RawEntry]
      var colourIndex = 0
      while colourIndex < counts.length do
        val colour = allColours(colourIndex)
        var threshold = 1
        while threshold <= counts(colourIndex) do
          val feature = coordinate(colour, threshold)
          entries += RawEntry(
            feature._1,
            colour,
            1.0,
            feature._2
          )
          threshold += 1
        colourIndex += 1
      entries.result()
    }

  private def signature[NodeKey, EdgeKey](
      trace: WLFeatureTrace[NodeKey, EdgeKey],
      colour: WLColour,
      durableCoordinates: Boolean
  ): PortableWLSignature =
    if !durableCoordinates then PortableWLSignature.Ephemeral(colour)
    else
      trace
        .signature(colour)
        .flatMap(_.stableDescriptor)
        .fold[PortableWLSignature](PortableWLSignature.Ephemeral(colour))(fingerprint =>
          PortableWLSignature.Durable(fingerprint.value)
        )

  private def normalize(
      sampleKeys: Vector[SampleKey],
      rows: RawRows,
      tolerance: NonNegativeFinite
  ): Either[
    WLFeatureEmissionError[Nothing],
    (RawRows, Vector[NumericalAuditEvent])
  ] =
    val normalized = Vector.newBuilder[Vector[RawEntry]]
    val events = Vector.newBuilder[NumericalAuditEvent]
    var index = 0
    while index < rows.size do
      val squares = rows(index).iterator.map(entry => entry.value * entry.value).toArray
      Sorting.quickSort(squares)
      val diagonal = squares.sum
      if !diagonal.isFinite then
        return Left(WLFeatureEmissionError.NonFiniteDiagonal(sampleKeys(index), index, diagonal))
      else if diagonal < -tolerance.toDouble then
        return Left(
          WLFeatureEmissionError.NegativeDiagonal(
            sampleKeys(index),
            index,
            diagonal,
            tolerance
          )
        )
      else if math.abs(diagonal) <= tolerance.toDouble then
        normalized += Vector.empty
        events += NumericalAuditEvent.DiagonalTreatedAsZero(
          sampleKeys(index),
          index,
          diagonal,
          tolerance
        )
      else
        val factor = 1.0 / math.sqrt(diagonal)
        normalized += rows(index).map(entry => entry.copy(value = factor * entry.value))
      index += 1
    Right(normalized.result() -> events.result())

  private def materialize(
      sampleKeys: Vector[SampleKey],
      rawRows: RawRows,
      events: Vector[NumericalAuditEvent],
      featureCount: Int
  ): WLFeatureEmission =
    val columnsByFeature = Array.fill(featureCount)(-1)
    val coordinates = Vector.newBuilder[PortableWLFeatureCoordinate]
    val origins = Vector.newBuilder[WLColour]
    var columnCount = 0
    val rows = rawRows.map { row =>
      val indices = new Array[Int](row.size)
      val values = new Array[Double](row.size)
      var entryIndex = 0
      while entryIndex < row.size do
        val entry = row(entryIndex)
        var column = columnsByFeature(entry.featureId)
        if column < 0 then
          column = columnCount
          columnCount += 1
          columnsByFeature(entry.featureId) = column
          coordinates += entry.coordinate
          origins += entry.origin
        indices(entryIndex) = column
        values(entryIndex) = entry.value
        entryIndex += 1
      sortPairs(indices, values)
      PortableSparseFeatureRow(indices.toVector, values.toVector)
    }
    val completedCoordinates = coordinates.result()
    val completedOrigins = origins.result()
    WLFeatureEmission(
      sampleKeys,
      rows,
      WLFeatureDictionary.build(completedCoordinates, completedOrigins),
      events
    )

  private def sortPairs(indices: Array[Int], values: Array[Double]): Unit =
    var index = 1
    while index < indices.length do
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
