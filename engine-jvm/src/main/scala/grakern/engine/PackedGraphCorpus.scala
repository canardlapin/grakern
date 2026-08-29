package grakern.engine

import grakern.core.*

final private[grakern] case class PackedEdge[V, +E](
    source: Int,
    target: Int,
    owner: Int,
    original: EdgeRef[V],
    value: E,
    orientation: Orientation
)

final private[grakern] class NumericNodeColumn private[engine] (
    private[engine] val values: Array[Double]
):
  def size: Int = values.length
  def apply(index: Int): Double = values(index)

final private[grakern] class NonNegativeEdgeColumn private[engine] (
    private[engine] val values: Array[Double]
):
  def size: Int = values.length
  def apply(index: Int): Double = values(index)

/** Private engine state: every sample of a corpus packed into disjoint integer offsets with CSR
  * adjacency. Adjacency lists preserve the adapter's incidence order and record, per entry, the
  * packed edge and the orientation of that edge relative to the row vertex.
  */
final private[grakern] class PackedGraphCorpus[V, +N, +E] private (
    val sampleKeys: Vector[SampleKey],
    private[engine] val graphVertexOffsets: Array[Int],
    private[engine] val adjacencyOffsets: Array[Int],
    private[engine] val adjacencyTargets: Array[Int],
    private[engine] val adjacencyEdges: Array[Int],
    private[engine] val adjacencyOrientations: Array[Byte],
    private[engine] val vertexOwners: Array[Int],
    private[engine] val originalVertices: Vector[V],
    private[engine] val nodeValues: Vector[N],
    private[engine] val packedEdges: Vector[PackedEdge[V, E]]
):
  def graphCount: Int = sampleKeys.size
  def vertexCount: Int = originalVertices.size
  def edgeCount: Int = packedEdges.size

  def graphVertexRange(graphIndex: Int): Range =
    graphVertexOffsets(graphIndex) until graphVertexOffsets(graphIndex + 1)

  def neighbors(vertexIndex: Int): collection.IndexedSeqView[Int] =
    adjacencyTargets.view.slice(
      adjacencyOffsets(vertexIndex),
      adjacencyOffsets(vertexIndex + 1)
    )

  private[engine] def adjacencyStart(vertexIndex: Int): Int =
    adjacencyOffsets(vertexIndex)

  private[engine] def adjacencyEnd(vertexIndex: Int): Int =
    adjacencyOffsets(vertexIndex + 1)

  private[engine] def adjacencyTarget(adjacencyIndex: Int): Int =
    adjacencyTargets(adjacencyIndex)

  private[engine] def adjacencyEdge(adjacencyIndex: Int): Int =
    adjacencyEdges(adjacencyIndex)

  private[engine] def adjacencyOrientation(adjacencyIndex: Int): Orientation =
    PackedGraphCorpus.orientationOf(adjacencyOrientations(adjacencyIndex))

  def ownerOfVertex(vertexIndex: Int): SampleKey =
    sampleKeys(vertexOwners(vertexIndex))

  def originalVertex(vertexIndex: Int): V =
    originalVertices(vertexIndex)

  def categoricalNodeColumn[C](projection: N => C): Vector[C] =
    nodeValues.map(projection)

  def numericNodeColumn(
      descriptor: ProjectionDescriptor
  )(projection: N => Double): Either[PrepareError[V, EdgeRef[V]], NumericNodeColumn] =
    val output = new Array[Double](nodeValues.size)
    var index = 0
    var failure: Option[PrepareError[V, EdgeRef[V]]] = None
    while index < nodeValues.size && failure.isEmpty do
      val observed = projection(nodeValues(index))
      FiniteDouble.from(observed) match
        case Right(value) =>
          output(index) = value.toDouble
        case Left(reason) =>
          failure = Some(
            PrepareError.InvalidNumeric(
              SourceRef.Node(
                ownerOfVertex(index),
                originalVertices(index)
              ),
              descriptor,
              observed,
              reason
            )
          )
      index += 1
    failure.toLeft(new NumericNodeColumn(output))

  def nonNegativeEdgeColumn(
      descriptor: ProjectionDescriptor
  )(
      projection: E => Double
  ): Either[
    PrepareError[Nothing, EdgeRef[V]],
    NonNegativeEdgeColumn
  ] =
    val output = new Array[Double](packedEdges.size)
    var index = 0
    var failure: Option[PrepareError[Nothing, EdgeRef[V]]] = None
    while index < packedEdges.size && failure.isEmpty do
      val edge = packedEdges(index)
      val observed = projection(edge.value)
      NonNegativeFinite.from(observed) match
        case Right(value) =>
          output(index) = value.toDouble
        case Left(reason) =>
          failure = Some(
            PrepareError.InvalidNumeric(
              SourceRef.Edge(
                sampleKeys(edge.owner),
                edge.original
              ),
              descriptor,
              observed,
              reason
            )
          )
      index += 1
    failure.toLeft(new NonNegativeEdgeColumn(output))

private[grakern] object PackedGraphCorpus:
  final private class IntArrayBuilder(initialCapacity: Int = 16):
    private var values = new Array[Int](math.max(1, initialCapacity))
    private var length = 0

    def size: Int = length

    def +=(value: Int): Unit =
      ensureCapacity(length + 1)
      values(length) = value
      length += 1

    def result(): Array[Int] = java.util.Arrays.copyOf(values, length)

    private def ensureCapacity(required: Int): Unit =
      if required > values.length then
        var capacity = values.length << 1
        while capacity < required do capacity = capacity << 1
        values = java.util.Arrays.copyOf(values, capacity)

  final private class ByteArrayBuilder(initialCapacity: Int = 16):
    private var values = new Array[Byte](math.max(1, initialCapacity))
    private var length = 0

    def +=(value: Byte): Unit =
      ensureCapacity(length + 1)
      values(length) = value
      length += 1

    def result(): Array[Byte] = java.util.Arrays.copyOf(values, length)

    private def ensureCapacity(required: Int): Unit =
      if required > values.length then
        var capacity = values.length << 1
        while capacity < required do capacity = capacity << 1
        values = java.util.Arrays.copyOf(values, capacity)

  /** Graph-local open-addressed vertex index using the protocol's Cats `Hash`, without allocating
    * one `HashKey` and boxed index per lookup.
    */
  final private class LocalVertexIndex[V](expectedSize: Int)(using hash: cats.kernel.Hash[V]):
    private val capacity =
      val desired = math.min(1L << 30, math.max(2L, expectedSize.toLong * 2L))
      var result = 2
      while result.toLong < desired do result = result << 1
      result
    private val mask = capacity - 1
    private val occupied = new Array[Boolean](capacity)
    private val keys = new Array[Any](capacity)
    private val indices = new Array[Int](capacity)

    def put(value: V, index: Int): Unit =
      val slot = findSlot(value)
      occupied(slot) = true
      keys(slot) = value
      indices(slot) = index

    def getOrNegative(value: V): Int =
      val slot = findSlot(value)
      if occupied(slot) then indices(slot) else -1

    private def findSlot(value: V): Int =
      var slot = spread(hash.hash(value)) & mask
      while occupied(slot) && !hash.eqv(keys(slot).asInstanceOf[V], value) do
        slot = (slot + 1) & mask
      slot

    private def spread(value: Int): Int = value ^ (value >>> 16)

  /** Primitive edge-slot table. Each physical edge is a node in one slot-local linked list; the two
    * incidence sides advance independent primitive cursors through that list.
    */
  final private class EdgeSlotTable(expectedEdges: Int):
    private val initialSlotCapacity =
      val desired = math.min(1L << 30, math.max(16L, expectedEdges.toLong * 2L))
      var result = 16
      while result.toLong < desired do result = result << 1
      result
    private var keys = new Array[Long](initialSlotCapacity)
    private var occupied = new Array[Boolean](initialSlotCapacity)
    private var heads = Array.fill(initialSlotCapacity)(-1)
    private var tails = Array.fill(initialSlotCapacity)(-1)
    private var trueCursors = Array.fill(initialSlotCapacity)(-1)
    private var falseCursors = Array.fill(initialSlotCapacity)(-1)
    private var sizes = new Array[Int](initialSlotCapacity)
    private var trueConsumed = new Array[Int](initialSlotCapacity)
    private var falseConsumed = new Array[Int](initialSlotCapacity)
    private var slotCount = 0
    private var edgeIds = new Array[Int](math.max(1, expectedEdges))
    private var nextEdges = Array.fill(math.max(1, expectedEdges))(-1)
    private var edgeCount = 0

    def append(key: Long, edgeId: Int): Int =
      var slot = findSlot(key)
      if !occupied(slot) && (slotCount + 1) * 10 >= keys.length * 7 then
        growSlots()
        slot = findSlot(key)
      ensureEdgeCapacity(edgeCount + 1)
      val ordinal = sizes(slot)
      edgeIds(edgeCount) = edgeId
      nextEdges(edgeCount) = -1
      if occupied(slot) then nextEdges(tails(slot)) = edgeCount
      else
        occupied(slot) = true
        keys(slot) = key
        heads(slot) = edgeCount
        slotCount += 1
      tails(slot) = edgeCount
      sizes(slot) += 1
      edgeCount += 1
      ordinal

    def resetCursors(): Unit =
      var slot = 0
      while slot < keys.length do
        if occupied(slot) then
          trueCursors(slot) = heads(slot)
          falseCursors(slot) = heads(slot)
        slot += 1

    def consume(key: Long, firstSide: Boolean): Int =
      val slot = findSlot(key)
      if !occupied(slot) then -1
      else
        val node = if firstSide then trueCursors(slot) else falseCursors(slot)
        if node < 0 then -1
        else
          if firstSide then
            trueCursors(slot) = nextEdges(node)
            trueConsumed(slot) += 1
          else
            falseCursors(slot) = nextEdges(node)
            falseConsumed(slot) += 1
          edgeIds(node)

    def firstUnmatched(): Option[Long] =
      var slot = 0
      while slot < keys.length do
        if occupied(slot) then
          val key = keys(slot)
          val needsSecondSide = slotDirected(key) || slotLower(key) != slotUpper(key)
          if trueConsumed(slot) != sizes(slot) ||
            needsSecondSide && falseConsumed(slot) != sizes(slot)
          then return Some(key)
        slot += 1
      None

    private def findSlot(key: Long): Int =
      val mask = keys.length - 1
      var slot = spread(key) & mask
      while occupied(slot) && keys(slot) != key do slot = (slot + 1) & mask
      slot

    private def growSlots(): Unit =
      val previousKeys = keys
      val previousOccupied = occupied
      val previousHeads = heads
      val previousTails = tails
      val previousSizes = sizes
      keys = new Array[Long](previousKeys.length << 1)
      occupied = new Array[Boolean](keys.length)
      heads = Array.fill(keys.length)(-1)
      tails = Array.fill(keys.length)(-1)
      trueCursors = Array.fill(keys.length)(-1)
      falseCursors = Array.fill(keys.length)(-1)
      sizes = new Array[Int](keys.length)
      trueConsumed = new Array[Int](keys.length)
      falseConsumed = new Array[Int](keys.length)
      var previousSlot = 0
      while previousSlot < previousKeys.length do
        if previousOccupied(previousSlot) then
          val slot = findSlot(previousKeys(previousSlot))
          occupied(slot) = true
          keys(slot) = previousKeys(previousSlot)
          heads(slot) = previousHeads(previousSlot)
          tails(slot) = previousTails(previousSlot)
          sizes(slot) = previousSizes(previousSlot)
        previousSlot += 1

    private def ensureEdgeCapacity(required: Int): Unit =
      if required > edgeIds.length then
        var capacity = edgeIds.length << 1
        while capacity < required do capacity = capacity << 1
        edgeIds = java.util.Arrays.copyOf(edgeIds, capacity)
        val expanded = Array.fill(capacity)(-1)
        Array.copy(nextEdges, 0, expanded, 0, nextEdges.length)
        nextEdges = expanded

    private def spread(value: Long): Int =
      val folded = (value ^ (value >>> 32)).toInt
      folded ^ (folded >>> 16)

  private[engine] def orientationByte(orientation: Orientation): Byte =
    orientation match
      case Orientation.Undirected => 0
      case Orientation.Outgoing   => 1
      case Orientation.Incoming   => 2

  private[engine] def orientationOf(value: Byte): Orientation =
    value match
      case 1 => Orientation.Outgoing
      case 2 => Orientation.Incoming
      case _ => Orientation.Undirected

  private def slotKey(lower: Int, upper: Int, directed: Boolean): Long =
    (lower.toLong << 33) | (upper.toLong << 1) | (if directed then 1L else 0L)

  private def slotLower(key: Long): Int = (key >>> 33).toInt
  private def slotUpper(key: Long): Int = ((key >>> 1) & 0xffffffffL).toInt
  private def slotDirected(key: Long): Boolean = (key & 1L) != 0L

  def pack[V, N, E](
      samples: Vector[LabelledNeighbourhood[V, N, E]]
  ): Either[PackedCorpusError[V], PackedGraphCorpus[V, N, E]] =
    val sampleKeys = samples.map(_.key)
    val graphOffsets = new IntArrayBuilder(samples.size + 1)
    graphOffsets += 0
    val adjacencyOffsets = new IntArrayBuilder()
    adjacencyOffsets += 0
    val adjacencyTargets = new IntArrayBuilder()
    val adjacencyEdges = new IntArrayBuilder()
    val adjacencyOrientations = new ByteArrayBuilder()
    val vertexOwners = new IntArrayBuilder()
    val originalVertices = Vector.newBuilder[V]
    val nodeValues = Vector.newBuilder[N]
    val packedEdges = Vector.newBuilder[PackedEdge[V, E]]
    var totalVertices = 0
    var totalEdges = 0
    var failure: Option[PackedCorpusError[V]] = None
    var graphIndex = 0

    while graphIndex < samples.size && failure.isEmpty do
      val sample = samples(graphIndex)
      given cats.kernel.Hash[V] = sample.vertexHash
      val vertices = sample.vertices
      val nextTotal = totalVertices.toLong + vertices.size.toLong
      if nextTotal > Int.MaxValue.toLong then
        failure = Some(
          PackedCorpusError.CapacityExceeded(
            sample.key,
            "vertices",
            nextTotal
          )
        )
      else
        val localToGlobal = new LocalVertexIndex[V](vertices.size)
        var vertexIndex = 0
        while vertexIndex < vertices.size do
          localToGlobal.put(vertices(vertexIndex), vertexIndex)
          vertexIndex += 1

        // Pass 1: create one packed edge per physical edge, in vertex/incidence order, and remember
        // the packed indices per slot so pass 2 can hand both endpoints the same edge.
        val slots = new EdgeSlotTable(vertices.size * 2)
        var localIndex = 0
        while localIndex < vertices.size && failure.isEmpty do
          val vertex = vertices(localIndex)
          val source = totalVertices + localIndex
          val incidences = sample.incidences(vertex)
          var incidenceIndex = 0
          while incidenceIndex < incidences.size && failure.isEmpty do
            val incidence = incidences(incidenceIndex)
            val targetLocal = localToGlobal.getOrNegative(incidence.other)
            if targetLocal < 0 then
              failure = Some(
                PackedCorpusError.UnknownNeighbor(sample.key, vertex, incidence.other)
              )
            else
              val target = totalVertices + targetLocal
              val creates =
                incidence.orientation match
                  case Orientation.Undirected => source <= target
                  case Orientation.Outgoing   => true
                  case Orientation.Incoming   => false
              if creates then
                val slot =
                  incidence.orientation match
                    case Orientation.Undirected =>
                      slotKey(
                        math.min(localIndex, targetLocal),
                        math.max(localIndex, targetLocal),
                        false
                      )
                    case _ => slotKey(localIndex, targetLocal, true)
                val ordinal = slots.append(slot, totalEdges)
                packedEdges += PackedEdge(
                  source,
                  target,
                  graphIndex,
                  EdgeRef(vertex, incidence.other, incidence.orientation, ordinal),
                  incidence.value,
                  incidence.orientation
                )
                totalEdges += 1
            incidenceIndex += 1
          localIndex += 1

        // Pass 2: adjacency in incidence order; each endpoint side consumes the slot's edges in
        // creation order, so the k-th incidence on one side pairs with the k-th on the other.
        slots.resetCursors()
        localIndex = 0
        while localIndex < vertices.size && failure.isEmpty do
          val vertex = vertices(localIndex)
          originalVertices += vertex
          vertexOwners += graphIndex
          nodeValues += sample.nodeValue(vertex)
          val incidences = sample.incidences(vertex)
          var incidenceIndex = 0
          while incidenceIndex < incidences.size && failure.isEmpty do
            val incidence = incidences(incidenceIndex)
            val targetLocal = localToGlobal.getOrNegative(incidence.other)
            val target = totalVertices + targetLocal
            val (slot, side) =
              incidence.orientation match
                case Orientation.Undirected =>
                  (
                    slotKey(
                      math.min(localIndex, targetLocal),
                      math.max(localIndex, targetLocal),
                      false
                    ),
                    localIndex <= targetLocal
                  )
                case Orientation.Outgoing => (slotKey(localIndex, targetLocal, true), true)
                case Orientation.Incoming => (slotKey(targetLocal, localIndex, true), false)
            val edgeIndex = slots.consume(slot, side)
            if edgeIndex >= 0 then
              adjacencyTargets += target
              adjacencyEdges += edgeIndex
              adjacencyOrientations += orientationByte(incidence.orientation)
            else
              failure = Some(
                PackedCorpusError.UnmatchedIncidence(
                  sample.key,
                  vertex,
                  incidence.other,
                  incidence.orientation
                )
              )
            incidenceIndex += 1
          adjacencyOffsets += adjacencyTargets.size
          localIndex += 1

        // Every created edge must have been consumed exactly once per endpoint side.
        if failure.isEmpty then
          slots.firstUnmatched().foreach { slot =>
            val lower = slotLower(slot)
            val upper = slotUpper(slot)
            failure = Some(
              PackedCorpusError.UnmatchedIncidence(
                sample.key,
                vertices(lower),
                vertices(upper),
                if slotDirected(slot) then Orientation.Outgoing else Orientation.Undirected
              )
            )
          }

        totalVertices = nextTotal.toInt
        graphOffsets += totalVertices
      graphIndex += 1

    failure.toLeft(
      new PackedGraphCorpus(
        sampleKeys,
        graphOffsets.result(),
        adjacencyOffsets.result(),
        adjacencyTargets.result(),
        adjacencyEdges.result(),
        adjacencyOrientations.result(),
        vertexOwners.result(),
        originalVertices.result(),
        nodeValues.result(),
        packedEdges.result()
      )
    )

enum PackedCorpusError[V]:
  case CapacityExceeded(sample: SampleKey, resource: String, requested: Long)
  case UnknownNeighbor(sample: SampleKey, source: V, target: V)
  case UnmatchedIncidence(
      sample: SampleKey,
      source: V,
      target: V,
      orientation: Orientation
  )
