# ADR-009: The labelled-neighbourhood protocol

Status: Accepted
Date: 2026-08-28
Supersedes: the "first public graph input" decision of ADR-001; keeps its identity rules.

## Context

ADR-001 bound grakern's only graph input to `graph4s.Graph` — finite, simple,
loopless, undirected — and `WLTrace` read incidences straight from it,
symmetrizing every edge under one key. Consumers with directed labelled
multigraphs (predicate–argument charts, where reversing `ARG0`/`ARG1` fillers
must change the kernel) could not be represented, and graph4s became a
transitive dependency of every consumer of `grakern-standard`. The README's
claim that `core` and `standard` are topology-neutral was not true.

## Decision

`grakern-core` owns a minimal protocol; graph4s becomes one adapter.

```scala
enum Orientation { Outgoing, Incoming, Undirected }
final case class Incidence[V, +E](other: V, value: E, orientation: Orientation)

trait LabelledNeighbourhood[V, +N, +E]:
  def key: SampleKey
  def vertexHash: Hash[V]
  def vertices: IndexedSeq[V]
  def nodeValue(vertex: V): N
  def incidences(vertex: V): IndexedSeq[Incidence[V, E]]
```

**Minimality.** The trait exposes exactly what refinement and decomposition
consume: graph-local identity (`vertexHash`, the ADR-001 rule that payload is
not identity), one total node value per vertex, and labelled incidences with
the orientation *relative to the vertex asked*. Node and edge *keys* are not
part of the protocol — they remain `Projection`s inside recipes, which is what
keeps fingerprintability unchanged (named projections/codecs are durable,
anonymous ones ephemeral). Any richer capability (topology equality, induced
subgraphs, field restriction) belongs to an adapter.

**Laws.** Vertices are distinct under `vertexHash`; every `other` is a vertex;
an `Undirected` incidence `v→u` is matched by `u→v` with equal value, with
multiplicity (a self-loop is one incidence); an `Outgoing` `v→u` is matched by
exactly one `Incoming` `u→v`, and vice versa. `NeighbourhoodSample` validates
them at construction; `PackedGraphCorpus.pack` re-checks them and pairs the two
appearances of a physical edge by slot and creation order, so parallel edges
and directed edges pack without a graph4s topology.

**Orientation participates in WL.** The refinement neighbourhood element is
`(edgeKey, orientation, colour)`. `Undirected` inputs are bit-identical to the
pre-protocol implementation on every committed GraKeL and GraphKernels.jl
fixture (features, gram, cross). For directed inputs reversing an edge changes
features and kernel values (law G1); alpha-renaming and enumeration order do
not (G2). The WL codec/refinement/neighbour schemas are bumped to `v2` so
undirected-era artifacts are never silently reused.

**Edge identity.** `EdgeRef[V](first, second, orientation, ordinal)` names a
physical edge in witnesses and errors; ordinals distinguish parallel edges.
`Neighbourhoods.canonicalEdges` enumerates each physical edge once.

**Loops, multiplicity, and graph4s.** `NeighbourhoodSample` is the native
multigraph adapter. Parallel incidences are an exact multiset: equal keys still
count separately, and enumeration order is irrelevant to refinement. An
undirected self-loop contributes one incidence at its vertex; a directed
self-loop contributes one `Outgoing` and one `Incoming` incidence. The graph4s
adapter remains deliberately simple and loopless. Its `fromEdgeList` constructor
rejects self-loops, parallel or reversed-duplicate edges, unknown endpoints, and
duplicate vertices with `GraphSampleEdgeListError`; it never constructs a
partial sample or lets graph4s set semantics silently coalesce relations.

**Relations as vertices.** `Reify.relationsAsVertices` is the topology-neutral
escape hatch when edge attributes must participate in node refinement. Every
physical `EdgeRef` becomes a distinct relation vertex whose value is the edge
value. Undirected relations connect to distinct endpoints with `Undirected` (a
self-loop once); directed relations use separate `Source` and `Target` endpoint
labels, including two labelled connections for a directed self-loop. The
construction is deterministic, alpha-invariant, and injective on labelled-edge
multisets; it returns `ReificationError` if its generated neighbourhood ever
violates the protocol.

**Module boundary.** `grakern-core` and `grakern-standard` import no graph4s
symbol (a source-level test enforces it); `grakern-graph4s` is the only module
that does. `GraphSample` keeps its `build`/`fromFields` constructors and now
implements the protocol with `Undirected` incidences; `nodeValue` is total
because graph4s fields are total by construction.

## Consequences

- Shortest-path recipes keep undirected traversal semantics (every incidence is
  traversable); directed shortest paths are a later recipe.
- `PackedCorpusError.MissingNodeValue/MissingEdgeValue/UnknownAdjacencyEdge`
  are replaced by `UnmatchedIncidence`; `HistogramWitness.Edge` carries an
  `EdgeRef`, not a graph4s `Edge`.
- Public `GraphSample.nodeValue` returns `N` (was `Option[N]`);
  `nodeValueOption` retains the old behaviour.
- The engine compiler (`engine-jvm`) is unchanged in placement; making the
  feature emitter portable is a separate bead.

## Rejected alternatives

- Encoding direction in an undirected edge label: both endpoints would see the
  same label, so reversal is invisible under WL.
- Widening the trait with topology queries (`containsEdge`, edges, degrees):
  every consumer would pay for capabilities only shortest-path lowerings need;
  derived helpers cover them without widening the contract.
- Changing graph4s's simple-graph semantics: graph4s's identity is the simple
  undirected graph; multigraphs use `NeighbourhoodSample` or explicit
  relation-as-vertex reification.
