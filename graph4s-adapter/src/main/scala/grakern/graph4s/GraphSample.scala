package grakern.graph4s

import cats.kernel.Eq
import grakern.core.SampleKey
import graph4s.{Edge, Graph}
import graph4s.data.{EdgeField, VertexField}

final class GraphSample[V, +N, +E] private (
    val key: SampleKey,
    val topology: Graph[V],
    val nodes: VertexField[V, N],
    val edges: EdgeField[V, E]
):
  def nodeValue(vertex: V): Option[N] = nodes.get(vertex)
  def edgeValue(edge: Edge[V]): Option[E] = edges.get(edge)
  def edgeValue(left: V, right: V): Option[E] = edges.get(left, right)

object GraphSample:
  def build[V, N, E](
      key: SampleKey,
      topology: Graph[V]
  )(
      nodeValue: V => N,
      edgeValue: Edge[V] => E
  ): GraphSample[V, N, E] =
    new GraphSample(
      key,
      topology,
      VertexField.total(topology)(nodeValue),
      EdgeField.total(topology)(edgeValue)
    )

  def fromFields[V, N, E](
      key: SampleKey,
      topology: Graph[V],
      nodes: VertexField[V, N],
      edges: EdgeField[V, E]
  ): Either[GraphSampleError[V], GraphSample[V, N, E]] =
    if !Eq[Graph[V]].eqv(topology, nodes.topology) then
      Left(
        GraphSampleError.NodeFieldTopologyMismatch(
          key,
          topology,
          nodes.topology
        )
      )
    else if !Eq[Graph[V]].eqv(topology, edges.topology) then
      Left(
        GraphSampleError.EdgeFieldTopologyMismatch(
          key,
          topology,
          edges.topology
        )
      )
    else Right(new GraphSample(key, topology, nodes, edges))

enum GraphSampleError[V]:
  case NodeFieldTopologyMismatch(
      key: SampleKey,
      expected: Graph[V],
      actual: Graph[V]
  )
  case EdgeFieldTopologyMismatch(
      key: SampleKey,
      expected: Graph[V],
      actual: Graph[V]
  )
