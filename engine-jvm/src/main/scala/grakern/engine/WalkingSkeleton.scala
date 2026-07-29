package grakern.engine

import cats.kernel.Eq
import grakern.core.*
import grakern.graph4s.GraphSample

/** Private E1 walking skeleton.
  *
  * It gives a categorical node counting measure the delta part kernel, lowers the result to exact
  * corpus features, and materializes a Gram matrix. It is deliberately not the public
  * vertex-histogram recipe or a general compiler SPI.
  */
private[grakern] object WalkingSkeleton:
  def prepare[V, N, E, C: Eq](
      samples: Vector[GraphSample[V, N, E]],
      nodeKey: Projection[N, C],
      equalityId: StableId,
      labelCodec: StableId,
      corpusContent: CorpusContentFingerprint
  ): Either[PackedCorpusError[V], WalkingPrepared] =
    PackedGraphCorpus.pack(samples).map { packed =>
      val planNode = PlanNodeId.fresh()
      val semantic =
        Fingerprintability.combine(
          schema = "grakern.walking.node-measure.v1",
          atoms = Vector(labelCodec.text),
          children = Vector(
            Kernel.deltaNamed[C](equalityId).fingerprintability,
            nodeKey.fingerprintAt(planNode)
          )
        )
      val columns =
        packed.categoricalNodeColumn(value => nodeKey(value))
      val rows = exactCountRows(packed, columns)
      val gram = gramFrom(rows)
      val artifact =
        semantic match
          case Fingerprintability.Durable(fingerprint) =>
            Right(
              ArtifactFingerprint.from(
                ArtifactFingerprintInput(
                  semantic = fingerprint,
                  corpus = corpusContent,
                  orderedSamples = packed.sampleKeys,
                  compilerSchema = stableId(
                    "grakern.walking-compiler"
                  ),
                  loweringSchema = stableId(
                    "exact-categorical-count-gram"
                  ),
                  numericalPolicy = stableId(
                    "ordered-double-dot-v1"
                  ),
                  approximation = None,
                  engineVersion = stableId("grakern-engine-e1")
                )
              )
            )
          case Fingerprintability.Ephemeral(blockers) =>
            Left(blockers)
      WalkingPrepared(
        packed.sampleKeys,
        gram,
        semantic,
        artifact
      )
    }

  private def exactCountRows[V, N, E, C: Eq](
      packed: PackedGraphCorpus[V, N, E],
      columns: Vector[C]
  ): Vector[Vector[Double]] =
    val equality = Eq[C]
    val dictionary =
      columns.foldLeft(Vector.empty[C]) { (current, value) =>
        if current.exists(equality.eqv(_, value)) then current
        else current :+ value
      }

    Vector.tabulate(packed.graphCount) { graphIndex =>
      val counts = Array.fill(dictionary.size)(0.0)
      packed.graphVertexRange(graphIndex).foreach { vertexIndex =>
        val value = columns(vertexIndex)
        val featureIndex =
          dictionary.indexWhere(equality.eqv(_, value))
        counts(featureIndex) += 1.0
      }
      counts.toVector
    }

  private def gramFrom(rows: Vector[Vector[Double]]): Vector[Vector[Double]] =
    Vector.tabulate(rows.size) { left =>
      Vector.tabulate(rows.size) { right =>
        Kernel.linear.evaluate(rows(left), rows(right))
      }
    }

  private def stableId(value: String): StableId =
    StableId
      .from(value)
      .fold(
        error =>
          throw new IllegalStateException(
            s"invalid internal stable id $value: $error"
          ),
        identity
      )

final private[grakern] case class WalkingPrepared(
    samples: Vector[SampleKey],
    gram: Vector[Vector[Double]],
    semantic: Fingerprintability,
    artifact: Either[FingerprintBlockers, ArtifactFingerprint]
)
