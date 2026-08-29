package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.*
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

final class WLFeatureEmitterSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  given Encoder[WLSerializedFeatureCoordinate] = deriveEncoder
  given Decoder[WLSerializedFeatureCoordinate] = deriveDecoder

  private val subtreeWeight = NonNegativeFinite.from(0.75).toOption.get
  private val assignmentWeight = NonNegativeFinite.from(0.25).toOption.get

  private def namedRefinement =
    WLRefinement
      .create(
        2,
        Projection.named[String, String](
          StableId.from("portable.node-projection").toOption.get,
          StableVersion.initial
        )(identity),
        Projection.named[Int, Int](
          StableId.from("portable.edge-projection").toOption.get,
          StableVersion.initial
        )(identity),
        WLCodecs(
          Some(
            StableCodec.named[String](
              StableId.from("portable.node-codec").toOption.get,
              StableVersion.initial
            )(identity)
          ),
          Some(
            StableCodec.named[Int](
              StableId.from("portable.edge-codec").toOption.get,
              StableVersion.initial
            )(_.toString)
          )
        )
      )
      .toOption
      .get

  private def samples: Vector[NeighbourhoodSample[Int, String, Int]] =
    Vector(
      NeighbourhoodSample
        .undirected(
          SampleKey("pair"),
          Vector(0, 1),
          vertex => if vertex == 0 then "C" else "O",
          Vector((0, 1, 1))
        )
        .toOption
        .get,
      NeighbourhoodSample
        .undirected(
          SampleKey("path"),
          Vector(0, 1, 2),
          vertex => if vertex == 1 then "O" else "C",
          Vector((0, 1, 1), (1, 2, 2))
        )
        .toOption
        .get,
      NeighbourhoodSample
        .undirected(
          SampleKey("isolated"),
          Vector(0),
          _ => "N",
          Vector.empty[(Int, Int, Int)]
        )
        .toOption
        .get
    )

  private def pairedKernel(refinement: WLRefinement[String, Int, String, Int]) =
    (
      refinement.subtree.weighted(subtreeWeight) +
        refinement.optimalAssignment.weighted(assignmentWeight)
    ).normalized

  test("portable emission has a committed cross-platform golden"):
    val emission =
      WLFeatureEmitter
        .emit(samples, pairedKernel(namedRefinement))
        .toOption
        .get
    val serialized = emission.dictionary.serializableCoordinates.toOption.get
    val receipt =
      serialized
        .map(coordinate =>
          Vector(
            coordinate.blockPath.mkString("."),
            coordinate.component,
            coordinate.round.toString,
            coordinate.signatureFingerprint,
            coordinate.multiplicityThreshold.fold("-")(_.toString)
          ).mkString("|")
        )
        .mkString(";") + "#" +
        emission.rows
          .map(row =>
            row.indices
              .zip(row.values)
              .map { case (index, value) => s"$index=${value.toString}" }
              .mkString(",")
          )
          .mkString(";")
    val checksum = receipt.foldLeft(1125899906842597L) { (current, character) =>
      current * 31L + character.toInt.toLong
    }

    assertEquals(emission.dictionary.durability, WLFeatureDictionaryDurability.Durable)
    assertEquals(emission.sampleKeys, samples.map(_.key))
    assertEquals(emission.dictionary.size, 29)
    assertEquals(emission.rows.map(_.activeSize), Vector(12, 17, 6))
    assertEquals(checksum, 3194310914208312967L)

  test("a JSON dictionary round trip aligns a fresh emission"):
    val original =
      WLFeatureEmitter
        .emit(samples, pairedKernel(namedRefinement))
        .toOption
        .get
    val serialized = original.dictionary.serializableCoordinates.toOption.get
    val json = serialized.asJson.noSpaces
    val decoded = decode[Vector[WLSerializedFeatureCoordinate]](json).toOption.get
    val restored = WLFeatureDictionary.fromSerialized(decoded).toOption.get

    val fresh =
      WLFeatureEmitter
        .emit(samples, pairedKernel(namedRefinement))
        .toOption
        .get
    val aligned = fresh.alignTo(restored)

    assertEquals(decoded, serialized)
    assertEquals(restored.coordinates, original.dictionary.coordinates)
    assertEquals(aligned.rows, original.rows)
    assertEquals(aligned.novelSquaredNorm, Vector.fill(samples.size)(0.0))

  test("missing stable codecs make dictionary serialization a typed refusal"):
    val refinement =
      WLRefinement
        .create(
          1,
          Projection.of[String, String](identity),
          Projection.of[Int, Int](identity),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    val emission = WLFeatureEmitter.emit(samples, refinement.subtree).toOption.get

    assertEquals(emission.dictionary.durability, WLFeatureDictionaryDurability.Ephemeral)
    assertEquals(
      emission.dictionary.serializableCoordinates,
      Left(WLFeatureDictionaryError.EphemeralCoordinate(0))
    )

  test("portable sparse cosine handles shared, disjoint, and zero rows"):
    val left = PortableSparseFeatureRow(Vector(0, 2), Vector(3.0, 4.0))
    val right = PortableSparseFeatureRow(Vector(0, 1, 2), Vector(6.0, 1.0, 8.0))
    val disjoint = PortableSparseFeatureRow(Vector(3), Vector(7.0))
    val empty = PortableSparseFeatureRow(Vector.empty, Vector.empty)

    assertEqualsDouble(left.cosine(right), 50.0 / math.sqrt(25.0 * 101.0), 1e-15)
    assertEquals(left.cosine(disjoint), 0.0)
    assertEquals(left.cosine(empty), 0.0)
