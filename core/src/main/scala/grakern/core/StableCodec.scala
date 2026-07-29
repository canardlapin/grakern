package grakern.core

/** A user-asserted stable encoding for durable semantic identities.
  *
  * Grakern records the identifier and version but cannot prove that the supplied function remains
  * stable, injective, or semantically correct.
  */
sealed trait StableCodec[-A]:
  def id: StableId
  def version: StableVersion
  def encode(value: A): String

object StableCodec:
  def named[A](
      stableId: StableId,
      stableVersion: StableVersion
  )(encoder: A => String): StableCodec[A] =
    new StableCodec[A]:
      val id: StableId = stableId
      val version: StableVersion = stableVersion
      def encode(value: A): String = encoder(value)
