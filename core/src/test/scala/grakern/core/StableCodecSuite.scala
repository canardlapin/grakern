package grakern.core

import munit.FunSuite

final class StableCodecSuite extends FunSuite:
  test("a named codec retains its asserted identity and encoding"):
    val id = StableId.from("molecule.element").toOption.get
    val codec =
      StableCodec.named[String](id, StableVersion.initial)(identity)

    assertEquals(codec.id.text, "molecule.element")
    assertEquals(codec.version.number, 1)
    assertEquals(codec.encode("C"), "C")
