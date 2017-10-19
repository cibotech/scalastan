package com.cibo.scalastan.data

import com.cibo.scalastan._
import org.scalatest.{FunSpec, Matchers}

class RDataSourceSpec extends FunSpec with Matchers {
  describe("RDataSourceSpec") {
    it("parses reals") {
      val ds = RDataSource.fromString("x <- 1.5")
      val decl = StanDeclaration[StanReal, DataDeclarationType](StanReal())
      ds.read(decl, "x") shouldBe 1.5
    }

    it("parses ints") {
      val ds = RDataSource.fromString("x <- 5")
      val decl = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      ds.read(decl, "x") shouldBe 5
    }

    it("parses with '='") {
      val ds = RDataSource.fromString("x = 2.5")
      val decl = StanDeclaration[StanReal, DataDeclarationType](StanReal())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles quotes") {
      val ds = RDataSource.fromString("\"what\" = 2.5")
      val decl = StanDeclaration[StanReal, DataDeclarationType](StanReal())
      ds.read(decl, "what") shouldBe 2.5
    }

    it("parses vectors") {
      val ds = RDataSource.fromString("x <- c(1.5, 2)")
      val index = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val decl = StanDeclaration[StanVector, DataDeclarationType](StanVector(index))
      ds.read(decl, "x") shouldBe Vector(1.5, 2.0)
    }

    it("parses matrices") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(2, 3))")
      val index1 = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val index2 = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val decl = StanDeclaration[StanMatrix, DataDeclarationType](StanMatrix(index1, index2))
      ds.read(decl, "x") shouldBe Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6)
      )
    }

    it("parses arrays") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(1, 2, 3))")
      val index1 = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val index2 = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val index3 = StanDeclaration[StanInt, DataDeclarationType](StanInt())
      val decl = StanDeclaration[StanArray[StanArray[StanVector]], DataDeclarationType](
        StanArray(index1, StanArray(index2, StanVector(index3)))
      )
      ds.read(decl, "x") shouldBe Vector(
        Vector(
          Vector(1, 2, 3),
          Vector(4, 5, 6)
        )
      )
    }
  }
}
