package com.cibo.scalastan.data

import com.cibo.scalastan._
import org.scalatest.{FunSpec, Matchers}

class RDataSourceSpec extends FunSpec with Matchers with ScalaStan {
  describe("RDataSourceSpec") {
    it("parses reals") {
      val ds = RDataSource.fromString("x <- 1.5")
      val decl = StanDataDeclaration[StanReal](StanReal())
      ds.read(decl, "x") shouldBe 1.5
    }

    it("parses ints") {
      val ds = RDataSource.fromString("x <- 5")
      val decl = StanDataDeclaration[StanInt](StanInt())
      ds.read(decl, "x") shouldBe 5
    }

    it("parses with '='") {
      val ds = RDataSource.fromString("x = 2.5")
      val decl = StanDataDeclaration[StanReal](StanReal())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles quotes") {
      val ds = RDataSource.fromString("\"what\" = 2.5")
      val decl = StanDataDeclaration[StanReal](StanReal())
      ds.read(decl, "what") shouldBe 2.5
    }

    it("ignores comments") {
      val ds = RDataSource.fromString("# this is a test\nx = 2.5")
      val decl = StanDataDeclaration[StanReal](StanReal())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("parses vectors") {
      val ds = RDataSource.fromString("x <- c(1.5, 2)")
      val index = StanDataDeclaration[StanInt](StanInt())
      val decl = StanDataDeclaration[StanVector](StanVector(index))
      ds.read(decl, "x") shouldBe Vector(1.5, 2.0)
    }

    it("parses matrices") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(2, 3))")
      val index1 = StanDataDeclaration[StanInt](StanInt())
      val index2 = StanDataDeclaration[StanInt](StanInt())
      val decl = StanDataDeclaration[StanMatrix](StanMatrix(index1, index2))
      ds.read(decl, "x") shouldBe Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6)
      )
    }

    it("parses arrays") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(1, 2, 3))")
      val index1 = StanDataDeclaration[StanInt](StanInt())
      val index2 = StanDataDeclaration[StanInt](StanInt())
      val index3 = StanDataDeclaration[StanInt](StanInt())
      val decl = StanDataDeclaration[StanArray[StanArray[StanVector]]](
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
