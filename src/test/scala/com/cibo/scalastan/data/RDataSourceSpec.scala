package com.cibo.scalastan.data

import com.cibo.scalastan._
import com.cibo.scalastan.ast.StanDataDeclaration
import org.scalatest.{FunSpec, Matchers}

class RDataSourceSpec extends FunSpec with Matchers {
  describe("RDataSourceSpec") {
    it("parses reals") {
      val ds = RDataSource.fromString("x <- 1.5")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "x") shouldBe 1.5
    }

    it("parses ints") {
      val ds = RDataSource.fromString("x <- 5")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "x") shouldBe 5
    }

    it("parses with '='") {
      val ds = RDataSource.fromString("x = 2.5")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles semicolons") {
      val ds = RDataSource.fromString("x = 2.5;")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles quotes") {
      val ds = RDataSource.fromString("\"what\" = 2.5")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "what") shouldBe 2.5
    }

    it("ignores comments") {
      val ds = RDataSource.fromString("# this is a test\nx = 2.5")
      val decl = StanDataDeclaration(StanReal(), "x")
      ds.read(decl, "x") shouldBe 2.5
    }

    it("parses vectors") {
      val ds = RDataSource.fromString("x <- c(1.5, 2)")
      val index = StanDataDeclaration(StanInt(), "index")
      val decl = StanDataDeclaration(StanVector(index), "x")
      ds.read(decl, "x") shouldBe Vector(1.5, 2.0)
    }

    it("parses matrices") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(2, 3))")
      val index1 = StanDataDeclaration(StanInt(), "m")
      val index2 = StanDataDeclaration(StanInt(), "n")
      val decl = StanDataDeclaration(StanMatrix(index1, index2), "x")
      ds.read(decl, "x") shouldBe Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6)
      )
    }

    it("parses arrays") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(1, 2, 3))")
      val index1 = StanDataDeclaration(StanInt(), "index1")
      val index2 = StanDataDeclaration(StanInt(), "index2")
      val index3 = StanDataDeclaration(StanInt(), "index3")
      val decl = StanDataDeclaration(StanArray(index1, StanArray(index2, StanVector(index3))), "x")
      ds.read(decl, "x") shouldBe Vector(
        Vector(
          Vector(1, 2, 3),
          Vector(4, 5, 6)
        )
      )
    }
  }
}
