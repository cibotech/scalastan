package com.cibo.scalastan.data

import com.cibo.scalastan._
import com.cibo.scalastan.ast.StanDataDeclaration
import org.scalatest.{FunSpec, Matchers}

class RDataSourceSpec extends FunSpec with Matchers with ScalaStan {
  describe("RDataSourceSpec") {
    it("parses reals") {
      val ds = RDataSource.fromString("x <- 1.5")
      val decl = data(real())
      ds.read(decl, "x") shouldBe 1.5
    }

    it("parses ints") {
      val ds = RDataSource.fromString("x <- 5")
      val decl = data(real())
      ds.read(decl, "x") shouldBe 5
    }

    it("parses with '='") {
      val ds = RDataSource.fromString("x = 2.5")
      val decl = data(real())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles semicolons") {
      val ds = RDataSource.fromString("x = 2.5;")
      val decl = data(real())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("handles quotes") {
      val ds = RDataSource.fromString("\"what\" = 2.5")
      val decl = data(real())
      ds.read(decl, "what") shouldBe 2.5
    }

    it("ignores comments") {
      val ds = RDataSource.fromString("# this is a test\nx = 2.5")
      val decl = data(real())
      ds.read(decl, "x") shouldBe 2.5
    }

    it("parses vectors") {
      val ds = RDataSource.fromString("x <- c(1.5, 2)")
      val index = data(int())
      val decl = data(vector(index))
      ds.read(decl, "x") shouldBe Vector(1.5, 2.0)
    }

    it("parses matrices") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(2, 3))")
      val index1 = data(int())
      val index2 = data(int())
      val decl = data(matrix(index1, index2))
      ds.read(decl, "x") shouldBe Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6)
      )
    }

    it("parses arrays") {
      val ds = RDataSource.fromString("x <- structure(c(1, 2, 3, 4, 5, 6), .Dim = c(1, 2, 3))")
      val index1 = data(int())
      val index2 = data(int())
      val index3 = data(int())
      val decl = data(vector(index3)(index1)(index2))
      ds.read(decl, "x") shouldBe Vector(
        Vector(
          Vector(1, 2, 3),
          Vector(4, 5, 6)
        )
      )
    }
  }
}
