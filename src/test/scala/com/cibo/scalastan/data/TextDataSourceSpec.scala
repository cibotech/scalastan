package com.cibo.scalastan.data

import com.cibo.scalastan._
import com.cibo.scalastan.ast.StanDataDeclaration

class TextDataSourceSpec extends ScalaStanBaseSpec with ScalaStan {
  describe("TextDataSource") {
    it("parses int") {
      val text = "5\n"
      val src = TextDataSource.fromString(text)
      val decl = data(int())

      src.read(decl) shouldBe 5
    }

    it("parses real") {
      val text = "5.5\n"
      val src = TextDataSource.fromString(text)
      val decl = data(real())

      src.read(decl) shouldBe 5.5
    }

    it("parses vector") {
      val text = "1.5 2.5 3.5 4.5\n"
      val src = TextDataSource.fromString(text)
      val n = data(int())
      val decl = data(vector(n))

      src.read(decl) shouldBe Seq[Double](1.5, 2.5, 3.5, 4.5)
    }

    it("parses matrix") {
      val text = "1.5 2.5\n3.5 4.5\n"
      val src = TextDataSource.fromString(text)
      val n = data(int())
      val decl = data(matrix(n, n))

      src.read(decl) shouldBe Seq(Seq(1.5, 2.5), Seq(3.5, 4.5))
    }
  }
}
