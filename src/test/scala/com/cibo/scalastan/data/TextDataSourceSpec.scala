package com.cibo.scalastan.data

import com.cibo.scalastan._
import com.cibo.scalastan.ast.StanDataDeclaration

class TextDataSourceSpec extends ScalaStanBaseSpec {

  private implicit val ss = new ScalaStan {}

  describe("TextDataSource") {
    it("parses int") {
      val data = "5\n"
      val src = TextDataSource.fromString(data)
      val decl = StanDataDeclaration(StanInt())

      src.read(decl) shouldBe 5
    }

    it("parses real") {
      val data = "5.5\n"
      val src = TextDataSource.fromString(data)
      val decl = StanDataDeclaration(StanReal())

      src.read(decl) shouldBe 5.5
    }

    it("parses vector") {
      val data = "1.5 2.5 3.5 4.5\n"
      val src = TextDataSource.fromString(data)
      val n = StanDataDeclaration(StanInt())
      val decl = StanDataDeclaration(StanVector(n))

      src.read(decl) shouldBe Seq[Double](1.5, 2.5, 3.5, 4.5)
    }

    it("parses matrix") {
      val data = "1.5 2.5\n3.5 4.5\n"
      val src = TextDataSource.fromString(data)
      val n = StanDataDeclaration(StanInt())
      val decl = StanDataDeclaration(StanMatrix(n, n))

      src.read(decl) shouldBe Seq(Seq(1.5, 2.5), Seq(3.5, 4.5))
    }
  }
}
