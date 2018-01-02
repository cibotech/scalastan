package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, ScalaStanBaseSpec, StanInt}
import com.cibo.scalastan.ast.{StanBlock, StanIfStatement, StanLocalDeclaration, StanReturnStatement}

class AstSimplifierSpec extends ScalaStanBaseSpec {
  describe("AstSimplifier") {
    it("simplifies nested blocks") {
      new ScalaStan {
        val returnStatement = StanReturnStatement(StanLocalDeclaration(StanInt()))
        val code = StanBlock(Seq(StanBlock(Seq(returnStatement))))
        val simplified = AstSimplifier.run(code)
        simplified shouldBe returnStatement
      }
    }

    it("removes empty conditionals") {
      new ScalaStan {
        val code = StanIfStatement(Seq((StanLocalDeclaration(StanInt()), StanBlock(Seq.empty))), None)
        val simplified = AstSimplifier.run(code)
        simplified shouldBe StanBlock(Seq.empty)
      }
    }
  }
}
