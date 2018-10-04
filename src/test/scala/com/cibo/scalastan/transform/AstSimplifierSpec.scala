package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStanBaseSpec, StanInt, StanModel}
import com.cibo.scalastan.ast._

class AstSimplifierSpec extends ScalaStanBaseSpec {
  describe("AstSimplifier") {
    it("simplifies nested blocks") {
      val model = new StanModel {}
      val returnStatement = StanReturnStatement(StanLocalDeclaration(StanInt(), "x"))
      val code = StanProgram(model = StanBlock(Seq(StanBlock(Seq(returnStatement)))))
      val simplifier = AstSimplifier()
      val simplified = simplifier.run(code)(model)
      simplified.model shouldBe returnStatement.copy(id = simplified.model.id)
    }

    it("removes empty conditionals") {
      val model = new StanModel {}
      val code = StanProgram(
        model = StanIfStatement(Seq((StanLocalDeclaration(StanInt(), "x"), StanBlock(Seq.empty))), None)
      )
      val simplifier = AstSimplifier()
      val simplified = simplifier.run(code)(model)
      simplified.model shouldBe StanBlock(Seq.empty, simplified.model.id)
    }
  }
}
