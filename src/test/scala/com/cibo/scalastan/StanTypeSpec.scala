package com.cibo.scalastan

class StanTypeSpec extends ScalaStanBaseSpec {
  describe("matrices") {
    it("emits the right thing") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val m = StanMatrix(rows, cols)
      val data = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )

      m.emitData(data) shouldBe "structure(c(1.0,4.0,2.0,5.0,3.0,6.0), .Dim = c(2,3))"
    }
  }

  describe("arrays") {
    it("should emit the same thing as a matrix") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanArray(rows, StanReal()))
      val m = StanMatrix(rows, cols)

      val data = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )

      a.emitData(data) shouldBe m.emitData(data)
    }
  }
}
