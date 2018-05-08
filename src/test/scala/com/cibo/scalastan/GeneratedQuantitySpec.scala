package com.cibo.scalastan

class GeneratedQuantitySpec extends ScalaStanBaseSpec {
  describe("GeneratedQuantity") {
    it("should allow assignment to the result") {
      new ScalaStan {
        val t = new GeneratedQuantity(real()) {
          result := 5
        }
        val model = new Model { local(real()) := t }
        checkCode(model, "generated quantities { real t; { t = 5; } }")
      }
    }
  }
}
