package com.cibo.scalastan

class GeneratedQuantitySpec extends ScalaStanBaseSpec {
  describe("GeneratedQuantity") {
    it("should allow assignment to the result") {
      new ScalaStan {
        val model = new Model { }
        val t = new GeneratedQuantity(real()) {
          result := 5
        }
        checkCode(model, "generated quantities { real t; { t = 5; } }")
      }
    }
  }
}
