package com.cibo.scalastan

class GeneratedQuantitySpec extends ScalaStanBaseSpec {
  describe("GeneratedQuantity") {
    it("should allow assignment to the result") {
      new ScalaStan {
        new GeneratedQuantity(real()) {
          result := 5
        }
        val model = new Model {}
        checkCode(model, "generated quantities { real v#; // v# v# = 5.0; }")
      }
    }
  }
}
