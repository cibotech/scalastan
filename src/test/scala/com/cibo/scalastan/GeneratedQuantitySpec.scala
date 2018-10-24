package com.cibo.scalastan

class GeneratedQuantitySpec extends ScalaStanBaseSpec {
  describe("GeneratedQuantity") {
    it("should allow assignment to the result (inside)") {
      object model extends StanModel {
        val t = new GeneratedQuantity(real()) {
          result := 5
        }
      }
      checkCode(model, "generated quantities { real t; { t = 5; } }")
    }

    it("should allow assignment to the result (outside)") {
      object model extends StanModel {
        val t = new GeneratedQuantity(real()) {
          result := 5
        }
      }
      checkCode(model, "generated quantities { real t; { t = 5; } }")
    }
  }
}
