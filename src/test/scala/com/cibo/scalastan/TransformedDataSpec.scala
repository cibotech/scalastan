package com.cibo.scalastan

class TransformedDataSpec extends ScalaStanBaseSpec {
  describe("DataTransform") {
    it("should allow assignment to result") {
      val model = new StanModel {
        val t = new TransformedData(real()) {
          result := 5.0
        }
        local(real()) := t
      }
      checkCode(model, "transformed data { real t; { t = 5.0; } }")
    }

    it("should not allow assignment to parameters") {
      """
      new StanModel {
        val p = parameter(real())
        new DataTransform(real()) {
          p := 5
        }
      }
      """ shouldNot compile
    }
  }
}
