package com.cibo.scalastan

class TransformedParameterSpec extends ScalaStanBaseSpec {
  describe("ParameterTransform") {
    it("should allow assignment to the result") {
      val model = new StanModel {
        val t = new TransformedParameter(real()) {
          result := 1
        }
        local(real()) := t
      }
      checkCode(model, "transformed parameters { real t; { t = 1; } }")
    }

    it("should allow updating the result") {
      val model = new StanModel {
        val t = new TransformedParameter(real()) {
          result += 1
        }
        local(real()) := t
      }
      checkCode(model, "transformed parameters { real t; { t += 1; } }")
    }

    it("should allow updating the result by index") {
      val model = new StanModel {
        val t = new TransformedParameter(vector(2)) {
          result(1) := 2.0
        }
        local(vector(2)) := t
      }
      checkCode(model, "transformed parameters { vector[2] t; { t[1] = 2.0; } }")
    }

    it("should allow updating the result by slice") {
      val model = new StanModel {
        val t = new TransformedParameter(vector(5)) {
          result(range(1, 2)) := result(range(3, 4))
        }
        local(vector(5)) := t
      }
      checkCode(model, "transformed parameters { vector[5] t; { t[1:2] = t[3:4]; } }")
    }

    it("should output dependencies in the right order") {
      val model = new StanModel {
        val a = new TransformedParameter(real()) {
          result := 5
        }
        val b = new TransformedParameter(real()) {
          result := a
        }
        local(real()) := a
        local(real()) := b
      }
      checkCode(model, "transformed parameters { real a; real b; { a = 5; } { b = a; } }")
    }

    it("should not allow assignment to data") {
      """
      val d = data(real())
      new TransformedParameter(real()) {
        d := 1
      }
      """ shouldNot compile
    }

    it("should support non-trivial bounds") {
      val model = new StanModel {
        val d = data(real())
        val pt = new TransformedParameter(real(lower = 1 / d)) {
          result := 0.1
        }
        local(real()) := pt
      }
      checkCode(model, "transformed parameters { real<lower=(1 / d)> pt; { pt = 0.1; } }")
    }

    it("should preserve transformed parameters used in conditionals") {
      val model = new StanModel {
        val tp = new TransformedParameter(real()) {
          result := 1.0
        }
        when(tp > 0) {
        }
      }
      checkCode(model, "transformed parameters { real tp; { tp = 1.0; } }")
    }
  }
}
