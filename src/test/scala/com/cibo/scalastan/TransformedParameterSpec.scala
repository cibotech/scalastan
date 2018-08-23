package com.cibo.scalastan

class TransformedParameterSpec extends ScalaStanBaseSpec with ScalaStan {
  describe("ParameterTransform") {
    it("should allow assignment to the result") {
      val t = new TransformedParameter(real()) {
        result := 1
      }
      val model = new Model { local(real()) := t; }
      checkCode(model, "transformed parameters { real t; { t = 1; } }")
    }

    it("should allow updating the result") {
      val t = new TransformedParameter(real()) {
        result += 1
      }
      val model = new Model { local(real()) := t; }
      checkCode(model, "transformed parameters { real t; { t += 1; } }")
    }

    it("should allow updating the result by index") {
      val t = new TransformedParameter(vector(2)) {
        result(1) := 2.0
      }
      val model = new Model { local(vector(2)) := t; }
      checkCode(model, "transformed parameters { vector[2] t; { t[1] = 2.0; } }")
    }

    it("should allow updating the result by slice") {
      val t = new TransformedParameter(vector(5)) {
        result(range(1, 2)) := result(range(3, 4))
      }
      val model = new Model {local(vector(5)) := t;}
      checkCode(model, "transformed parameters { vector[5] t; { t[1:2] = t[3:4]; } }")
    }

    it("should output dependencies in the right order") {
      val a = new TransformedParameter(real()) {
        result := 5
      }
      val b = new TransformedParameter(real()) {
        result := a
      }
      val model = new Model {local(real()) := a; local(real()) := b}
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
      val d = data(real())
      val pt = new TransformedParameter(real(lower = 1 / d)) {
        result := 0.1
      }
      val model = new Model { local(real()) := pt }
      checkCode(model, "transformed parameters { real<lower=(1) / (d)> pt; { pt = 0.1; } }")
    }
  }
}
