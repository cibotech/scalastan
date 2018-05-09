package com.cibo.scalastan

class TransformedParameterSpec extends ScalaStanBaseSpec {
  describe("ParameterTransform") {
    it("should allow assignment to the result") {
      new ScalaStan {
        val t = new TransformedParameter(real()) {
          result := 1
        }
        val model = new Model { local(real()) := t; }
        checkCode(model, "transformed parameters { real t; { t = 1; } }")
      }
    }

    it("should allow updating the result") {
      new ScalaStan {
        val t = new TransformedParameter(real()) {
          result += 1
        }
        val model = new Model { local(real()) := t; }
        checkCode(model, "transformed parameters { real t; { t += 1; } }")
      }
    }

    it("should allow updating the result by index") {
      new ScalaStan {
        val t = new TransformedParameter(vector(2)) {
          result(1) := 2.0
        }
        val model = new Model { local(vector(2)) := t; }
        checkCode(model, "transformed parameters { vector[2] t; { t[1] = 2.0; } }")
      }
    }

    it("should allow updating the result by slice") {
      new ScalaStan {
        val t = new TransformedParameter(vector(5)) {
          result(range(1, 2)) := result(range(3, 4))
        }
        val model = new Model { local(vector(5)) := t; }
        checkCode(model, "transformed parameters { vector[5] t; { t[1:2] = t[3:4]; } }")
      }
    }

    it("should output dependencies in the right order") {
      new ScalaStan {
        val a = new TransformedParameter(real()) {
          result := 5
        }
        val b = new TransformedParameter(real()) {
          result := a
        }
        val model = new Model { local(real()) := a; local(real()) := b }
        checkCode(model, "transformed parameters { real a; real b; { a = 5; } { b = a; } }")
      }
    }

    it("should not allow assignment to data") {
      """
      new ScalaStan {
        val d = data(real())
        new ParameterTransform(real()) {
          d := 1
        }
      }
      """ shouldNot compile
    }
  }
}
