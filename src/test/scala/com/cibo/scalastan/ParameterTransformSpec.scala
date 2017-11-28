package com.cibo.scalastan

class ParameterTransformSpec extends ScalaStanBaseSpec {
  describe("ParameterTransform") {
    it("should allow assignment to the result") {
      new ScalaStan {
        new ParameterTransform(real()) {
          result := 1
        }
        val model = new Model {}
        checkCode(model, "transformed parameters { real v#; v# = 1; }")
      }
    }

    it("should allow updating the result") {
      new ScalaStan {
        new ParameterTransform(real()) {
          result += 1
        }
        val model = new Model {}
        checkCode(model, "transformed parameters { real v#; v# += 1; }")
      }
    }

    it("should allow updating the result by index") {
      new ScalaStan {
        new ParameterTransform(vector(2)) {
          result(1) := 2.0
        }
        val model = new Model {}
        checkCode(model, "transformed parameters { vector[2] v#; v#[1] = 2.0; }")
      }
    }

    it("should allow updating the result by slice") {
      new ScalaStan {
        new ParameterTransform(vector(5)) {
          result(range(1, 2)) := result(range(3, 4))
        }
        val model = new Model {}
        checkCode(model, "transformed parameters { vector[5] v#; v#[1:2] = v#[3:4]; }")
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
