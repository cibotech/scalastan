package com.cibo.scalastan

class FunctionSpec extends ScalaStanBaseSpec {
  describe("Function") {
    it("should create simple functions") {
      new ScalaStan {
        val f = new Function(real()) {
          output(5)
        }
        val model = new Model {
          f()
        }
        checkCode(model, "functions { real v#() { return 5.0; }")
      }
    }

    it("should create functions with parameters") {
      new ScalaStan {
        val f = new Function(real()) {
          val i = input(real())
          output(i)
        }
        val model = new Model {
          f(5)
        }
        checkCode(model, "functions { real v#(real v#) { return v#; }")
      }
    }
  }
}
