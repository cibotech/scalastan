package com.cibo.scalastan

class FunctionSpec extends ScalaStanBaseSpec {
  describe("Function") {
    it("should create simple functions") {
      new ScalaStan {
        val f = new Function(real()) {
          output(5)
        }
        val model = new Model {
          local(real()) := f()
        }
        checkCode(model, "functions { real v#() { return 5.0; }")
      }
    }

    it("should create functions with parameters") {
      new ScalaStan {
        val f = new Function() {
          val i = input(real())
          stan.print(i)
        }
        val model = new Model {
          f(5)
        }
        checkCode(model, "functions { void v#(real i) { print(i); } } model { v#(5); }")
      }
    }
  }
}
