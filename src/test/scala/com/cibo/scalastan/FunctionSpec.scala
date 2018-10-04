package com.cibo.scalastan

class FunctionSpec extends ScalaStanBaseSpec {
  describe("Function") {
    it("should create simple functions") {
      object model extends StanModel {
        val f = new Function(real()) {
          output(5.0)
        }
        local(real()) := f()
      }
      checkCode(model, "functions { real f() { return 5.0; }")
    }

    it("should create functions with parameters") {
      object model extends StanModel {
        val f = new Function() {
          val i = input(real())
          stan.print(i)
        }
        f(5)
      }
      checkCode(model, "functions { void f(real i) { print(i); } } model { f(5); }")
    }

    it("should create functions with vector parameters") {
      object model extends StanModel {
        val f = new Function(vector()) {
          val x = input(vector())
          val y = input(vector()())
          output(x)
        }
        val x = local(vector(5))
        val y = local(vector(5)(7))
        local(vector(5)) := f(x, y)
      }
      checkCode(model, "functions { vector f(vector x, vector[] y) { return x; } }")
    }
  }
}
