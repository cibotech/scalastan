package com.cibo.scalastan

class StanBuiltInFunctionsSpec extends ScalaStanBaseSpec {
  describe("built-in functions") {
    it("generates reject") {
      val model = new Model {
        reject(1)
      }
      checkCode(model, "model { reject(1); }")
    }

    it("generates print") {
      val model = new Model {
        print("test", 1)
      }
      checkCode(model, "model { print(\"test\", 1); }")
    }

    describe("appendRow") {
      it("appendRow(real, vector)") {
        val model = new Model {
          val n = local(int())
          local(vector(n)) := appendRow(local(real()), local(vector(n)))
        }
        checkCode(model, "v# = append_row(v#, v#);")
      }
    }

    describe("appendCol") {
      it("appendCol(real, row vector)") {
        val model = new Model {
          val n = local(int())
          local(rowVector(n)) := appendCol(local(real()), local(rowVector(n)))
        }
        checkCode(model, "v# = append_col(v#, v#);")
      }
    }

    describe("min") {
      it("min(int, int)") {
        val model = new Model {
          local(int()) := min(local(int()), local(int()))
        }
        checkCode(model, "v# = min(v#, v#);")
      }

      it("min(vector)") {
        val model = new Model {
          local(real()) := min(local(vector(local(int()))))
        }
        checkCode(model, "v# = min(v#);")
      }
    }
  }
}
