package com.cibo.scalastan

class StanBuiltInFunctionsSpec extends ScalaStanBaseSpec {
  describe("built-in functions") {
    it("generates reject") {
      new ScalaStan {
        val model = new Model {
          reject(1)
        }
        checkCode(model, "model { reject(1); }")
      }
    }

    it("generates print") {
      new ScalaStan {
        val model = new Model {
          print("test", 1)
        }
        checkCode(model, "model { print(\"test\", 1); }")
      }
    }

    describe("appendRow") {
      it("appendRow(real, vector)") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(vector(n)) := appendRow(local(real()), local(vector(n)))
          }
          checkCode(model, "v# = append_row(v#, v#);")
        }
      }
    }

    describe("appendCol") {
      it("appendCol(real, row vector)") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(rowVector(n)) := appendCol(local(real()), local(rowVector(n)))
          }
          checkCode(model, "v# = append_col(v#, v#);")
        }
      }
    }

    describe("min") {
      it("min(int, int)") {
        new ScalaStan {
          val model = new Model {
            local(int()) := min(local(int()), local(int()))
          }
          checkCode(model, "v# = min(v#, v#);")
        }
      }

      it("min(vector)") {
        new ScalaStan {
          val model = new Model {
            local(real()) := min(local(vector(local(int()))))
          }
          checkCode(model, "v# = min(v#);")
        }
      }
    }

    describe("abs") {
      it("can abs ints") {
        new ScalaStan {
          val model = new Model {
            local(int()) := abs(local(int()))
          }
          checkCode(model, "v# = abs(v#);")
        }
      }
    }

    describe("pow") {
      it("can pow ints") {
        new ScalaStan {
          val model = new Model {
            local(real()) := pow(local(int()), local(int()))
          }
          checkCode(model, "v# = pow(v#,v#);")
        }
      }

      it("can pow reals") {
        new ScalaStan {
          val model = new Model {
            local(real()) := pow(local(real()), local(real()))
          }
          checkCode(model, "v# = pow(v#,v#);")
        }
      }
    }
  }

  describe("distance") {
    it("does not compile for scalars") {
      "new ScalaStan { new Model { distance(local(real()), local(real())) } }" shouldNot compile
    }

    it("works with vectors") {
      new ScalaStan {
        val model = new Model {
          val n = local(int())
          local(real()) := distance(local(vector(n)), local(vector(n)))
        }
        checkCode(model, "v# = distance(v#,v#);")
      }
    }

    it("works with vector x row_vector") {
      new ScalaStan {
        val model = new Model {
          val n = local(int())
          local(real()) := distance(local(vector(n)), local(rowVector(n)))
        }
        checkCode(model, "v# = distance(v#,v#);")
      }
    }

    it("works with row_vector x vector") {
      new ScalaStan {
        val model = new Model {
          val n = local(int())
          local(real()) := distance(local(rowVector(n)), local(vector(n)))
        }
        checkCode(model, "v# = distance(v#,v#);")
      }
    }
  }
}
