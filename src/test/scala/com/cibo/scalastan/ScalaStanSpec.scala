package com.cibo.scalastan

class ScalaStanSpec extends ScalaStanBaseSpec with ScalaStan {
  describe("data") {
    it("causes a data declaration to be generated") {
      data(int())
      val model = new Model { }
      checkCode(model, "data { int v#; // v# }")
    }
  }

  describe("parameter") {
    it("causes a parameter declaration to be generated") {
      parameter(int())
      val model = new Model { }
      checkCode(model, "parameters { int v#; // v# }")
    }
  }

  describe("model") {
    describe("when") {
      it("generates an if statement") {
        val model = new Model {
          when(1) {
          }
        }
        checkCode(model, "model { if(1) { } }")
      }
    }

    describe(":=") {
      it("generates an assignment") {
        val model = new Model {
          val a = local(int())
          a := 1
        }
        checkCode(model, "model { int v#; v# = 1; }")
      }
    }

    describe("~") {
      it("generates a ~ statement") {
        val model = new Model {
          val a = local(real())
          a ~ Normal(0, 1)
        }
        checkCode(model, "model { real v#; v# ~ normal(0,1); }")
      }
    }

    describe("break") {
      it("is generated in for loops") {
        val model = new Model {
          for (_ <- range(1, 2)) {
            break
          }
        }
        checkCode(model, "model { for(v# in 1:2) { break; } }")
      }

      it ("is generated in while loops") {
        val model = new Model {
          loop(1) {
            break
          }
        }
        checkCode(model, "model { while(1) { break; } }")
      }

      it("is generated nested under an if") {
        val model = new Model {
          loop(1) {
            when(1) {
              break
            }
          }
        }
        checkCode(model, "model { while(1) { if(1) { break; } } }")
      }

      it("is not allowed without a loop") {
        an[Exception] should be thrownBy {
          new Model {
            when(1) {
              break
            }
          }
        }
      }
    }

    describe("continue") {
      it("is generated in loops") {
        val model = new Model {
          loop(1) {
            continue
          }
        }
        checkCode(model, "model { while(1) { continue; } }")
      }
    }
  }
}
