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

      it("generates an if - else if - else statement") {
        val model = new Model {
          when(1) {
          }.when(2) {
          } otherwise {
          }
        }
        checkCode(model, "model { if(1) {} else if(2) {} else {} }")
      }
    }

    describe("assignment operators") {
      it("generates :=") {
        val model = new Model {
          local(int()) := 1
        }
        checkCode(model, "model { int v#; v# = 1; }")
      }

      it("generates +=") {
        val model = new Model {
          local(int()) += 1
        }
        checkCode(model, "model { int v#; v# += 1; }")
      }

      it("generates -=") {
        val model = new Model {
          local(int()) -= 1
        }
        checkCode(model, "model { int v#; v# -= 1; }")
      }

      it("generates *=") {
        val model = new Model {
          local(int()) *= 1
        }
        checkCode(model, "model { int v#; v# *= 1; }")
      }

      it("generates /=") {
        val model = new Model {
          local(int()) /= 1
        }
        checkCode(model, "model { int v#; v# /= 1; }")
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

    describe("operators") {
      it("generates +") {
        val model = new Model {
          local(real()) := local(real()) + local(int())
        }
        checkCode(model, "v# = (v#) + (v#);")
      }

      it("generates -") {
        val model = new Model {
          local(real()) := local(real()) - local(int())
        }
        checkCode(model, "v# = (v#) - (v#);")
      }

      it("generates *") {
        val model = new Model {
          local(real()) := local(real()) * local(int())
        }
        checkCode(model, "v# = (v#) * (v#);")
      }

      it("generates /") {
        val model = new Model {
          local(real()) := local(real()) / local(int())
        }
        checkCode(model, "v# = (v#) / (v#);")
      }

      it("generates \\") {
        val model = new Model {
          val n = local(int())
          local(matrix(n, n)) := local(matrix(n, n)) \ local(matrix(n, n))
        }
        checkCode(model, "v# = (v#) \\ (v#);")
      }

      it("generates ^") {
        val model = new Model {
          local(real()) := local(real()) ^ local(int())
        }
        checkCode(model, "v# = (v#) ^ (v#);")
      }

      it("generates %") {
        val model = new Model {
          local(int()) := local(int()) % local(int())
        }
        checkCode(model, "v# = (v#) % (v#);")
      }

      it("generates transpose") {
        val model = new Model {
          val n = local(int())
          local(matrix(n, n)) := local(matrix(n, n)).t
        }
        checkCode(model, "v# = (v#)';")
      }

      it("generate index 1") {
        val model = new Model {
          val r = local(vector(local(int())))
          r(5) := 1
        }
        checkCode(model, "v#[5] = 1.0;")
      }

      it("generate index 2") {
        val model = new Model {
          val r = local(matrix(local(int()), local(int())))
          r(5, 6) := 1
        }
        checkCode(model, "v#[5,6] = 1.0;")
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
