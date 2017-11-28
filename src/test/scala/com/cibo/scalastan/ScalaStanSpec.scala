package com.cibo.scalastan

class ScalaStanSpec extends ScalaStanBaseSpec {
  describe("data") {
    it("causes a data declaration to be generated") {
      new ScalaStan {
        data(int())
        val model = new Model {}
        checkCode(model, "data { int v#; }")
      }
    }
  }

  describe("parameter") {
    it("causes a parameter declaration to be generated") {
      new ScalaStan {
        parameter(real())
        val model = new Model {}
        checkCode(model, "parameters { real v#; }")
      }
    }
  }

  describe("local") {
    it("places local declarations correctly when used inside a loop") {
      new ScalaStan {
        val model = new Model {
          val x = local(int())
          for (i <- range(1, 2)) {
            x += 1
          }
        }
        checkCode(model, "model { int v#; for(v# in 1:2) { v# += 1; } }")
      }
    }

    it("places local declarations correctly when declared after a loop") {
      new ScalaStan {
        val model = new Model {
          for (i <- range(1, 2)) {
          }
          val x = local(int())
          x += 1
        }
        checkCode(model, "model { int v#; for(v# in 1:2) { } v# += 1; }")
      }
    }

    it("places local declarations inside loops") {
      new ScalaStan {
        val model = new Model {
          for (i <- range(1, 2)) {
            val x = local(int())
            x += 1
          }
        }
        checkCode(model, "model { for(v# in 1:2) { int v#; v# += 1; } }")
      }
    }
  }

  describe("model") {
    describe("when") {
      it("generates an if statement") {
        new ScalaStan {
          val model = new Model {
            when(1) {
            }
          }
          checkCode(model, "model { if(1) { } }")
        }
      }

      it("generates an if - else if - else statement") {
        new ScalaStan {
          val model = new Model {
            when(1) {
            }.when(2) {
            } otherwise {
            }
          }
          checkCode(model, "model { if(1) {} else if(2) {} else {} }")
        }
      }
    }

    describe("assignment operators") {
      it("generates :=") {
        new ScalaStan {
          val model = new Model {
            local(int()) := 1
          }
          checkCode(model, "model { int v#; v# = 1; }")
        }
      }

      it("generates +=") {
        new ScalaStan {
          val model = new Model {
            local(int()) += 1
          }
          checkCode(model, "model { int v#; v# += 1; }")
        }
      }

      it("generates -=") {
        new ScalaStan {
          val model = new Model {
            local(int()) -= 1
          }
          checkCode(model, "model { int v#; v# -= 1; }")
        }
      }

      it("generates *=") {
        new ScalaStan {
          val model = new Model {
            local(int()) *= 1
          }
          checkCode(model, "model { int v#; v# *= 1; }")
        }
      }

      it("generates /=") {
        new ScalaStan {
          val model = new Model {
            local(int()) /= 1
          }
          checkCode(model, "model { int v#; v# /= 1; }")
        }
      }
    }

    describe("~") {
      it("generates a ~ statement") {
        new ScalaStan {
          val model = new Model {
            val a = local(real())
            a ~ Normal(0, 1)
          }
          checkCode(model, "model { real v#; v# ~ normal(0,1); }")
        }
      }
    }

    describe("operators") {
      it("generates +") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) + local(int())
          }
          checkCode(model, "v# = (v#) + (v#);")
        }
      }

      it("generates -") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) - local(int())
          }
          checkCode(model, "v# = (v#) - (v#);")
        }
      }

      it("generates *") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) * local(int())
          }
          checkCode(model, "v# = (v#) * (v#);")
        }
      }

      it("generates /") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) / local(int())
          }
          checkCode(model, "v# = (v#) / (v#);")
        }
      }

      it("generates \\") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(matrix(n, n)) := local(matrix(n, n)) \ local(matrix(n, n))
          }
          checkCode(model, "v# = (v#) \\ (v#);")
        }
      }

      it("generates ^") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) ^ local(int())
          }
          checkCode(model, "v# = (v#) ^ (v#);")
        }
      }

      it("generates %") {
        new ScalaStan {
          val model = new Model {
            local(int()) := local(int()) % local(int())
          }
          checkCode(model, "v# = (v#) % (v#);")
        }
      }

      it("generates transpose") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(matrix(n, n)) := local(matrix(n, n)).t
          }
          checkCode(model, "v# = (v#)';")
        }
      }

      it("generate index 1") {
        new ScalaStan {
          val model = new Model {
            val r = local(vector(local(int())))
            r(5) := 1
          }
          checkCode(model, "v#[5] = 1.0;")
        }
      }

      it("generate index 2") {
        new ScalaStan {
          val model = new Model {
            val r = local(matrix(local(int()), local(int())))
            r(5, 6) := 1
          }
          checkCode(model, "v#[5,6] = 1.0;")
        }
      }
    }

    describe("break") {
      it("is generated in for loops") {
        new ScalaStan {
          val model = new Model {
            for (_ <- range(1, 2)) {
              break
            }
          }
          checkCode(model, "model { for(v# in 1:2) { break; } }")
        }
      }

      it ("is generated in while loops") {
        new ScalaStan {
          val model = new Model {
            loop(1) {
              break
            }
          }
          checkCode(model, "model { while(1) { break; } }")
        }
      }

      it("is generated nested under an if") {
        new ScalaStan {
          val model = new Model {
            loop(1) {
              when(1) {
                break
              }
            }
          }
          checkCode(model, "model { while(1) { if(1) { break; } } }")
        }
      }

      it("is not allowed without a loop") {
        new ScalaStan {
          an[Exception] should be thrownBy {
            new Model {
              when(1) {
                break
              }
            }
          }
        }
      }
    }

    describe("continue") {
      it("is generated in loops") {
        new ScalaStan {
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
}
