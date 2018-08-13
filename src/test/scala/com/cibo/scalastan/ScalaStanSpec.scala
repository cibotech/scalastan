package com.cibo.scalastan

class ScalaStanSpec extends ScalaStanBaseSpec {
  describe("data") {
    it("causes a data declaration to be generated") {
      new ScalaStan {
        val d = data(int())
        val model = new Model { local(int()) := d }
        checkCode(model, "data { int d; }")
      }
    }
  }

  describe("parameter") {
    it("causes a parameter declaration to be generated") {
      new ScalaStan {
        val p = parameter(real())
        val model = new Model { local(real()) := p }
        checkCode(model, "parameters { real p; }")
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
        checkCode(model, "model { int x; for(ss_v# in 1:2) { x += 1; } }")
      }
    }

    it("places local declarations correctly when declared after a loop") {
      new ScalaStan {
        val model = new Model {
          for (i <- range(1, 2)) {
            stan.print(i)
          }
          val x = local(int())
          x += 1
        }
        checkCode(model, "model { int x; for(ss_v# in 1:2) { print(ss_v#); } x += 1; }")
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
        checkCode(model, "model { for(ss_v# in 1:2) { int x; x += 1; } }")
      }
    }
  }

  describe("model") {
    it("allows the same parameter with multiple models") {
      new ScalaStan {
        val param = parameter(real())
        val model1 = new Model {
          param ~ stan.normal(0, 1)
        }
        val model2 = new Model {
          param ~ stan.normal(2, 3)
        }

        checkCode(model1, "model { param ~ normal(0,1); }")
        checkCode(model2, "model { param ~ normal(2,3); }")
      }
    }

    describe("when") {
      it("generates an if statement") {
        new ScalaStan {
          val model = new Model {
            when(1) {
              stan.print(1)
            }
          }
          checkCode(model, "model { if(1) { print(1); } }")
        }
      }

      it("generates an if - else if - else statement") {
        new ScalaStan {
          val model = new Model {
            when(1) {
              stan.print(1)
            }.when(2) {
              stan.print(2)
            } otherwise {
              stan.print(3)
            }
          }
          checkCode(model, "model { if(1) { print(1); } else if(2) { print(2); } else { print(3); } }")
        }
      }
    }

    describe("assignment operators") {
      it("generates :=") {
        new ScalaStan {
          val model = new Model {
            local(int()) := 1
          }
          checkCode(model, "model { int ss_v#; ss_v# = 1; }")
        }
      }

      it("generates +=") {
        new ScalaStan {
          val model = new Model {
            local(int()) += 1
          }
          checkCode(model, "model { int ss_v#; ss_v# += 1; }")
        }
      }

      it("generates -=") {
        new ScalaStan {
          val model = new Model {
            local(int()) -= 1
          }
          checkCode(model, "model { int ss_v#; ss_v# -= 1; }")
        }
      }

      it("generates *=") {
        new ScalaStan {
          val model = new Model {
            local(int()) *= 1
          }
          checkCode(model, "model { int ss_v#; ss_v# *= 1; }")
        }
      }

      it("generates /=") {
        new ScalaStan {
          val model = new Model {
            local(int()) /= 1
          }
          checkCode(model, "model { int ss_v#; ss_v# /= 1; }")
        }
      }
    }

    describe("~") {
      it("generates a ~ statement") {
        new ScalaStan {
          val model = new Model {
            val a = local(real())
            a ~ stan.normal(0, 1)
          }
          checkCode(model, "model { real a; a ~ normal(0,1); }")
        }
      }
    }

    describe("operators") {
      it("generates +") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) + local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) + (ss_v#);")
        }
      }

      it("generates -") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) - local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) - (ss_v#);")
        }
      }

      it("generates *") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) * local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) * (ss_v#);")
        }
      }

      it("generates /") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) / local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) / (ss_v#);")
        }
      }

      it("generates \\") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(matrix(n, n)) := local(matrix(n, n)) \ local(matrix(n, n))
          }
          checkCode(model, "ss_v# = (ss_v#) \\ (ss_v#);")
        }
      }

      it("generates ^") {
        new ScalaStan {
          val model = new Model {
            local(real()) := local(real()) ^ local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) ^ (ss_v#);")
        }
      }

      it("generates %") {
        new ScalaStan {
          val model = new Model {
            local(int()) := local(int()) % local(int())
          }
          checkCode(model, "ss_v# = (ss_v#) % (ss_v#);")
        }
      }

      it("generates transpose") {
        new ScalaStan {
          val model = new Model {
            val n = local(int())
            local(matrix(n, n)) := local(matrix(n, n)).t
          }
          checkCode(model, "ss_v# = (ss_v#)';")
        }
      }

      it("generate index 1") {
        new ScalaStan {
          val model = new Model {
            val r = local(vector(local(int())))
            r(5) := 1
          }
          checkCode(model, "r[5] = 1;")
        }
      }

      it("generate index 2") {
        new ScalaStan {
          val model = new Model {
            val r = local(matrix(local(int()), local(int())))
            r(5, 6) := 1
          }
          checkCode(model, "r[5,6] = 1;")
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
          checkCode(model, "model { for(ss_v# in 1:2) { break; } }")
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
            }.getCode
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
