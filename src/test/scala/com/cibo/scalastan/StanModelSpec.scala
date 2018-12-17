package com.cibo.scalastan

import java.io.{PrintWriter, StringWriter}

class StanModelSpec extends ScalaStanBaseSpec {
  describe("data") {
    it("causes a data declaration to be generated") {
      object model extends StanModel {
        val d = data(int())
        local(int()) := d
      }
      checkCode(model, "data { int d; }")
    }

    it("renames keywords") {
      object model extends StanModel {
        val mean = data(int())
        local(int()) := mean
      }
      checkCode(model, "data { int ss_v#; }")
    }
  }

  describe("parameter") {
    it("causes a parameter declaration to be generated") {
      object model extends StanModel {
        val p = parameter(real())
        local(real()) := p
      }
      checkCode(model, "parameters { real p; }")
    }
  }

  describe("local") {
    it("places local declarations correctly when used inside a loop") {
      object model extends StanModel {
        val x = local(int())
        for (i <- range(1, 2)) {
          x += 1
        }
      }
      checkCode(model, "model { int x; for(ss_v# in 1:2) { x += 1; } }")
    }

    it("places local declarations correctly when declared after a loop") {
      object model extends StanModel {
        for (i <- range(1, 2)) {
          stan.print(i)
        }
        val x = local(int())
        x += 1
      }
      checkCode(model, "model { int x; for(ss_v# in 1:2) { print(ss_v#); } x += 1; }")
    }

    it("places local declarations inside loops") {
      object model extends StanModel {
        for (i <- range(1, 2)) {
          val x = local(int())
          x += 1
        }
      }
      checkCode(model, "model { for(ss_v# in 1:2) { int x; x += 1; } }")
    }

    it("supports initial values") {
      object model extends StanModel {
        val x = local(real(), 2.5)
        x += 1.0
      }
      checkCode(model, "model { real x = 2.5; x += 1.0; }")
    }
  }

  describe("model") {
    it("allows the same parameter with multiple models") {
      object model1 extends StanModel {
        val param = parameter(real())
        param ~ stan.normal(0, 1)
      }
      object model2 extends StanModel {
        model1.param ~ stan.normal(2, 3)
      }

      checkCode(model1, "model { param ~ normal(0,1); }")
      checkCode(model2, "model { param ~ normal(2,3); }")
    }

    describe("when") {
      it("generates an if statement") {
        object model extends StanModel {
          when(1) {
            stan.print(1)
          }
        }
        checkCode(model, "model { if(1) { print(1); } }")
      }

      it("generates an if - else if - else statement") {
        object model extends StanModel {
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

    describe("assignment operators") {
      it("generates :=") {
        object model extends StanModel {
          val x = local(int())
          x := 1
        }
        checkCode(model, "model { int x; x = 1; }")
      }

      it("generates +=") {
        object model extends StanModel {
          val x = local(int())
          x += 1
        }
        checkCode(model, "model { int x; x += 1; }")
      }

      it("generates -=") {
        object model extends StanModel {
          val x = local(int())
          x -= 1
        }
        checkCode(model, "model { int x; x -= 1; }")
      }

      it("generates *=") {
        object model extends StanModel {
          val x = local(int())
          x *= 1
        }
        checkCode(model, "model { int x; x *= 1; }")
      }

      it("generates /=") {
        object model extends StanModel {
          val x = local(int())
          x /= 1
        }
        checkCode(model, "model { int x; x /= 1; }")
      }
    }

    describe("~") {
      it("generates a ~ statement") {
        object model extends StanModel {
          val a = local(real())
          a ~ stan.normal(0, 1)
        }
        checkCode(model, "model { real a; a ~ normal(0,1); }")
      }
    }

    describe("operators") {
      it("generates +") {
        object model extends StanModel {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y + z
        }
        checkCode(model, "x = (y + z);")
      }

      it("generates -") {
        object model extends StanModel {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y - z
        }
        checkCode(model, "x = (y - z);")
      }

      it("generates *") {
        object model extends StanModel {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y * z
        }
        checkCode(model, "x = (y * z);")
      }

      it("generates /") {
        object model extends StanModel {
          val x = local(real())
          val y = local(real())
          val z = local(int())
          x := y / z
        }
        checkCode(model, "x = (y / z);")
      }

      it("generates \\") {
        object model extends StanModel {
          val n = local(int())
          val a = local(matrix(n, n))
          val b = local(matrix(n, n))
          val c = local(matrix(n, n))
          a := b \ c
        }
        checkCode(model, "a = (b \\ c);")
      }

      it("generates ^") {
        object model extends StanModel {
          val a = local(real())
          val b = local(real())
          val c = local(int())
          a := b ^ c
        }
        checkCode(model, "a = (b ^ c);")
      }

      it("generates %") {
        object model extends StanModel {
          val a = local(int())
          val b = local(int())
          val c = local(int())
          a := b % c
        }
        checkCode(model, "a = (b % c);")
      }

      it("generates transpose") {
        object model extends StanModel {
          val n = local(int())
          val a = local(matrix(n, n))
          val b = local(matrix(n, n))
          a := b.t
        }
        checkCode(model, "a = b';")
      }

      it("generate index 1") {
        object model extends StanModel {
          val r = local(vector(local(int())))
          r(5) := 1
        }
        checkCode(model, "r[5] = 1;")
      }

      it("generate index 2") {
        object model extends StanModel {
          val r = local(matrix(local(int()), local(int())))
          r(5, 6) := 1
        }
        checkCode(model, "r[5,6] = 1;")
      }
    }

    describe("break") {
      it("is generated in for loops") {
        object model extends StanModel {
          for (_ <- range(1, 2)) {
            break
          }
        }
        checkCode(model, "model { for(ss_v# in 1:2) { break; } }")
      }

      it ("is generated in while loops") {
        object model extends StanModel {
          loop(1) {
            break
          }
        }
        checkCode(model, "model { while(1) { break; } }")
      }

      it("is generated nested under an if") {
        object model extends StanModel {
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
          new StanModel {
            when(1) {
              break
            }
          }.emit(new PrintWriter(new StringWriter()))
        }
      }
    }

    describe("continue") {
      it("is generated in loops") {
        object model extends StanModel {
          loop(1) {
            continue
          }
        }
        checkCode(model, "model { while(1) { continue; } }")
      }
    }
  }
}
