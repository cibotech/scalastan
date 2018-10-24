package com.cibo.scalastan

class StanFunctionsSpec extends ScalaStanBaseSpec {
  describe("built-in functions") {
    it("generates reject") {
      object model extends StanModel {
        stan.reject(1)
      }
      checkCode(model, "model { reject(1); }")
    }

    it("generates print") {
      object model extends StanModel {
        stan.print("test", 1)
      }
      checkCode(model, "model { print(\"test\", 1); }")
    }

    describe("appendRow") {
      it("appendRow(real, vector)") {
        object model extends StanModel {
          val n = local(int())
          val dest = local(vector(n))
          val a = local(real())
          val b = local(vector(n))
          dest := stan.append_row(a, b)
        }
        checkCode(model, "dest = append_row(a, b);")
      }
    }

    describe("appendCol") {
      it("appendCol(real, row vector)") {
        object model extends StanModel {
          val n = local(int())
          val dest = local(rowVector(n))
          val a = local(real())
          val b = local(rowVector(n))
          dest := stan.append_col(a, b)
        }
        checkCode(model, "dest = append_col(a, b);")
      }
    }

    describe("min") {
      it("min(int, int)") {
        object model extends StanModel {
          val dest = local(int())
          val a = local(int())
          val b = local(int())
          dest := stan.min(a, b)
        }
        checkCode(model, "dest = min(a, b);")
      }

      it("min(vector)") {
        object model extends StanModel {
          val dest = local(real())
          val n = local(int())
          val vec = local(vector(n))
          dest := stan.min(vec)
        }
        checkCode(model, "dest = min(vec);")
      }
    }

    describe("abs") {
      it("can abs ints") {
        object model extends StanModel {
          val dest = local(int())
          val src = local(int())
          dest := stan.abs(src)
        }
        checkCode(model, "dest = abs(src);")
      }
    }

    describe("pow") {
      it("can pow ints") {
        object model extends StanModel {
          val x = local(real())
          val a = local(int())
          val b = local(int())
          x := stan.pow(a, b)
        }
        checkCode(model, "x = pow(a, b);")
      }

      it("can pow reals") {
        object model extends StanModel {
          val x = local(real())
          val a = local(real())
          val b = local(real())
          x := stan.pow(a, b)
        }
        checkCode(model, "x = pow(a, b);")
      }
    }
  }

  describe("distance") {
    it("does not compile for scalars") {
      "new StanModel { stan.distance(local(real()), local(real())) }" shouldNot compile
    }

    it("works with vectors") {
      object model extends StanModel {
        val n = local(int())
        val dest = local(real())
        val a = local(vector(n))
        val b = local(vector(n))
        dest := stan.distance(a, b)
      }
      checkCode(model, "dest = distance(a, b);")
    }

    it("works with vector x row_vector") {
      object model extends StanModel {
        val n = local(int())
        val dest = local(real())
        val a = local(vector(n))
        val b = local(rowVector(n))
        dest := stan.distance(a, b)
      }
      checkCode(model, "dest = distance(a, b);")
    }

    it("works with row_vector x vector") {
      object model extends StanModel {
        val n = local(int())
        val dest = local(real())
        val a = local(rowVector(n))
        val b = local(vector(n))
        dest := stan.distance(a, b)
      }
      checkCode(model, "dest = distance(a, b);")
    }
  }
}
