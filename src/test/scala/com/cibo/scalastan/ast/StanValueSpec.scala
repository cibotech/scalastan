package com.cibo.scalastan.ast

import com.cibo.scalastan._
import scala.language.existentials

class StanValueSpec extends ScalaStanBaseSpec with ScalaStan {

  describe("+") {
    describe("ints") {
      it("can add int + const_int") {
        val r = StanConstant[StanInt](StanInt(), 1) + 3
        r.emit shouldBe "(1 + 3)"
      }
      it("can add const_int + int") {
        import com.cibo.scalastan.Implicits._
        val r = 2 + StanConstant[StanInt](StanInt(), 1)
        r.emit shouldBe "(2 + 1)"
      }
      it("can add int + int") {
        val r = StanConstant[StanInt](StanInt(), 1) + StanConstant[StanInt](StanInt(), 2)
        r.emit shouldBe "(1 + 2)"
      }
    }

    describe("ints and doubles") {
      it("can add double + const_int") {
        val r = StanConstant[StanReal](StanReal(), 1) + 3
        r.emit shouldBe "(1.0 + 3)"
      }
      it("can add const_int + double") {
        import com.cibo.scalastan.Implicits._
        val r = 2 + StanConstant[StanReal](StanReal(), 1)
        r.emit shouldBe "(2 + 1.0)"
      }
      it("can add int + double") {
        val r = StanConstant[StanInt](StanInt(), 1) + StanConstant[StanReal](StanReal(), 2)
        r.emit shouldBe "(1 + 2.0)"
      }
      it("can add double + int") {
        val r = StanConstant[StanReal](StanReal(), 1) + StanConstant[StanInt](StanInt(), 2)
        r.emit shouldBe "(1.0 + 2)"
      }
    }

    describe("doubles") {
      it("can add double + const_double") {
        val r = StanConstant[StanReal](StanReal(), 1) + 3.0
        r.emit shouldBe "(1.0 + 3.0)"
      }
      it("can add const_double + double") {
        import com.cibo.scalastan.Implicits._
        val r = 2.0 + StanConstant[StanReal](StanReal(), 1)
        r.emit shouldBe "(2.0 + 1.0)"
      }
      it("can add double + double") {
        val r = StanConstant[StanReal](StanReal(), 1) + StanConstant[StanReal](StanReal(), 2)
        r.emit shouldBe "(1.0 + 2.0)"
      }
    }

    describe("vectors and ints") {
      it("can add vector + const_int") {
        val r = StanLocalDeclaration[StanVector](StanVector(StanConstant[StanInt](StanInt(), 1)), "x") + 1
        check(r.emit, "(x + 1)")
      }
    }
  }

  describe("*") {
    it("can multiply doubles") {
      val r = StanConstant[StanReal](StanReal(), 1) * 3.0
      r.emit shouldBe "(1.0 * 3.0)"
    }

    it("can multiply matrices") {
      val mat = StanLocalDeclaration(
        StanMatrix(StanConstant[StanInt](StanInt(), 2), StanConstant[StanInt](StanInt(), 2)),
        "mat"
      )
      val r = mat * mat
      r.emit shouldBe "(mat * mat)"
    }
  }

  describe(":/") {
    it("can divide vectors") {
      val n = StanLocalDeclaration(StanInt(), "n")
      val r = StanLocalDeclaration(StanVector(n), "x") /:/ StanLocalDeclaration(StanVector(n), "y")
      check(r.emit, "(x ./ y)")
    }

    it("can divide scalar by vector") {
      val n = StanLocalDeclaration(StanInt(), "n")
      val r = n /:/ StanLocalDeclaration(StanVector(n), "x")
      check(r.emit, "(n ./ x)")
    }

    it("can divide matrix by scalar") {
      val n = StanLocalDeclaration(StanInt(), "n")
      val r = StanLocalDeclaration(StanMatrix(n, n), "x") /:/ n
      check(r.emit, "(x ./ n)")
    }

    it("can not divide scalar by scalar") {
      val r = StanLocalDeclaration(StanReal(), "r")
      "r /:/ r" shouldNot compile
    }
  }

  describe("^") {
    it("can pow ints") {
      val r = StanConstant[StanInt](StanInt(), 1) ^ StanConstant[StanInt](StanInt(), 2)
      r.emit shouldBe "(1 ^ 2)"
    }
    it("can pow real / int") {
      val r = StanConstant[StanReal](StanReal(), 1.0) ^ StanConstant[StanInt](StanInt(), 2)
      r.emit shouldBe "(1.0 ^ 2)"
    }
    it("can pow reals") {
      val r = StanConstant[StanReal](StanReal(), 1.0) ^ StanConstant[StanReal](StanReal(), 2.0)
      r.emit shouldBe "(1.0 ^ 2.0)"
    }
  }

  describe("unary -") {
    it("can negate ints") {
      val r = -StanConstant[StanInt](StanInt(), 1)
      r.emit shouldBe "(-1)"
    }
  }

  describe("implicit conversion") {
    it("can convert int to real") {
      val model = new Model {
        local(real()) := 1
      }
      checkCode(model, "v# = 1;")
    }

    it("can not convert real to int") {
      "new Model { local(int()) := 1.0 }" shouldNot compile
    }
  }

  describe("index (with assignment)") {
    it("can be read 1d") {
      val i = StanConstant[StanInt](StanInt(), 1)
      val d = StanLocalDeclaration(StanReal()(i), "d").apply(i)
      d.emit shouldBe "d[1]"
    }

    it("can be read vector") {
      val i = StanConstant[StanInt](StanInt(), 1)
      val d = StanLocalDeclaration(StanVector(i), "d").apply(i)
      d.emit shouldBe "d[1]"
    }

    it("can be read 2d") {
      val i1 = StanConstant[StanInt](StanInt(), 1)
      val i2 = StanConstant[StanInt](StanInt(), 2)
      val d = StanLocalDeclaration(StanReal()(i1, i2), "d").apply(i1, i2)
      d.emit shouldBe "d[1,2]"
    }

    it("can be read matrix") {
      val i1 = StanConstant[StanInt](StanInt(), 1)
      val i2 = StanConstant[StanInt](StanInt(), 2)
      val d = StanLocalDeclaration(StanMatrix(i1, i2), "d").apply(i1, i2)
      d.emit shouldBe "d[1,2]"
    }

    it("can slice a matrix") {
      val i1 = StanConstant[StanInt](StanInt(), 1)
      val i2 = StanConstant[StanInt](StanInt(), 2)
      val d = StanLocalDeclaration(StanMatrix(i1, i2), "d").apply(i1)
      d.returnType shouldBe an[StanRowVector]
      d.emit shouldBe "d[1]"
    }

    it("can assign to a slice of a matrix") {
      val model = new Model {
        val d = local(matrix(1, 2))
        val rv = local(rowVector(2))
        d(1) := rv
      }
      checkCode(model, "d[1] = rv;")
    }

    it("can set vector") {
      val model = new Model {
        val a = local(vector(5))
        a(2) := 2
      }
      checkCode(model, "vector[5] a; a[2] = 2;")
    }

    it("can set matrix") {
      val model = new Model {
        val a = local(matrix(5, 6))
        a(2, 3) := 4
      }
      checkCode(model, "matrix[5,6] a; a[2,3] = 4;")
    }
  }

  describe("index (read-only)") {
    it("can be read vector") {
      val i = StanConstant[StanInt](StanInt(), 1)
      val d = StanParameterDeclaration(StanVector(i), "d").apply(i)
      d.emit shouldBe "d[1]"
    }

    it("can not set vector") {
      """
      val n = data(int())
      val p = parameter(vector(n))
      new Model {
        p(1) := 2
      }
      """ shouldNot compile
    }
  }

  describe("slice") {
    it("can be read") {
      val i1 = StanConstant[StanInt](StanInt(), 1)
      val i2 = StanConstant[StanInt](StanInt(), 2)
      val d = StanLocalDeclaration(StanVector(i1), "x").apply(StanValueRange(i1, i2))
      d.emit shouldBe "x[1:2]"
    }
  }

  describe("multiple indexes") {
    it("can have multiple indexes") {
      val n = StanDataDeclaration[StanInt](StanInt(), "n")
      val i = StanDataDeclaration[StanArray[StanInt]](StanArray(n, StanInt()), "i")
      val vec = StanDataDeclaration[StanArray[StanReal]](StanArray(n, StanReal()), "vec")
      vec(i).emit shouldBe "vec[i]"
    }
  }

  describe("indexed expression") {
    it("can index into an expression") {
      val n = StanDataDeclaration[StanInt](StanInt(), "n")
      val v = StanDataDeclaration[StanVector](StanVector(n), "v")
      val expression = (v + 1.0).apply(2)
      expression.emit shouldBe "(v + 1.0)[2]"
    }
  }

  describe("ternary operator") {
    it("emits the right thing") {
      val model = new Model {
        val cond = local(int())
        val left = local(real())
        val right = local(real())
        target += when(cond, left, right)
      }
      checkCode(model, "target += (cond ? left : right)")
    }
  }
}
