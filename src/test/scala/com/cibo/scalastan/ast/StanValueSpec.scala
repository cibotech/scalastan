package com.cibo.scalastan.ast

import com.cibo.scalastan._

class StanValueSpec extends ScalaStanBaseSpec {

  private implicit val ss = new ScalaStan {}

  describe("+") {
    describe("ints") {
      it("can add int + const_int") {
        val r = StanConstant[StanInt](1) + 3
        r.emit shouldBe "(1) + (3)"
      }
      it("can add const_int + int") {
        import com.cibo.scalastan.Implicits._
        val r = 2 + StanConstant[StanInt](1)
        r.emit shouldBe "(2) + (1)"
      }
      it("can add int + int") {
        val r = StanConstant[StanInt](1) + StanConstant[StanInt](2)
        r.emit shouldBe "(1) + (2)"
      }
    }

    describe("ints and doubles") {
      it("can add double + const_int") {
        val r = StanConstant[StanReal](1) + 3
        r.emit shouldBe "(1.0) + (3)"
      }
      it("can add const_int + double") {
        import com.cibo.scalastan.Implicits._
        val r = 2 + StanConstant[StanReal](1)
        r.emit shouldBe "(2) + (1.0)"
      }
      it("can add int + double") {
        val r = StanConstant[StanInt](1) + StanConstant[StanReal](2)
        r.emit shouldBe "(1) + (2.0)"
      }
      it("can add double + int") {
        val r = StanConstant[StanReal](1) + StanConstant[StanInt](2)
        r.emit shouldBe "(1.0) + (2)"
      }
    }

    describe("doubles") {
      it("can add double + const_double") {
        val r = StanConstant[StanReal](1) + 3.0
        r.emit shouldBe "(1.0) + (3.0)"
      }
      it("can add const_double + double") {
        import com.cibo.scalastan.Implicits._
        val r = 2.0 + StanConstant[StanReal](1)
        r.emit shouldBe "(2.0) + (1.0)"
      }
      it("can add double + double") {
        val r = StanConstant[StanReal](1) + StanConstant[StanReal](2)
        r.emit shouldBe "(1.0) + (2.0)"
      }
    }

    describe("vectors and ints") {
      it("can add vector + const_int") {
        val r = StanLocalDeclaration[StanVector](StanVector(StanConstant[StanInt](1))) + 1
        check(r.emit, "(v#) + (1)")
      }
    }
  }

  describe(":/") {
    it("can divide vectors") {
      val n = StanLocalDeclaration(StanInt())
      val r = StanLocalDeclaration(StanVector(n)) /:/ StanLocalDeclaration(StanVector(n))
      check(r.emit, "(v#) ./ (v#)")
    }

    it("can divide scalar by vector") {
      val n = StanLocalDeclaration(StanInt())
      val r = n /:/ StanLocalDeclaration(StanVector(n))
      check(r.emit, "(v#) ./ (v#)")
    }

    it("can divide matrix by scalar") {
      val n = StanLocalDeclaration(StanInt())
      val r = StanLocalDeclaration(StanMatrix(n, n)) /:/ n
      check(r.emit, "(v#) ./ (v#)")
    }

    it("can not divide scalar by scalar") {
      val r = StanLocalDeclaration(StanReal())
      "r /:/ r" shouldNot compile
    }
  }

  describe("^") {
    it("can pow ints") {
      val r = StanConstant[StanInt](1) ^ StanConstant[StanInt](2)
      r.emit shouldBe "(1) ^ (2)"
    }
    it("can pow real / int") {
      val r = StanConstant[StanReal](1.0) ^ StanConstant[StanInt](2)
      r.emit shouldBe "(1.0) ^ (2)"
    }
    it("can pow reals") {
      val r = StanConstant[StanReal](1.0) ^ StanConstant[StanReal](2.0)
      r.emit shouldBe "(1.0) ^ (2.0)"
    }
  }

  describe("unary -") {
    it("can negate ints") {
      val r = -StanConstant[StanInt](1)
      r.emit shouldBe "-(1)"
    }
  }

  describe("implicit conversion") {
    it("can convert int to real") {
      new ScalaStan {
        val model = new Model {
          local(real()) := 1
        }
        checkCode(model, "v# = 1;")
      }
    }

    it("can not convert real to int") {
      "new ScalaStan { new Model { local(int()) := 1.0 } }" shouldNot compile
    }
  }

  describe("index (with assignment)") {
    it("can be read 1d") {
      val i = StanConstant[StanInt](1)
      val d = StanLocalDeclaration(StanReal()(i)).apply(i)
      d.emit should fullyMatch regex "v[0-9]+\\[1\\]"
    }

    it("can be read vector") {
      val i = StanConstant[StanInt](1)
      val d = StanLocalDeclaration(StanVector(i)).apply(i)
      d.emit should fullyMatch regex "v[0-9]+\\[1\\]"
    }

    it("can be read 2d") {
      val i1 = StanConstant[StanInt](1)
      val i2 = StanConstant[StanInt](2)
      val d = StanLocalDeclaration(StanReal()(i1, i2)).apply(i1, i2)
      d.emit should fullyMatch regex "v[0-9]+\\[1,2\\]"
    }

    it("can be read matrix") {
      val i1 = StanConstant[StanInt](1)
      val i2 = StanConstant[StanInt](2)
      val d = StanLocalDeclaration(StanMatrix(i1, i2)).apply(i1, i2)
      d.emit should fullyMatch regex "v[0-9]+\\[1,2\\]"
    }

    it("can set vector") {
      new ScalaStan {
        val model = new Model {
          val a = local(vector(5))
          a(2) := 2
        }
        checkCode(model, "vector[5] a; a[2] = 2;")
      }
    }

    it("can set matrix") {
      new ScalaStan {
        val model = new Model {
          val a = local(matrix(5, 6))
          a(2, 3) := 4
        }
        checkCode(model, "matrix[5,6] a; a[2,3] = 4;")
      }
    }
  }

  describe("index (read-only)") {
    it("can be read vector") {
      val i = StanConstant[StanInt](1)
      val d = StanParameterDeclaration(StanVector(i)).apply(i)
      d.emit should fullyMatch regex "v[0-9]+\\[1\\]"
    }

    it("can not set vector") {
      """
      new ScalaStan {
        val n = data(int())
        val p = parameter(vector(n))
        new Model {
          p(1) := 2
        }
      }
      """ shouldNot compile
    }
  }

  describe("slice") {
    it("can be read") {
      val i1 = StanConstant[StanInt](1)
      val i2 = StanConstant[StanInt](2)
      val d = StanLocalDeclaration(StanVector(i1)).apply(StanValueRange(i1, i2))
      d.emit should fullyMatch regex "v[0-9]+\\[1:2\\]"
    }
  }

  describe("multiple indexes") {
    it("can have multiple indexes") {
      val n = StanDataDeclaration[StanInt](StanInt())
      val i = StanDataDeclaration[StanArray[StanInt]](StanArray(n, StanInt()))
      val vec = StanDataDeclaration[StanArray[StanReal]](StanArray(n, StanReal()))
      vec(i).emit should fullyMatch regex "v[0-9]+\\[v[0-9]+\\]"
    }
  }
}
