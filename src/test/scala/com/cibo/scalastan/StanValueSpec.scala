package com.cibo.scalastan

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

  describe("index") {
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
  }

  describe("slice") {
    it("can be read") {
      val i1 = StanConstant[StanInt](1)
      val i2 = StanConstant[StanInt](2)
      val d = StanLocalDeclaration(StanVector(i1)).apply(ValueRange(i1, i2))
      d.emit should fullyMatch regex "v[0-9]+\\[1:2\\]"
    }
  }
}
