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
}
