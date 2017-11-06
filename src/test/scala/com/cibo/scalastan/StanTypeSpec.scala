package com.cibo.scalastan

class StanTypeSpec extends ScalaStanBaseSpec {

  private implicit val ss = new ScalaStan {}

  describe("ints") {
    it("emits the right data") {
      val i = StanInt()
      i.emitData(5) shouldBe "5"
    }

    it("emits the right declaration") {
      StanInt().emitDeclaration("x") shouldBe "int x"
    }

    it("emits the right declaration with lower bounds") {
      StanInt(lower = 2).emitDeclaration("x") shouldBe "int<lower=2> x"
    }

    it("emits the right declaration with upper bounds") {
      StanInt(upper = 5).emitDeclaration("x") shouldBe "int<upper=5> x"
    }

    it("emits the right declaration with lower and upper bounds") {
      StanInt(lower = 1, upper = 2).emitDeclaration("x") shouldBe "int<lower=1,upper=2> x"
    }
  }

  describe("categoricals") {
    it("emits the right data") {
      val c = StanCategorical()
      c.emitData("one") shouldBe "0"
      c.emitData("two") shouldBe "1"
      c.emitData("one") shouldBe "0"
    }

    it("emits the right declaration") {
      StanCategorical().emitDeclaration("x") shouldBe "int x"
    }
  }

  describe("reals") {
    it("emits the right data") {
      val r = StanReal()
      r.emitData(2.5) shouldBe "2.5"
    }

    it("emits the right declaration") {
      StanReal().emitDeclaration("y") shouldBe "real y"
    }

    it("emits the right declaration with bounds") {
      StanReal(lower = 1.5, upper = 2).emitDeclaration("y") shouldBe "real<lower=1.5,upper=2.0> y"
    }
  }

  describe("strings") {
    it("emits the right data") {
      val s = StanString()
      s.emitData("test") shouldBe "\"test\""
    }
  }

  describe("vectors") {
    it("emits the right data") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanVector(n)
      v.emitData(Vector(1.0, 2.0, 3.0)) shouldBe "c(1.0,2.0,3.0)"
    }

    it("emits vectors with zero size") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanVector(n)
      v.emitData(Vector.empty) shouldBe "c()"
    }

    it("emits the right declaration") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanVector(n, lower = 1.5, upper = 2.5)
      v.emitDeclaration("vec") should fullyMatch regex "vector<lower=1.5,upper=2.5>\\[.+\\] vec"
    }
  }

  describe("row vectors") {
    it("emits the right data") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanRowVector(n)
      v.emitData(Vector(1.0, 2.0, 3.0)) shouldBe "c(1.0,2.0,3.0)"
    }

    it("emits row vectors with zero size") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanRowVector(n)
      v.emitData(Vector.empty) shouldBe "c()"
    }

    it("emits the right declaration") {
      val n = StanParameterDeclaration(StanInt())
      val v = StanRowVector(n)
      v.emitDeclaration("asdf") should fullyMatch regex "row_vector\\[.+\\] asdf"
    }
  }

  describe("matrices") {
    it("emits the right thing") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val m = StanMatrix(rows, cols)
      val data = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )

      m.emitData(data) shouldBe "structure(c(1.0,4.0,2.0,5.0,3.0,6.0), .Dim = c(2,3))"
    }

    it("emits matrices with zero size") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val m = StanMatrix(rows, cols)

      m.emitData(Vector.empty) shouldBe "structure(c(), .Dim = c(0,0))"
    }

    it("emits the right declaration") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val m = StanMatrix(rows, cols, lower = 1.5)

      m.emitDeclaration("m") should fullyMatch regex "matrix<lower=1.5>\\[.+,.+\\] m"
    }
  }

  describe("arrays") {
    it("should emit the same thing as a matrix") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanArray(rows, StanReal()))
      val m = StanMatrix(rows, cols)

      val data = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )

      a.emitData(data) shouldBe m.emitData(data)
    }

    it("emits arrays with zero size in 1 dimension") {
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanReal())

      a.emitData(Vector.empty) shouldBe "c()"
    }

    it("emits arrays with zero size in 2 dimensions") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanArray(rows, StanReal()))

      a.emitData(Vector.empty) shouldBe "structure(c(), .Dim = c(0,0))"
    }

    it("emits the right declaration for 1 dimension") {
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanReal())

      a.emitDeclaration("a") should fullyMatch regex "real a\\[.+\\]"
    }

    it("emits the right declaration for 2 dimensions") {
      val rows = StanParameterDeclaration(StanInt())
      val cols = StanParameterDeclaration(StanInt())
      val a = StanArray(cols, StanArray(rows, StanInt()))

      a.emitDeclaration("a") should fullyMatch regex "int a\\[.+,.+\\]"
    }
  }
}
