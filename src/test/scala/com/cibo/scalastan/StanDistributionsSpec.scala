package com.cibo.scalastan

class StanDistributionsSpec extends ScalaStanBaseSpec {
  describe("poisson") {
    it("should work with integers") {
      new ScalaStan {
        val x = data(int())
        val model = new Model {
          x ~ stan.poisson(1.0)
        }
        checkCode(model, "model { x ~ poisson(1.0); }")
      }
    }

    it("should not work with reals") {
      //FIXME: This should not compile/run.
      new ScalaStan {
        val x = data(real())
        val model = new Model {
          x ~ stan.poisson(1.0)
        }
        checkCode(model, "model { x ~ poisson(1.0); }")
      }
    }
  }
}
