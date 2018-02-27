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
      """
        new ScalaStan {
          val x = data(real())
          val model = new Model {
            x ~ stan.poisson(1.0)
          }
        }
      """ shouldNot compile
    }
  }

  describe("inv_wishart") {
    it("should work") {
      new ScalaStan {
        val nu = parameter(real())
        val sigma = parameter(matrix(5, 5))
        val y = data(matrix(5, 5))
        val model = new Model {
          y ~ stan.inv_wishart(nu, sigma)
        }
        checkCode(model, "model { y ~ inv_wishart(nu, sigma); }")
      }
    }
  }
}
