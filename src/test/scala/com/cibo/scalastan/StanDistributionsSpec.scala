package com.cibo.scalastan

class StanDistributionsSpec extends ScalaStanBaseSpec {
  describe("poisson") {
    it("should work with integers") {
      val model = new StanModel {
        val x = data(int())
        x ~ stan.poisson(1.0)
      }
      checkCode(model, "model { x ~ poisson(1.0); }")
    }

    it("should not work with reals") {
      """
        val x = data(real())
        val model = new Model {
          x ~ stan.poisson(1.0)
        }
      """ shouldNot compile
    }
  }

  describe("inv_wishart") {
    it("should work") {
      val model = new StanModel {
        val nu = parameter(real())
        val sigma = parameter(matrix(5, 5))
        val y = data(matrix(5, 5))
        y ~ stan.inv_wishart(nu, sigma)
      }
      checkCode(model, "model { y ~ inv_wishart(nu, sigma); }")
    }
  }

  describe("lkj_corr") {
    it("should work") {
      val model = new StanModel {
        val y = data(matrix(5, 5))
        y ~ stan.lkj_corr(2)
      }
      checkCode(model, "model { y ~ lkj_corr(2); }")
    }
  }

  describe("lkj_cholesky") {
    it("should work") {
      val model = new StanModel {
        val y = data(matrix(5, 5))
        y ~ stan.lkj_cholesky(2)
      }
      checkCode(model, "model { y ~ lkj_cholesky(2); }")
    }
  }

  describe("lkj_corr_cholesky") {
    it("should work") {
      val model = new StanModel {
        val y = data(matrix(5, 5))
        y ~ stan.lkj_corr_cholesky(2)
      }
      checkCode(model, "model { y ~ lkj_corr_cholesky(2); }")
    }
  }

  describe("multi_normal") {
    it("should vectorize") {
      val model = new StanModel {
        val k = data(int())
        val j = data(int())
        val n = data(int())
        val x = data(vector(n)(j))
        val y = data(vector(n)(k))
        val beta = parameter(matrix(k, j))
        val sigma = parameter(covMatrix(k))
        val mu = local(vector(k)(n))
        for (n <- range(1, n)) {
          mu(n) := beta * x(n)
          beta * x(n)
        }
        y ~ stan.multi_normal(mu, sigma)
      }
      checkCode(model, "y ~ multi_normal(mu, sigma);")
    }

    it("should vectorize matrices") {
      val model = new StanModel {
        val k = data(int())
        val j = data(int())
        val n = data(int())
        val y = data(matrix(n, k))
        val mu = parameter(vector(k))
        val beta = parameter(matrix(k, j))
        val sigma = parameter(covMatrix(k))
        y(1) ~ stan.multi_normal(mu, sigma)
      }
      checkCode(model, "y[1] ~ multi_normal(mu, sigma);")
    }
  }
}
