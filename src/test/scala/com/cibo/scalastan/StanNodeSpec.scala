package com.cibo.scalastan

class StanNodeSpec extends ScalaStanBaseSpec with ScalaStan with StanDistributions {

  private val v1 = StanLocalDeclaration[StanReal](StanReal())
  private val v2 = StanLocalDeclaration[StanReal](StanReal())
  private val v3 = StanLocalDeclaration[StanInt](StanInt())
  private val y = StanLocalDeclaration[StanReal](StanReal())
  private val continuous: StanContinuousDistribution[StanReal] = Normal(v1, v2)
  private val discreteCdf: StanDiscreteDistribution[StanInt] = Binomial(v3, v2)

  describe("StanContinuousDistribution") {
    it("generates sample syntax") {
      val model = new Model {
        y ~ continuous
      }
      checkCode(model, "model { y ~ normal(v1,v2); }")
    }

    it("generates sample syntax with lower bound") {
      val model = new Model {
        y ~ continuous.truncate(v2)
      }
      checkCode(model, "model { y ~ normal(v1,v2) T[v2,]; }")
    }

    it("generates sample syntax with upper bound") {
      val model = new Model {
        y ~ continuous.truncate(upper = v2)
      }
      checkCode(model, "model { y ~ normal(v1,v2) T[,v2]; }")
    }

    it("generates sample syntax with upper and lower bounds") {
      val model = new Model {
        y ~ continuous.truncate(v1, v2)
      }
      checkCode(model, "model { y ~ normal(v1,v2) T[v1,v2]; }")
    }

    it("generates lpdf syntax") {
      val model = new Model {
        target += continuous.lpdf(y)
      }
      checkCode(model, "model { target += normal_lpdf(y | v1,v2); }")
    }

    it("generates cdf syntax") {
      val model = new Model {
        v2 := continuous.cdf(y)
      }
      checkCode(model, "model { v2 = normal_cdf(y , v1,v2); }")
    }

    it("should allow rng in a generated quantity") {
      val gen = new GeneratedQuantity(real()) { continuous.rng }
    }

    it("should not allow rng in a model") {
      "val model = new Model { continuous.rng }" shouldNot compile
    }
  }
}
