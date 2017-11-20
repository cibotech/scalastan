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
      checkCode(model, "model { v#_y ~ normal(v#_v1,v#_v2); }")
    }

    it("generates lpdf syntax") {
      val model = new Model {
        target += continuous.lpdf(y)
      }
      checkCode(model, "model { target += normal_lpdf(v#_y | v#_v1,v#_v2); }")
    }

    it("generates cdf syntax") {
      val model = new Model {
        v2 := continuous.cdf(y)
      }
      checkCode(model, "model { v#_v2 = normal_cdf(v#_y , v#_v1,v#_v2); }")
    }

    it("should allow rng in a generated quantity") {
      val gen = new GeneratedQuantity(real()) { continuous.rng }
    }

    it("should not allow rng in a model") {
      "val model = new Model { continuous.rng }" shouldNot compile
    }
  }
}
