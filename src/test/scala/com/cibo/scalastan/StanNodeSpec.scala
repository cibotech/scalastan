package com.cibo.scalastan

class StanNodeSpec extends ScalaStanBaseSpec with ScalaStan with StanDistributions {

  private val v1 = StanDeclaration[StanReal, LocalDeclarationType](StanReal())
  private val v2 = StanDeclaration[StanReal, LocalDeclarationType](StanReal())
  private val v3 = StanDeclaration[StanInt, LocalDeclarationType](StanInt())
  private val y = StanDeclaration[StanReal, LocalDeclarationType](StanReal())
  private val continuous: StanContinuousDistribution[StanReal] = Normal(v1, v2)
  private val discreteCdf: StanDiscreteDistribution[StanReal] = Binomial(v3, v2)

  describe("StanContinuousDistribution") {
    it("generates sample syntax") {
      val model = new Model {
        y ~ continuous
      }
      checkCode(model, "model { v# ~ normal(v#,v#) }")
    }

    it("generates lpdf syntax") {
      val model = new Model {
        target += continuous.lpdf(y)
      }
      checkCode(model, "model { target += normal_lpdf(v# | v#,v#) }")
    }

    it("generates cdf syntax") {
      val model = new Model {
        v2 := continuous.cdf(y)
      }
      checkCode(model, "model { v# = normal_cdf(v# , v#,v#) }")
    }
  }
}
