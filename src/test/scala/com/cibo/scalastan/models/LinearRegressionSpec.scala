package com.cibo.scalastan.models

import com.cibo.scalastan.{ScalaStanBaseSpec, StanReal, StanResults, StanVector}

class LinearRegressionSpec extends ScalaStanBaseSpec {
  describe("LinearRegression") {

    val xs = Seq(Seq(1.0), Seq(2.0), Seq(3.0))
    val ys = Seq(2.0, 3.0, 4.0)
    val lr = LinearRegression(xs, ys)

    it("generates the model") {
      val model = lr.compile
      model.isInstanceOf[MockCompiledModel] shouldBe true
      check(model.asInstanceOf[MockCompiledModel].code,
        """
           model {
             v#_sigma ~ inv_gamma(0.01,0.01);
             v#_y ~ normal(((v#_x) * (v#_beta)) + (v#_beta0),v#_sigma);
           }
        """
      )
    }

    it("generates predictions") {
      val results = lr.compile.asInstanceOf[MockCompiledModel]
        .set[StanVector](lr.beta, Seq(Seq(2.0)))
        .set[StanReal](lr.beta0, Seq(3.0))
        .run()
      lr.predict(Seq(Seq(4.0)), results) shouldBe Seq(3.0 + 2.0 * 4.0)
    }
  }
}
