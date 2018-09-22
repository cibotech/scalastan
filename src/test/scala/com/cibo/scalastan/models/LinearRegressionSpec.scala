package com.cibo.scalastan.models

import com.cibo.scalastan.{ScalaStanBaseSpec, StanReal, StanVector}

class LinearRegressionSpec extends ScalaStanBaseSpec {
  describe("LinearRegression") {

    val xs = Seq(Seq(1.0), Seq(2.0), Seq(3.0))
    val ys = Seq(2.0, 3.0, 4.0)
    val lr = LinearRegression(xs, ys)

    it("generates the model") {
      val model = lr.compile
      checkCode(model.model,
        """
           model {
             y ~ normal(((x * beta) + beta0),sigma);
             sigma ~ cauchy(0,1);
           }
        """
      )
    }

    it("generates predictions") {
      val runner = MockRunner()
        .set[StanVector](lr.beta, Seq(Seq(2.0)))
        .set[StanReal](lr.beta0, Seq(3.0))
      val results = lr.compile.copy(runner = runner).run()
      lr.predict(Seq(Seq(4.0)), results) shouldBe Seq(3.0 + 2.0 * 4.0)
    }
  }
}
