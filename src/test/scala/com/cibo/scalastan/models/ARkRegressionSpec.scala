package com.cibo.scalastan.models

import com.cibo.scalastan._

class ARkRegressionSpec extends ScalaStanBaseSpec {
  describe("ARkRegression") {
    val xs = Seq(Seq(1.0, 2.0), Seq(3.0, 4.0))
    val ys = Seq(1.0, 2.0)
    val cs = Seq(1, 2)
    val ar1 = ARkRegression(xs, ys, cs)

    it("generates predictions") {
      val runner = MockRunner()
        .set[StanVector](ar1.xbeta, Seq(Seq(1.0, 1.5)))
        .set[StanMatrix](ar1.beta, Seq(Seq(Seq(0.5), Seq(1.5))))
        .set[StanReal](ar1.alpha, Seq(3.0))
      val results = ar1.compile.copy(runner = runner).run()

      val expected1 = 3.0 + 1.5 * 1.0 + 2.5 * 1.5
      val expected2 = 3.0 + 3.5 * 1.0 + 4.5 * 1.5 + 1.5 * expected1
      ar1.predict(Seq(Seq(1.5, 2.5), Seq(3.5, 4.5)), 2, results) shouldBe Seq(expected1, expected2)
    }
  }
}
