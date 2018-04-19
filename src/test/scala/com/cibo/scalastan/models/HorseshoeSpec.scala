package com.cibo.scalastan.models

import com.cibo.scalastan.{ScalaStanBaseSpec, StanReal, StanVector}

class HorseshoeSpec extends ScalaStanBaseSpec {
  describe("Horseshoe") {
    val xs = Seq(Seq(1.0, 2.0), Seq(2.0, 3.0))
    val ys = Seq(1.0, 2.0)
    val hs = Horseshoe(xs, ys)

    it("generates predictions") {
      val results = hs.compile.asInstanceOf[MockCompiledModel]
        .set[StanVector](hs.beta.result, Seq(Seq(0.5, 1.5)))
        .set[StanReal](hs.beta0, Seq(2.0))
        .run()
      hs.predict(Seq(Seq(3.0, 4.0)), results) shouldBe Seq(2.0 + 0.5 * 3.0 + 1.5 * 4.0)
    }
  }
}
