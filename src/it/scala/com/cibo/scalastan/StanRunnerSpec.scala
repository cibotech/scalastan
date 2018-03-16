package com.cibo.scalastan

import com.cibo.scalastan.models.LinearRegression
import org.scalatest.FunSpec

class StanRunnerSpec extends FunSpec {
  describe("StanRunner") {
    it("captures standard output in results") {
      println("Start StanRunner test")
      // Taken from linear regression example
      val xs = Seq[Seq[Double]](Seq(1), Seq(2), Seq(3), Seq(4), Seq(6))
      val ys = Seq[Double](2.9, 6.1, 7.0, 9.2, 13.1)

      val model: LinearRegression = LinearRegression(xs, ys)
      val results: StanResults = model.compile.run(chains = 4, seed = -1, cache = false)
      println("Stan output:")
      println(results.stanOutput)
      println("Stan error output:")
      println(results.stanErrorOutput)
    }
  }
}
