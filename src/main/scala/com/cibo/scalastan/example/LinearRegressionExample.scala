package com.cibo.scalastan.example

import com.cibo.scalastan.models.LinearRegression

object LinearRegressionExample extends App {

  val xs = Seq[Double](1, 2, 3, 4, 6)
  val ys = Seq[Double](2.9, 6.1, 7.0, 9.2, 13.1)

  val model = LinearRegression(xs, ys)
  println(s"m:     ${model.mhat}")
  println(s"b:     ${model.bhat}")
  println(s"sigma: ${model.sigma}")

}
