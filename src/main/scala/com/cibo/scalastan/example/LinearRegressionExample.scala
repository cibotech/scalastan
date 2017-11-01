package com.cibo.scalastan.example

import com.cibo.scalastan.models.LinearRegression

object LinearRegressionExample extends App {

  val xs = Seq[Double](1, 2, 3, 4, 6)
  val ys = Seq[Double](2.9, 6.1, 7.0, 9.2, 13.1)

  val model = LinearRegression()
  val results = model.compile
    .withData(model.x, xs)
    .withData(model.y, ys)
    .run(chains = 4)

  println(s"m:     ${results.best(model.m)}")
  println(s"b:     ${results.best(model.b)}")
  println(s"sigma: ${results.best(model.sigma)}")

}
