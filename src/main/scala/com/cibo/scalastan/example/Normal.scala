package com.cibo.scalastan.example

import com.cibo.scalastan.ScalaStan

object Normal extends App with ScalaStan {

  val N = data(int(lower = 0))
  val y = data(vector(N))

  val sigma = parameter(real(lower = 0.0))
  val mu = parameter(real())

  val model = new Model {
    y ~ Normal(mu, sigma)
  }

  val dataset = Vector(1.0, -2.0, 3.0, 0.0, 0.5, -1.0)
  val results = model
    .withData(y, dataset)
    .run()
  println(s"mu = ${results.mean(mu)}")
  println(s"sigma = ${results.mean(sigma)}, ${results.sd(sigma)}, ${results.quantile(sigma, 0.5)}")

}
