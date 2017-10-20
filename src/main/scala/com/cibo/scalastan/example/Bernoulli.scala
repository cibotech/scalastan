package com.cibo.scalastan.example

import com.cibo.scalastan.ScalaStan

object Bernoulli extends App with ScalaStan {

  val N = data(int(lower = 0))
  val y = data(array(N, int(lower = 0, upper = 1)))
  val theta = parameter(real(lower = 0.0, upper = 1.0))

  val model = new Model {
    theta ~ Beta(1.0, 1.0)
    for(n <- range(1, N)) {
      target += Bernoulli(theta).lpmf(y(n))
    }
  }

  val dataset = Vector(0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
  val results = model
    .withData(N, dataset.length)
    .withData(y, dataset)
    .run()
  println(s"theta = ${results.best(theta)}")
}
