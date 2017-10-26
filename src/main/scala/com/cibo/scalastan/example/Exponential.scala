package com.cibo.scalastan.example

import com.cibo.scalastan.ScalaStan

object Exponential extends App with ScalaStan {

  val len = data(int())
  val y = data(vector(len))
  val lambda = parameter(real(lower = 0))

  val model = new Model {
    val alpha = local(real())
    val beta = local(real())

    alpha := 1.0
    beta := 1.0

    lambda ~ Gamma(alpha, beta)
    y ~ Exponential(lambda)
  }

  val dataset = Vector(1.0, 2.0, 3.0)
  val results = model
    .withData(y, dataset)
    .run()
  println(s"lambda = ${results.best(lambda)}")

}
