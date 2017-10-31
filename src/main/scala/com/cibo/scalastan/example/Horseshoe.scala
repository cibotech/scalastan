package com.cibo.scalastan.example

import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.data.TextDataSource

object Horseshoe extends App with ScalaStan {

  val n = data(int(lower = 0))
  val p = data(int(lower = 0))
  val x = data(matrix(n, p))
  val y = data(vector(n))

  val beta = parameter(vector(p))
  val lambda = parameter(vector(p, lower = 0))
  val tau = parameter(real(lower = 0))
  val sigma = parameter(real(lower = 0))

  val model = new Model {
    lambda ~ Cauchy(0, 1)
    tau ~ Cauchy(0, 1)
    sigma ~ InvGamma(0.5, 0.5)
    beta ~ Normal(0, lambda * tau)
    y ~ Normal(x * beta, sigma)
  }

  val xSource = TextDataSource.fromFile("x.dat")
  val ySource = TextDataSource.fromFile("y.dat")
  val results = model
    .withData(xSource(x))
    .withData(ySource(y))
    .run(chains = 4)

  results.summary(System.out)

  println(s"sigma: ${results.best(sigma)}")
  results.best(beta).zipWithIndex.sortBy(v => math.abs(v._1)).foreach { case (b, i) =>
    println(s"$i: $b")
  }

}
