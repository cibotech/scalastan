package com.cibo.scalastan.models

import com.cibo.scalastan.ScalaStan

case class LinearRegression(xs: Seq[Double], ys: Seq[Double]) extends ScalaStan {

  require(xs.length == ys.length, s"length of Xs (${xs.length}) does not equal the length of Ys (${ys.length})")

  private val n = data(int(lower = 0))
  private val x = data(vector(n))
  private val y = data(vector(n))

  private val b = parameter(real())
  private val m = parameter(real())
  private val sig = parameter(real(lower = 0))

  private val model = new Model {
    sig ~ Gamma(100.0, 100.0)
    y ~ Normal(m * x + b, sig)
  }

  private lazy val results = model
    .withData(x, xs.toVector)
    .withData(y, ys.toVector)
    .run()

  def bhat: Double = results.mean(b)
  def mhat: Double = results.mean(m)
  def sigma: Double = results.mean(sig)
}
