package com.cibo.scalastan.models

import com.cibo.scalastan._

case class LinearRegression() extends ScalaStan {
  private val n = data(int(lower = 0))

  val x: StanDataDeclaration[StanVector] = data(vector(n))
  val y: StanDataDeclaration[StanVector] = data(vector(n))

  val b: StanParameterDeclaration[StanReal] = parameter(real())
  val m: StanParameterDeclaration[StanReal] = parameter(real())
  val sigma: StanParameterDeclaration[StanReal] = parameter(real(lower = 0))

  private val model = new Model {
    sigma ~ InvGamma(0.01, 0.01)
    y ~ Normal(m * x + b, sigma)
  }

  def compile: CompiledModel = model.compile
}
