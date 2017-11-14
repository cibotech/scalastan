package com.cibo.scalastan.models

import com.cibo.scalastan._

case class LinearRegression(
  xs: Seq[Seq[Double]],   // Inputs
  ys: Seq[Double]         // Outputs
) extends ScalaStan {

  require(xs.length == ys.length, "Length of inputs must match the length of the outputs")

  private val n = data(int(lower = 0))  // Number of observations
  private val p = data(int(lower = 0))  // Number of parameters
  private val x: StanDataDeclaration[StanMatrix] = data(matrix(n, p))   // Inputs
  private val y: StanDataDeclaration[StanVector] = data(vector(n))      // Outputs

  val beta0: StanParameterDeclaration[StanReal] = parameter(real())             // Offset
  val beta: StanParameterDeclaration[StanVector] = parameter(vector(p))         // Coefficients
  val sigma: StanParameterDeclaration[StanReal] = parameter(real(lower = 0))    // Error

  private val model = new Model {
    sigma ~ InvGamma(0.01, 0.01)
    y ~ Normal(x * beta + beta0, sigma)
  }

  def compile: CompiledModel = model.compile
    .withData(x, xs)
    .withData(y, ys)

  def predict(data: Seq[Double], results: StanResults): Double = {
    val bestBeta0 = results.best(beta0)
    val bestBeta = results.best(beta)
    require(data.length == bestBeta.length)
    bestBeta0 + data.zip(bestBeta).map { case (d, b) => d * b }.sum
  }
}
