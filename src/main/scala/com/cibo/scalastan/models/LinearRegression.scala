/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

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
    sigma ~ stan.Cauchy(0, 1)
    y ~ stan.Normal(x * beta + beta0, sigma)
  }

  def compile[M <: CompiledModel](implicit runner: StanRunner[M]): CompiledModel = model.compile
    .withData(x, xs)
    .withData(y, ys)

  def predict(data: Seq[Seq[Double]], results: StanResults): Seq[Double] = {
    val bestBeta0 = results.best(beta0)
    val bestBeta = results.best(beta)
    data.map { ds =>
      require(ds.length == bestBeta.length)
      bestBeta0 + ds.zip(bestBeta).map { case (d, b) => d * b }.sum
    }
  }
}
