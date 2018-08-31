/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.models

import com.cibo.scalastan._
import com.cibo.scalastan.run.{StanCompiler, StanRunner}

case class Horseshoe(
  xs: Seq[Seq[Double]],                 // Inputs
  ys: Seq[Double],                      // Outputs
  p0: Double = 3.0,                     // Prior on the number of important parameters.
  scaleInterceptPrior: Double = 10.0,   // Prior standard deviation for the intercept.
  nuLocalPrior: Double = 1.0,           // Prior on degrees of freedom for the half-t priors for lambda.
  nuGlobalPrior: Double = 1.0,          // Prior on degrees of freedom for the half-t priors on tau.
  slabScalePrior: Double = 1.0,         // Slabe scale for the regularized horseshoe.
  slabDfPrior: Double = 1.0             // Slab degrees of freedom for the regularized horseshoe.
) extends ScalaStan {

  // Horsehoe prior from:
  // Juho Piironen and Aki Vehtari, "Sparsity Information and Regularization in the Horseshoe and Other
  // Shrinkage Priors", 2017.
  // This code is adapted from section C.2.

  val n: DataDeclaration[StanInt] = data(int(lower = 0))    // Number of observations
  val p: DataDeclaration[StanInt] = data(int(lower = 0))    // Number of parameters

  val x: DataDeclaration[StanMatrix] = data(matrix(n, p))   // Inputs
  val y: DataDeclaration[StanVector] = data(vector(n))      // Outputs

  // Priors
  val scaleIntercept: DataDeclaration[StanReal] = data(real(lower = 0.0))
  val nuLocal: DataDeclaration[StanReal] = data(real(lower = 1.0))
  val nuGlobal: DataDeclaration[StanReal] = data(real(lower = 1.0))
  val slabScale: DataDeclaration[StanReal] = data(real(lower = 0.0))
  val slabDf: DataDeclaration[StanReal] = data(real(lower = 0.0))

  val beta0: ParameterDeclaration[StanReal] = parameter(real())   // y-intercept
  val sigma: ParameterDeclaration[StanReal] = parameter(real(lower = 0.0)) // Noise standard deviation
  
  val z: ParameterDeclaration[StanVector] = parameter(vector(p))
  private val aux1Global = parameter(real(lower = 0.0))
  private val aux2Global = parameter(real(lower = 0.0))
  private val aux1Local = parameter(vector(p, lower = 0.0))
  private val aux2Local = parameter(vector(p, lower = 0.0))
  private val caux = parameter(real(lower = 0.0))

  private val tau0 = new TransformedData(real(lower = 0.0)) {
    result := stan.fmin(p0, p - 1) / (p - stan.fmin(p0, p - 1)) / stan.pow(n, 0.5)
  }

  // Global shrinkage parameter.
  val tau: ParameterDeclaration[StanReal] = new TransformedParameter(real(lower = 0)) {
    result := aux1Global * stan.sqrt(aux2Global) * tau0 * sigma
  }

  // Slab scale.
  val c: ParameterDeclaration[StanReal] = new TransformedParameter(real(lower = 0)) {
    result := slabScale * stan.sqrt(caux)
  }

  // Local shrinkage parameter.
  val lambda: ParameterDeclaration[StanVector] = new TransformedParameter(vector(p, lower = 0)) {
    result := aux1Local *:* stan.sqrt(aux2Local)
  }

  // "Truncated" local shrinkage parameter.
  val lambdaTilde: ParameterDeclaration[StanVector] = new TransformedParameter(vector(p, lower = 0)) {
    result := stan.sqrt((c ^ 2) * stan.square(lambda) /:/ ((c ^ 2) + (tau ^ 2) * stan.square(lambda)))
  }

  // Regression coefficients.
  val beta: ParameterDeclaration[StanVector] = new TransformedParameter(vector(p)) {
    result := z *:* lambdaTilde * tau
  }

  // Latent function values.
  val f: ParameterDeclaration[StanVector] = new TransformedParameter(vector(n)) {
    result := beta0 + x * beta
  }

  val model: Model = new Model {
    sigma ~ stan.cauchy(0, 1)
    z ~ stan.normal(0, 1)
    aux1Local ~ stan.normal(0, 1)
    aux2Local ~ stan.inv_gamma(0.5 * nuLocal, 0.5 * nuLocal)
    aux1Global ~ stan.normal(0, 1)
    aux2Global ~ stan.inv_gamma(0.5 * nuGlobal, 0.5 * nuGlobal)
    caux ~ stan.inv_gamma(0.5 * slabDf, 0.5 * slabDf)
    beta0 ~ stan.normal(0, scaleIntercept)
    y ~ stan.normal(f, sigma)
  }

  def compile(implicit compiler: StanCompiler): CompiledModel = model.compile
    .withData(scaleIntercept, scaleInterceptPrior)
    .withData(nuGlobal, nuGlobalPrior)
    .withData(nuLocal, nuLocalPrior)
    .withData(slabScale, slabScalePrior)
    .withData(slabDf, slabDfPrior)
    .withData(x, xs)
    .withData(y, ys)

  def predict(data: Seq[Seq[Double]], results: StanResults): Seq[Double] = {
    val bestBeta = results.best(beta)
    val bestOffset = results.best(beta0)
    data.map { vs =>
      require(vs.length == bestBeta.length)
      vs.zip(bestBeta).map { case (v, b) => v * b }.sum + bestOffset
    }
  }
}
