package com.cibo.scalastan.models

import com.cibo.scalastan._

case class Horseshoe(
  p0: Double = 3.0,                     // Prior on the number of important parameters.
  scaleInterceptPrior: Double = 10.0,   // Prior standard deviation for the ntercept.
  nuLocalPrior: Double = 1.0,           // Prior on degrees of freedom for the half-t priors for lambda.
  nuGlobalPrior: Double = 1.0,          // Prior on degrees of freedom for the half-t priors on tau.
  slabScalePrior: Double = 1.0,         // Slabe scale for the regularized horseshoe.
  slabDfPrior: Double = 1.0             // Slab degrees of freedom for the regularized horseshoe.
) extends ScalaStan {

  // Horsehoe prior from:
  // Juho Piironen and Aki Vehtari, "Sparsity Information and Regularization in the Horseshoe and Other
  // Shrinkage Priors", 2017.
  // This code is adapted from section C.2.

  private val n = data(int(lower = 0))    // Number of observations
  private val p = data(int(lower = 0))    // Number of parameters

  val x: StanDataDeclaration[StanMatrix] = data(matrix(n, p))   // Inputs
  val y: StanDataDeclaration[StanVector] = data(vector(n))      // Outputs

  // Priors
  private val scaleIntercept = data(real(lower = 0))
  private val nuLocal = data(real(lower = 1))
  private val nuGlobal = data(real(lower = 1))
  private val slabScale = data(real(lower = 0))
  private val slabDf = data(real(lower = 0))

  val beta0: StanParameterDeclaration[StanReal] = parameter(real())   // y-intercept

  private val sigma = parameter(real(lower = 0))            // Noise standard deviation
  private val z = parameter(vector(p))
  private val aux1Global = parameter(real(lower = 0))
  private val aux2Global = parameter(real(lower = 0))
  private val aux1Local = parameter(vector(p, lower = 0))
  private val aux2Local = parameter(vector(p, lower = 0))
  private val caux = parameter(real(lower = 0))

  private val tau0 = new DataTransform(real(lower = 0)) {
    result := fmin(p0, p - 1) / (p - fmin(p0, p - 1)) / pow(n, 0.5)
  }

  // Global shrinkage parameter.
  private val tau = new ParameterTransform(real(lower = 0)) {
    result := aux1Global * sqrt(aux2Global) * tau0 * sigma
  }

  // Slab scale.
  private val c = new ParameterTransform(real(lower = 0)) {
    result := slabScale * sqrt(caux)
  }

  // Local shrinkage parameter.
  private val lambda = new ParameterTransform(vector(p, lower = 0)) {
    result := aux1Local :* sqrt(aux2Local)
  }

  // "Truncated" local schrinkage parameter.
  private val lambdaTilde = new ParameterTransform(vector(p, lower = 0)) {
    result := sqrt((c ^ 2) * square(lambda) :/ ((c ^ 2) + (tau ^ 2) * square(lambda)))
  }

  // Regression coefficients.
  val beta: StanParameterDeclaration[StanVector] = new ParameterTransform(vector(p)) {
    result := z :* lambdaTilde * tau
  }

  // Latent function values.
  private val f = new ParameterTransform(vector(n)) {
    result := beta0 + x * beta
  }

  private val model = new Model {
    sigma ~ Cauchy(0, 1)
    z ~ Normal(0, 1)
    aux1Local ~ Normal(0, 1)
    aux2Local ~ InvGamma(0.5 * nuLocal, 0.5 * nuLocal)
    aux1Global ~ Normal(0, 1)
    aux2Global ~ InvGamma(0.5 * nuGlobal, 0.5 * nuGlobal)
    caux ~ InvGamma(0.5 * slabDf, 0.5 * slabDf)
    beta0 ~ Normal(0, scaleIntercept)
    y ~ Normal(f, sigma)
  }

  def compile: CompiledModel = model.compile
    .withData(scaleIntercept, scaleInterceptPrior)
    .withData(nuGlobal, nuGlobalPrior)
    .withData(nuLocal, nuLocalPrior)
    .withData(slabScale, slabScalePrior)
    .withData(slabDf, slabDfPrior)

  def predictions(data: Seq[Seq[Double]], results: StanResults): Seq[Double] = {
    val bestBeta = results.best(beta)
    val bestOffset = results.best(beta0)
    data.map { xs =>
      require(xs.length == bestBeta.length)
      xs.zip(bestBeta).map { case (v, b) => v * b }.sum + bestOffset
    }
  }
}
