package com.cibo.scalastan.models

import com.cibo.scalastan._

case class Horseshoe(
  p0: Double = 3.0  // Expected number of relevant parameters.
) extends ScalaStan {

  private val n = data(int(lower = 0))    // Number of observations
  private val p = data(int(lower = 0))    // Number of parameters

  // Inputs
  val x: StanDataDeclaration[StanMatrix] = data(matrix(n, p))
  val y: StanDataDeclaration[StanVector] = data(vector(n))

  // Output
  val beta: StanParameterDeclaration[StanVector] = parameter(vector(p))

  private val cSquared = parameter(real(lower = 0))
  private val nu = parameter(real(lower = 1))
  private val lambda = parameter(vector(p, lower = 0))
  private val tau = parameter(real(lower = 0))
  private val sigma = parameter(real(lower = 0))

  // Prior for Tau from:
  // Juho Piironen and Aki Vehtari, "On the Hyperprior Choice for the Global Shrinkage Parameter in the Horseshoe
  // Prior", in Proc of 20th Int'l Conf on Artificial Intelligence and Statistics, Vol 54, 2017.
  private val tau0 = new DataTransform(real(lower = 0)) {
    result := fmin(p0, p - 1.0) / ((p - fmin(p0, p - 1.0)) * (n ^ 0.5))
  }

  // Regularization from:
  // Juho Piironen and Aki Vehtari, "Sparsity Information and Regularization in the Horseshoe and Other
  // Shrinkage Priors", 2017.
  private val lambdaTilde = new ParameterTransform(vector(p)) {
    for (i <- lambda.range) {
      result(i) := sqrt(cSquared * pow(lambda(i), 2.0)) / (cSquared + pow(tau * lambda(i), 2.0))
    }
  }

  // Horseshoe Prior from:
  // Carlos M. Carvalho, Nicholas G. Polson, and James G. Scott, "Handling Sparsity via the Horseshoe",
  // in Proc. of 12th Int'l Conf on Artificial Intelligence and Statistics, Vol 5, 2009.
  private val model = new Model {
    nu ~ Gamma(2, 0.1)
    cSquared ~ InvGamma(nu / 2.0, nu * 1.0 / 2.0)
    sigma ~ Cauchy(0, 1)
    tau ~ Cauchy(0, pow(tau0 * sigma, 2.0))
    lambda ~ Cauchy(0, 1)
    beta ~ Normal(0, tau * lambdaTilde)
    y ~ Normal(x * beta, sigma)
  }

  def compile: CompiledModel = model.compile

  def predictions(data: Seq[Seq[Double]], results: StanResults): Seq[Double] = {
    val bestBeta = results.best(beta)
    data.map { xs =>
      require(xs.length == bestBeta.length)
      xs.zip(bestBeta).map { case (v, b) => v * b }.sum
    }
  }
}
