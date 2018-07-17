package com.cibo.scalastan.models

import com.cibo.scalastan.{CompiledModel, ScalaStan, StanArray, StanResults}

/**
  * Finnish horseshoe prior linear model.
  *
  * Converted from an implementation by Michael Betancourt
  * https://betanalpha.github.io/assets/case_studies/bayes_sparse_regression.html#36_the_finnish_horseshoe
  *
  * "As with any Finn, the Finnish horseshoe can be a bit stubborn and we have to push
  * the limits of Hamiltonian Monte Carlo a bit to resolve all of the structure it
  * induces in the posterior distribution."
  *
  * To analyze a model like this, consider the posterior distributions of the beta, alpha, and sigma parameters, the
  * covariate weights, intercept, and error spread parameters respectively.
  *
  * @param expectedModelSize Expected number of large slopes
  * @param selectedCovariateScale Scale for large slopes
  * @param selectedCovariateDF Effective degrees of freedom for large slopes
  */
final case class FinnishHorseshoe(
    expectedModelSize: Int = 10,
    selectedCovariateScale: Double = 3,
    selectedCovariateDF: Int = 25
)(outputs: Vector[Double], inputs: Vector[Vector[Double]])
    extends ScalaStan {

  /*

    data {
      int<lower=1> N; // Number of data
      int<lower=1> M; // Number of covariates
      matrix[M, N] X;
      real y[N];
    }

    NOTE: We work with X' called Xt instead of X directly.
          This small divergence helps with data loading.

   */

  private val N = data(int(lower = 1)) // number of data
  private val M = data(int(lower = 1)) // number of covariates

  private val Xt = data(matrix(N, M))
  private val y = data(StanArray(N, real()))

  /*

    transformed data {
      real m0 = 10;           // Expected number of large slopes
      real slab_scale = 3;    // Scale for large slopes
      real slab_scale2 = square(slab_scale);
      real slab_df = 25;      // Effective degrees of freedom for large slopes
      real half_slab_df = 0.5 * slab_df;
    }

   */

  private val m0 = data(real()) // expectedModelSize.toDouble
  private val slab_scale = data(real()) // selectedCovariateScale
  private val slab_scale2 = new TransformedData(real()) { result := slab_scale ^ 2 }
  private val slab_df = data(real()) // selectedCovariateDF.toDouble
  private val half_slab_df = new TransformedData(real()) { result := 0.5 * slab_df }

  /*

    parameters {
      vector[M] beta_tilde;
      vector<lower=0>[M] lambda;
      real<lower=0> c2_tilde;
      real<lower=0> tau_tilde;
      real alpha;
      real<lower=0> sigma;
    }

   */

  private val beta_tilde = parameter(vector(M))
  private val lambda = parameter(vector(M, lower = 0))
  private val c2_tilde = parameter(real(lower = 0))
  private val tau_tilde = parameter(real(lower = 0))
  val alpha = parameter(real())
  val sigma = parameter(real(lower = 0))

  /*

    transformed parameters {
      vector[M] beta;
      {
        real tau0 = (m0 / (M - m0)) * (sigma / sqrt(1.0 * N));
        real tau = tau0 * tau_tilde; // tau ~ cauchy(0, tau0)

        // c2 ~ inv_gamma(half_slab_df, half_slab_df * slab_scale2)
        // Implies that marginally beta ~ student_t(slab_df, 0, slab_scale)
        real c2 = slab_scale2 * c2_tilde;

        vector[M] lambda_tilde =
          sqrt( c2 * square(lambda) ./ (c2 + square(tau) * square(lambda)) );

        // beta ~ normal(0, tau * lambda_tilde)
        beta = tau * lambda_tilde .* beta_tilde;
      }
    }

   */

  val beta = new TransformedParameter(vector(M)) {
    val tau0 = local(real())
    val tau = local(real())
    val c2 = local(real())
    val lambda_tilde = local(vector(M))

    tau0 := {
      val t1 = m0 / (M - m0)
      val t2 = sigma / stan.sqrt(N * 1.0)
      t1 * t2
    }
    tau := tau0 * tau_tilde // tau ~ cauchy(0, tau0)

    // c2 ~ inv_gamma(half_slab_df, half_slab_df * slab_scale2)
    // Implies that marginally beta ~ student_t(slab_df, 0, slab_scale)
    c2 := slab_scale2 * c2_tilde

    lambda_tilde := {
      val numer = c2 * stan.square(lambda)
      val denom = c2 + stan.square(tau) * stan.square(lambda)
      stan.sqrt(numer /:/ denom)
    }

    // beta ~ normal(0, tau * lambda_tilde)
    result := tau * (lambda_tilde *:* beta_tilde)
  }

  /*

    model {
      beta_tilde ~ normal(0, 1);
      lambda ~ cauchy(0, 1);
      tau_tilde ~ cauchy(0, 1);
      c2_tilde ~ inv_gamma(half_slab_df, half_slab_df);

      alpha ~ normal(0, 2);
      sigma ~ normal(0, 2);

      y ~ normal(X' * beta + alpha, sigma);
    }

   */

  val model = new Model {

    beta_tilde ~ stan.normal(0, 1)
    lambda ~ stan.cauchy(0, 1)
    tau_tilde ~ stan.cauchy(0, 1)
    c2_tilde ~ stan.inv_gamma(half_slab_df, half_slab_df)

    alpha ~ stan.normal(0, 2)
    sigma ~ stan.normal(0, 2)

    y ~ stan.normal(Xt * beta + alpha, sigma)

  }

  private def mean(vs: Vector[Double]): Double = vs.sum / vs.length.toDouble
  private def sd(vs: Vector[Double]): Double = {
    val x0 = mean(vs)
    val devs = vs.map(v => Math.pow(v - x0, 2))
    Math.sqrt(devs.sum / (devs.length - 1))
  }

  def train: CompiledModel =
    model
      .withData(m0, expectedModelSize.toDouble)
      .withData(slab_scale, selectedCovariateScale)
      .withData(slab_df, selectedCovariateDF.toDouble)
      .withData(y, outputs)
      .withData(Xt, inputs)

  def predictMAP(results: StanResults)(ins: Vector[Vector[Double]] = inputs): Vector[Double] = {
    val bestAlpha: Double = results.best(alpha)
    val bestBetas: Seq[Double] = results.best(beta).toVector
    ins.map { input =>
      (bestBetas zip input).foldLeft(bestAlpha) { case (tot, (a, b)) => tot + a * b }
    }
  }

  def residualsMAP(results: StanResults): Vector[Double] =
    (predictMAP(results)() zip outputs).map { case (a, b) => a - b }

}
