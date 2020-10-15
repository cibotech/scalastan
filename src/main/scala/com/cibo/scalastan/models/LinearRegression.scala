/*
 * Copyright (c) 2017 - 2020 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.models

import com.cibo.scalastan._
import com.cibo.scalastan.ast.{StanDataDeclaration, StanParameterDeclaration, StanValue}
import com.cibo.scalastan.run.StanCompiler

object LinearRegression {

  // Common linear regression declarations.
  trait RegressionDeclarations extends StanModel {
    val n: StanDataDeclaration[StanInt] = data(int(lower = 0))
    val p: StanDataDeclaration[StanInt] = data(int(lower = 0))
    val x: StanDataDeclaration[StanMatrix] = data(matrix(n, p))
    val y: StanDataDeclaration[StanVector] = data(vector(n))
    val beta: StanParameterDeclaration[StanVector] = parameter(vector(p))
    val beta0: StanParameterDeclaration[StanReal] = parameter(real())

    def estimate: StanValue[StanVector] = x * beta + beta0
  }

  // Linear regression by minimizing the squared error.
  trait LeastSquaresRegression extends RegressionDeclarations {
    target += -stan.dot_self(y - estimate)
  }

  // Bayesian linear regression.
  trait BayesianRegression extends RegressionDeclarations {
    val sigma: StanParameterDeclaration[StanReal] = parameter(real(lower = 0.0))
    y ~ stan.normal(estimate, sigma)
  }

  // Ridge penalty (penalize the Euclidean length of the coefficients).
  trait RidgePenalty extends RegressionDeclarations {
    val ridgeLambda: StanDataDeclaration[StanReal] = data(real(lower = 0.0))
    target += -ridgeLambda * stan.dot_self(beta)
  }

  // Normal prior on predictors.
  trait NormalPrior extends RegressionDeclarations {
    val priorSigma: StanValue[StanReal] = 1.0
    beta ~ stan.normal(0, priorSigma)
  }

  // Horseshoe prior on predictors (for sparsity).
  trait HorseshoePrior extends RegressionDeclarations {
    val lambda: StanParameterDeclaration[StanVector] = parameter(vector(p, lower = 0.0))  // Local shrinkage
    val tau: StanParameterDeclaration[StanReal] = parameter(real(lower = 0.0))            // Global shrinkage

    beta ~ stan.normal(0, stan.square(lambda * tau))
    lambda ~ stan.cauchy(0, 1)
  }

  // Lasso penalty (penalize the sum of the absolute coefficients).
  trait LassoPenalty extends RegressionDeclarations {
    val lassoLambda: StanDataDeclaration[StanReal] = data(real(lower = 0.0))

    for (i <- beta.range) {
      target += -lassoLambda * stan.fabs(beta(i))
    }
  }

  // ElasticNet combines LeastSquares regression with the Lasso and Ridge penalties.
  trait ElasticNet extends LeastSquaresRegression with RidgePenalty with LassoPenalty

}

case class LinearRegression(
  xs: Seq[Seq[Double]],   // Inputs
  ys: Seq[Double]         // Outputs
) extends LinearRegression.BayesianRegression {

  require(xs.length == ys.length, "Length of inputs must match the length of the outputs")

  sigma ~ stan.cauchy(0, 1)

  override def compile(implicit compiler: StanCompiler): CompiledModel = super.compile
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
