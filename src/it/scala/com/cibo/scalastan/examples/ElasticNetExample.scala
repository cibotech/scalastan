/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.examples

import com.cibo.scalastan._

object ElasticNetExample extends App with ScalaStan {

  // Elastic Net implementation based on code from
  // "Stan Modeling Language: User's Guide and Reference Manual" version 2.16.0.

  // Linear regression by minimizing the squared error.
  trait LeastSquaresLike extends Model {
    val x: DataDeclaration[StanMatrix]
    val y: DataDeclaration[StanVector]
    val beta: ParameterDeclaration[StanVector]

    target += -stan.dot_self(y - x * beta)
  }

  // Add a Ridge penalty (penalize the Euclidean length of the coefficients).
  trait RidgePenaltyLike extends Model {
    val ridgeLambda: DataDeclaration[StanReal]
    val beta: ParameterDeclaration[StanVector]

    target += -ridgeLambda * stan.dot_self(beta)
  }

  // Add a Lasso penalty (penalize the sum of the absolute coefficients).
  trait LassoPenaltyLike extends Model {
    val lassoLambda: DataDeclaration[StanReal]
    val beta: ParameterDeclaration[StanVector]

    for (i <- beta.range) {
      target += -lassoLambda * stan.fabs(beta(i))
    }
  }

  // Model to perform least squares regression.
  case class LeastSquaresRegression(
    x: DataDeclaration[StanMatrix],
    y: DataDeclaration[StanVector],
    beta: ParameterDeclaration[StanVector]
  ) extends LeastSquaresLike

  // Model to perform Ridge regression.
  // This uses the least squares trait along with the Ridge penalty.
  case class RidgeRegression(
    x: DataDeclaration[StanMatrix],
    y: DataDeclaration[StanVector],
    beta: ParameterDeclaration[StanVector],
    ridgeLambda: DataDeclaration[StanReal]
  ) extends LeastSquaresLike with RidgePenaltyLike

  // Model to perform Lasso regression.
  // This uses the least squares trait along with the Lasso penalty.
  case class LassoRegression(
    x: DataDeclaration[StanMatrix],
    y: DataDeclaration[StanVector],
    beta: ParameterDeclaration[StanVector],
    lassoLambda: DataDeclaration[StanReal]
  ) extends LeastSquaresLike with LassoPenaltyLike

  // Model to perform Elastic Net regression.
  // This uses the least squares trait along with both the Ridge and Lasso penalties.
  case class ElasticNetRegression(
    x: DataDeclaration[StanMatrix],
    y: DataDeclaration[StanVector],
    beta: ParameterDeclaration[StanVector],
    ridgeLambda: DataDeclaration[StanReal],
    lassoLambda: DataDeclaration[StanReal]
  ) extends LeastSquaresLike with RidgePenaltyLike with LassoPenaltyLike {
    val betaElasticNet = new GeneratedQuantity(vector(stan.cols(x))) {
      result := (1 + ridgeLambda) * beta
    }
  }

  // Ridge/lasso weights.
  val lambda1 = data(real(lower = 0))
  val lambda2 = data(real(lower = 0))

  // Inputs.
  val (n, p) = (data(int(lower = 0)), data(int(lower = 0)))
  val y = data(vector(n))
  val x = data(matrix(n, p))

  // The coefficient vector.
  val beta = parameter(vector(p))

  val model = ElasticNetRegression(x, y, beta, lambda1, lambda2)

  val xs = Seq(Seq(1.0, 0.5, 0.3), Seq(2.1, 0.7, 0.1), Seq(2.9, 0.8, 0.2))
  val ys = Seq(1.5, 2.5, 3.5)

  val results = model
    .withData(lambda1, 0.2)
    .withData(lambda2, 0.1)
    .withData(x, xs)
    .withData(y, ys)
    .run(chains = 4, method = RunMethod.Optimize())
  results.summary(System.out)

}

class ElasticNetExampleSpec extends AppRunnerSpec(ElasticNetExample)
