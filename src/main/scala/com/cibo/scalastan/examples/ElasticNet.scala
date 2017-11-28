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

object ElasticNet extends App with ScalaStan {

  // Linear regression by minimizing the squared error.
  trait LeastSquaresLike extends Model {
    val x: StanDataDeclaration[StanMatrix]
    val y: StanDataDeclaration[StanVector]
    val beta: StanParameterDeclaration[StanVector]

    target += -dotSelf(y - x * beta)
  }

  // Add a Ridge penalty (penalize the Euclidean length of the coefficients).
  trait RidgePenaltyLike extends Model {
    val ridgeLambda: StanDataDeclaration[StanReal]
    val beta: StanParameterDeclaration[StanVector]

    target += -ridgeLambda * dotSelf(beta)
  }

  // Add a Lasso penalty (penalize the sum of the absolute coefficients).
  trait LassoPenaltyLike extends Model {
    val lassoLambda: StanDataDeclaration[StanReal]
    val beta: StanParameterDeclaration[StanVector]

    for (i <- beta.range) {
      target += -lassoLambda * fabs(beta(i))
    }
  }

  // Model to perform least squares regression.
  case class LeastSquaresRegression(
    x: StanDataDeclaration[StanMatrix],
    y: StanDataDeclaration[StanVector],
    beta: StanParameterDeclaration[StanVector]
  ) extends LeastSquaresLike

  // Model to perform Ridge regression.
  // This uses the least squares trait along with the Ridge penalty.
  case class RidgeRegression(
    x: StanDataDeclaration[StanMatrix],
    y: StanDataDeclaration[StanVector],
    beta: StanParameterDeclaration[StanVector],
    ridgeLambda: StanDataDeclaration[StanReal]
  ) extends LeastSquaresLike with RidgePenaltyLike

  // Model to perform Lasso regression.
  // This uses the least squares trait along with the Lasso penalty.
  case class LassoRegression(
    x: StanDataDeclaration[StanMatrix],
    y: StanDataDeclaration[StanVector],
    beta: StanParameterDeclaration[StanVector],
    lassoLambda: StanDataDeclaration[StanReal]
  ) extends LeastSquaresLike with LassoPenaltyLike

  // Model to perform Elastic Net regression.
  // This uses the least squares trait along with both the Ridge and Lasso penalties.
  case class ElasticNetRegression(
    x: StanDataDeclaration[StanMatrix],
    y: StanDataDeclaration[StanVector],
    beta: StanParameterDeclaration[StanVector],
    ridgeLambda: StanDataDeclaration[StanReal],
    lassoLambda: StanDataDeclaration[StanReal]
  ) extends LeastSquaresLike with RidgePenaltyLike with LassoPenaltyLike {
    val betaElasticNet = new GeneratedQuantity(vector(cols(x))) {
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

  val xs = Seq(Seq(1.0, 0.5), Seq(2.1, 0.7), Seq(2.9, 0.8))
  val ys = Seq(1.5, 2.5, 3.5)

  val results = model
    .withData(lambda1, 0.1)
    .withData(lambda2, 0.1)
    .withData(x, xs)
    .withData(y, ys)
    .run(chains = 4, method = RunMethod.Optimize())
  results.summary(System.out)

}
