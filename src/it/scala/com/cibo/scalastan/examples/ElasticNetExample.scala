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
import com.cibo.scalastan.ast.{StanDataDeclaration, StanParameterDeclaration}

object ElasticNetExample extends App {

  // Elastic Net implementation based on code from
  // "Stan Modeling Language: User's Guide and Reference Manual" version 2.16.0.

  trait RegressionLike extends StanModel {
    val n: StanDataDeclaration[StanInt] = data(int(lower = 0))
    val p: StanDataDeclaration[StanInt] = data(int(lower = 0))
    val x: StanDataDeclaration[StanMatrix] = data(matrix(n, p))
    val y: StanDataDeclaration[StanVector] = data(vector(n))
    val beta: StanParameterDeclaration[StanVector] = parameter(vector(p))
  }

  // Linear regression by minimizing the squared error.
  trait LeastSquaresLike extends RegressionLike {
    target += -stan.dot_self(y - x * beta)
  }

  // Add a Ridge penalty (penalize the Euclidean length of the coefficients).
  trait RidgePenaltyLike extends RegressionLike {
    val ridgeLambda: StanDataDeclaration[StanReal] = data(real(lower = 0.0))

    target += -ridgeLambda * stan.dot_self(beta)
  }

  // Add a Lasso penalty (penalize the sum of the absolute coefficients).
  trait LassoPenaltyLike extends RegressionLike {
    val lassoLambda: StanDataDeclaration[StanReal] = data(real(lower = 0.0))

    for (i <- beta.range) {
      target += -lassoLambda * stan.fabs(beta(i))
    }
  }

  // Model to perform least squares regression.
  class LeastSquaresRegression extends LeastSquaresLike

  // Model to perform Ridge regression.
  // This uses the least squares trait along with the Ridge penalty.
  class RidgeRegression extends LeastSquaresLike with RidgePenaltyLike

  // Model to perform Lasso regression.
  // This uses the least squares trait along with the Lasso penalty.
  class LassoRegression extends LeastSquaresLike with LassoPenaltyLike

  // Model to perform Elastic Net regression.
  // This uses the least squares trait along with both the Ridge and Lasso penalties.
  class ElasticNetRegression extends LeastSquaresLike with RidgePenaltyLike with LassoPenaltyLike {
    val betaElasticNet = new GeneratedQuantity(vector(stan.cols(x))) {
      result := (1 + ridgeLambda) * beta
    }
  }

  val model = new ElasticNetRegression

  val xs = Seq(Seq(1.0, 0.5, 0.3), Seq(2.1, 0.7, 0.1), Seq(2.9, 0.8, 0.2))
  val ys = Seq(1.5, 2.5, 3.5)

  val results = model
    .withData(model.lassoLambda, 0.1)
    .withData(model.ridgeLambda, 0.2)
    .withData(model.x, xs)
    .withData(model.y, ys)
    .run(method = RunMethod.Optimize())
  results.summary(System.out)

}

class ElasticNetExampleSpec extends AppRunnerSpec(ElasticNetExample)
