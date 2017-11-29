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

import com.cibo.scalastan.ScalaStan

object EightSchools extends App with ScalaStan {

  val j = data(int(lower = 0))
  val y = data(real()(j))
  val sigma = data(real(lower = 0)(j))

  val mu = parameter(real())
  val tau = parameter(real(lower = 0))
  val theta = parameter(real()(j))

  val model = new Model {
    mu ~ stan.Normal(0, 5)
    tau ~ stan.Cauchy(0, 5)
    theta ~ stan.Normal(mu, tau)
    y ~ stan.Normal(theta, sigma)
  }

  val ys = Seq[Double](28, 8, -3, 7, -1, 1, 18, 12)
  val sigmas = Seq[Double](15, 10, 16, 11, 9, 11, 10, 18)
  val result = model
    .withData(y, ys)
    .withData(sigma, sigmas)
    .run(
      chains = 4,
      seed = 194838
    )

  result.summary(System.out)

  result.checkTreeDepth(15)
  result.checkEnergy()
  result.checkDivergence()

  val (thetaDiv, thetaNoDiv) = result.partitionByDivergence(theta)

}
