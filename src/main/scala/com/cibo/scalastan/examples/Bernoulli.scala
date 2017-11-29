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

import com.cibo.scalastan.{RunMethod, ScalaStan}

object Bernoulli extends App with ScalaStan {

  val N = data(int(lower = 0))
  val y = data(int(lower = 0, upper = 1)(N))
  val theta = parameter(real(lower = 0.0, upper = 1.0))

  val model = new Model {
    theta ~ stan.Beta(1.0, 1.0)
    for(n <- y.range) {
      target += stan.Bernoulli(theta).lpmf(y(n))
    }
  }

  val dataset = Seq(0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
  val results = model
    .withData(y, dataset)
    .run(chains = 5, method = RunMethod.Sample())
  println(s"mean(${theta.name}) = ${results.mean(theta)}")
  println(s"N_Eff(${theta.name}) = ${results.effectiveSampleSize(theta)}")

  results.checkTreeDepth()
  results.checkEnergy()
  results.checkDivergence()

  results.summary(System.out)
}
