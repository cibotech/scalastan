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

object KidScoreMomHs extends App with ScalaStan {

  val N = data(int(lower = 0))
  val kidScore = data(vector(N, lower = 0, upper = 200))
  val momHs = data(vector(N, lower = 0, upper = 1))

  val beta = parameter(vector(2))
  val sigma = parameter(real(lower = 0))

  val model = new Model {
    sigma ~ stan.Cauchy(0, 2.5)
    kidScore ~ stan.Normal(beta(1) + beta(2) * momHs, sigma)
  }

  val dataset = Vector[(Double, Double)](
    (65, 1), (98, 1), (85, 1), (83, 1), (115, 1), (98, 0), (69, 1), (106, 1), (102, 1), (95, 1), (91, 1),
    (58, 1), (84, 1), (78, 1), (102, 1)
  )
  val results = model
    .withData(kidScore, dataset.map(_._1))
    .withData(momHs, dataset.map(_._2))
    .run()
  println("sigma: " + results.best(sigma))
  println("beta: " + results.best(beta))

}
