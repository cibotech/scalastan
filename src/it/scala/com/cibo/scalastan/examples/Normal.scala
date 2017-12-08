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

class NormalSpec extends AppRunnerSpec(Normal)

object Normal extends App with ScalaStan {

  val N = data(int(lower = 0))
  val y = data(vector(N))

  val sigma = parameter(real(lower = 0.0))
  val mu = parameter(real())

  val model = new Model {
    y ~ stan.normal(mu, sigma)
  }

  val dataset = Vector(1.0, -2.0, 3.0, 0.0, 0.5, -1.0)
  val results = model
    .withData(y, dataset)
    .run()
  println(s"mu = ${results.mean(mu)}")
  println(s"sigma = ${results.mean(sigma)}, ${results.sd(sigma)}, ${results.quantile(sigma, 0.5)}")

}
