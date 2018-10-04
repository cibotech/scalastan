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

import com.cibo.scalastan.StanModel

object NormalExample extends App {

  object model extends StanModel {
    val N = data(int(lower = 0))
    val y = data(vector(N))

    val sigma = parameter(real(lower = 0.0))
    val mu = parameter(real())

    y ~ stan.normal(mu, sigma)
  }

  val dataset = Vector(1.0, -2.0, 3.0, 0.0, 0.5, -1.0)
  val results = model
    .withData(model.y, dataset)
    .run()
  val muMean: Double = results.mean(model.mu)
  println(s"mu = $muMean")
  println(s"sigma = ${results.mean(model.sigma)}, ${results.sd(model.sigma)}, ${results.quantile(model.sigma, 0.5)}")

}

class NormalExampleSpec extends AppRunnerSpec(NormalExample)
