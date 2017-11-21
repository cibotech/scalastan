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

object Exponential extends App with ScalaStan {

  val n = data(int(lower = 0))
  val y = data(vector(n))
  val lambda = parameter(real(lower = 0))

  val model = new Model {
    val alpha = local(real())
    val beta = local(real())

    alpha := 1.0
    beta := 1.0

    lambda ~ Gamma(alpha, beta)
    y ~ Exponential(lambda)
  }

  val dataset = Vector(1.0, 2.0, 3.0)
  val results = model
    .withData(y, dataset)
    .run()
  println(s"lambda = ${results.best(lambda)}")

}
