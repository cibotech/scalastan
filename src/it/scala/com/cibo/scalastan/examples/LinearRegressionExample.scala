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

import com.cibo.scalastan.models.LinearRegression

class LinearRegressionExampleSpec extends AppRunnerSpec(LinearRegressionExample)

object LinearRegressionExample extends App {

  val xs = Seq[Seq[Double]](Seq(1), Seq(2), Seq(3), Seq(4), Seq(6))
  val ys = Seq[Double](2.9, 6.1, 7.0, 9.2, 13.1)

  val model = LinearRegression(xs, ys)
  val results = model.compile.run(chains = 4)

  results.summary(System.out)

  println(s"beta0: ${results.best(model.beta0)}")
  println(s"beta:  ${results.best(model.beta)}")
  println(s"sigma: ${results.best(model.sigma)}")

}
