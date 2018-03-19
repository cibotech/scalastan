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
import com.cibo.scalastan.data.RDataSource

object DogsExample extends App with ScalaStan {

  // Dogs example from "Applied Regression Modeling", Gelman and Hill 2007.
  // Translated from the Stan example model from https://github.com/stan-dev/example-models

  val nDogs = data(int(lower = 0))
  val nTrials = data(int(lower = 0))
  val y = data(int(lower = 0, upper = 1)(nDogs, nTrials))

  val beta = parameter(vector(3))

  val nAvoid = new TransformedParameter(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      result(j, 1) := 0
      for (t <- range(2, nTrials)) {
        result(j, t) := result(j, t - 1) + 1 - y(j, t - 1)
      }
    }
  }

  val nShock = new TransformedParameter(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      result(j, 1) := 0
      for (t <- range(2, nTrials)) {
        result(j, t) := result(j, t - 1) + y(j, t - 1)
      }
    }
  }

  val p = new TransformedParameter(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      for (t <- range(1, nTrials)) {
        result(j, t) := beta(1) + beta(2) * nAvoid(j, t) + beta(3) * nShock(j, t)
      }
    }
  }

  val model = new Model {
    beta ~ stan.normal(0.0, 100.0)
    for (i <- range(1, nDogs)) {
      for (j <- range(1, nTrials)) {
        y(i, j) ~ stan.bernoulli_logit(p(i, j))
      }
    }
  }

  val rData = RDataSource.fromFile("dogs.R")
  val results = model
    .withData(rData(y, "y"))
    .run()

  //results.summary(System.out)
  for { (processOutput, i) <- results.chainOutputs.zipWithIndex } {
    println(s"chain output $i:")
    println("Error:")
    println(processOutput.error)
    println()
    println("Output:")
    println(processOutput.output)
  }
}

class DogsExampleSpec extends AppRunnerSpec(DogsExample)
