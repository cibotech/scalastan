package com.cibo.scalastan.example

import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.data.RDataSource

object Dogs extends App with ScalaStan {

  val nDogs = data(int(lower = 0))
  val nTrials = data(int(lower = 0))
  val y = data(int(lower = 0, upper = 1)(nDogs, nTrials))

  val beta = parameter(vector(3))

  val nAvoid = new ParameterTransform(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      result(j, 1) := 0
      for (t <- range(2, nTrials)) {
        result(j, t) := result(j, t - 1) + 1 - y(j, t - 1)
      }
    }
  }

  val nShock = new ParameterTransform(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      result(j, 1) := 0
      for (t <- range(2, nTrials)) {
        result(j, t) := result(j, t - 1) + y(j, t - 1)
      }
    }
  }

  val p = new ParameterTransform(matrix(nDogs, nTrials)) {
    for (j <- range(1, nDogs)) {
      for (t <- range(1, nTrials)) {
        result(j, t) := beta(1) + beta(2) * nAvoid(j, t) + beta(3) * nShock(j, t)
      }
    }
  }

  val model = new Model {
    beta ~ Normal(0.0, 100.0)
    for (i <- range(1, nDogs)) {
      for (j <- range(1, nTrials)) {
        y(i, j) ~ BernoulliLogit(p(i, j))
      }
    }
  }

  val rData = RDataSource.fromFile("dogs.R")
  val results = model
    .withData(rData(nDogs, "n_dogs"))
    .withData(rData(nTrials, "n_trials"))
    .withData(rData(y, "y"))
    .run()

  println(results.mean(beta))
  val pValues = results.mean(p.result)
  println(pValues(30 - 1)(19 - 1))

}
