package com.cibo.scalastan.examples

import com.cibo.scalastan.StanModel

object ImportedModelExample extends App {

  object model extends StanModel {
    val n = data(int(lower = 0))
    val x = data(vector(n))

    val mu = parameter(real())
    val sigma = parameter(real(lower = 0.0))

    loadFromString(
      s"""
        data {
          int<lower=0> n;
          vector[n] x;
        }
        parameters {
          real mu;
          real<lower=0> sigma;
        }
        model {
          x ~ normal(mu, sigma);
        }
    """
    )
  }

  val xs = Seq(1.0, 0.5, 1.5, 0.75)

  val results = model
    .withData(model.x, xs)
    .withData(model.n, xs.length)
    .run()

  results.summary(System.out, model.mu, model.sigma)
}

class ImportedModelExampleSpec extends AppRunnerSpec(ImportedModelExample)
