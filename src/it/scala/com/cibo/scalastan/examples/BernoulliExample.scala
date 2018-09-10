package com.cibo.scalastan.examples

import com.cibo.scalastan.{RunMethod, ScalaStan}

object BernoulliExample extends App with ScalaStan {

  val N = data(int(lower = 0))
  val y = data(int(lower = 0, upper = 1)(N))
  val theta = parameter(real(lower = 0.0, upper = 1.0))

  val model = new Model {
    theta ~ stan.beta(1.0, 1.0)
    target += stan.bernoulli(theta).lpmf(y)
  }

  val dataset = Seq(0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
  val results = model
    .withData(y, dataset)
    .run(chains = 5, method = RunMethod.Sample())
  println(s"mean(${theta.name}) = ${results.mean(theta)}")
  println(s"N_Eff(${theta.name}) = ${results.effectiveSampleSize(theta)}")

  println(s"Iterations saturation the max tree depth: ${results.checkTreeDepth}")
  println(s"Iterations below the energy threshold: ${results.checkEnergy()}")
  println(s"Iterations with a divergence: ${results.checkDivergence}")

  results.summary(System.out)
}

class BernoulliExampleSpec extends AppRunnerSpec(BernoulliExample)
