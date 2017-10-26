package com.cibo.scalastan.example

import com.cibo.scalastan.{RunMethod, ScalaStan}

object EightSchools extends App with ScalaStan {

  val j = data(int(lower = 0))
  val y = data(real()(j))
  val sigma = data(real(lower = 0)(j))

  val mu = parameter(real())
  val tau = parameter(real(lower = 0))
  val theta = parameter(real()(j))

  val model = new Model {
    mu ~ Normal(0, 5)
    tau ~ Cauchy(0, 5)
    theta ~ Normal(mu, tau)
    y ~ Normal(theta, sigma)
  }

  val ys = Seq[Double](28, 8, -3, 7, -1, 1, 18, 12)
  val sigmas = Seq[Double](15, 10, 16, 11, 9, 11, 10, 18)
  val result = model
    .withData(y, ys)
    .withData(sigma, sigmas)
    .run(
      chains = 4,
      seed = 194838,
      method = RunMethod.Sample(
        algorithm = RunMethod.Hmc(
          engine = RunMethod.Nuts(
            maxDepth = 15
          )
        ),
        adapt = RunMethod.SampleAdapt(
          delta = 0.9
        )
      )
    )

  result.summary(System.out)

  result.checkTreeDepth(15)
  result.checkEnergy()
  result.checkDivergence()

  val (thetaDiv, thetaNoDiv) = result.partitionByDivergence(theta)

}
