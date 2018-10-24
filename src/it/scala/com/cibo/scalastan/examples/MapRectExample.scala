package com.cibo.scalastan.examples

import com.cibo.scalastan.StanModel

object MapRectExample extends App {

  object model extends StanModel {
    val n = data(int(lower = 0))
    val x = data(real()(n, 2))

    val alpha = parameter(real())
    val beta = parameter(real())
    val sigma = parameter(real(lower = 0.0))

    val func = new Function(vector()) {
      val phi = input(vector())
      val theta = input(vector())
      val x_r = input(real()())
      val x_i = input(int()())

      val r = local(vector(1))
      r(1) := stan.normal(phi(1) + phi(2) * x_r(1), phi(3)).lpdf(x_r(2))
      output(r)
    }

    val phi = local(vector(3))
    phi(1) := alpha
    phi(2) := beta
    phi(3) := sigma

    val theta = local(vector(0)(n))
    val xi = local(int()(n, 0))

    target += stan.sum(stan.map_rect(func, phi, theta, x, xi))
  }

  val compiledModel = model
    .withData(model.x,
      Seq(
        Seq(1.0, 2.5),
        Seq(3.0, 5.1),
        Seq(4.0, 6.3),
        Seq(5.0, 7.0)
      )
    )

  val results = compiledModel.run()
  results.summary(System.out)

}
