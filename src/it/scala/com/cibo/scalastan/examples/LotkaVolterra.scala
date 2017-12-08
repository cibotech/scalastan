package com.cibo.scalastan.examples

import com.cibo.scalastan.ScalaStan

class LotkaVolterraSpec extends AppRunnerSpec(LotkaVolterra)

object LotkaVolterra extends App with ScalaStan {

  val dz_dt = new Function(real()()) {
    val t = input(real())
    val z = input(real()())
    val theta = input(real()())
    val x_r = input(real()())
    val x_i = input(int()())

    val u = z(1)
    val v = z(2)

    val alpha = theta(1)
    val beta = theta(2)
    val gamma = theta(3)
    val delta = theta(4)

    val du_dt = (alpha - beta * v) * u
    val dv_dt = (-gamma + delta * u) * v
    output(Seq(du_dt, dv_dt))
  }

  val N = data(int(lower = 0))
  val ts = data(real()(N))
  val y0 = data(real()(2))
  val y = data(real(lower = 0)(N, 2))

  val theta = parameter(real(lower = 0)(4))
  val z0 = parameter(real(lower = 0)(2))
  val sigma = parameter(real(lower = 0)(2))

  val z = new ParameterTransform(real()(N, 2)) {
    result := stan.integrate_ode_rk45(dz_dt, z0, 0.0, ts, theta, stan.rep_array(0.0, 0),
      stan.rep_array(0, 0), 1e-6, 1e-4, 1000)
  }

  val model = new Model {
    sigma ~ stan.lognormal(0, 0.5)
    theta(1) ~ stan.normal(1, 0.5)
    theta(3) ~ stan.normal(1, 0.5)
    theta(2) ~ stan.normal(0.05, 0.05)
    theta(4) ~ stan.normal(0.05, 0.05)

    z0(1) ~ stan.lognormal(stan.log(30), 5)
    z0(2) ~ stan.lognormal(stan.log(5), 5)

    for (k <- range(1, 2)) {
      y0(k) ~ stan.lognormal(stan.log(z0(k)), sigma)
      for (i <- range(1, N)) {
        y(i, k) ~ stan.lognormal(stan.log(z(i, k)), sigma)
      }
    }
  }
  model.emit(System.out)

  val data = Seq(
    Seq(27.0, 4.0),
    Seq(48.0, 5.0),
    Seq(74.0, 6.0),
    Seq(81.0, 30.0),
    Seq(32.0, 60.0)
  )
  val results = model
    .withData(y, data)
    .withData(y0, Seq(5.0, 5.0))
    .withData(ts, Seq(1.0, 2.0, 3.0, 4.0, 5.0))
    .run(chains = 4)

  results.summary(System.out)

}
