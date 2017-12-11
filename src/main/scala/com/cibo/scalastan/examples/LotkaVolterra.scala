package com.cibo.scalastan.examples

import com.cibo.scalastan.ScalaStan

object LotkaVolterra extends App with ScalaStan {

  // Lotka-Volterra for Predator-Prey Populations example from
  // https://github.com/stan-dev/example-models

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

  val z = new TransformedParameter(real()(N, 2)) {
    result := stan.integrate_ode_rk45(dz_dt, z0, 0.0, ts, theta, stan.rep_array(0.0, 0),
      stan.rep_array(0, 0), 1e-5, 1e-3, 500)
  }

  val model = new Model {
    sigma ~ stan.lognormal(0, 0.5)
    theta(1) ~ stan.normal(1, 0.5)
    theta(3) ~ stan.normal(1, 0.5)
    theta(2) ~ stan.normal(0.05, 0.05)
    theta(4) ~ stan.normal(0.05, 0.05)

    z0(1) ~ stan.lognormal(stan.log(30), 1)
    z0(2) ~ stan.lognormal(stan.log(5), 1)

    for (k <- range(1, 2)) {
      y0(k) ~ stan.lognormal(stan.log(z0(k)), sigma)
      for (i <- range(1, N)) {
        y(i, k) ~ stan.lognormal(stan.log(z(i, k)), sigma)
      }
    }
  }

  val y0_rep = new GeneratedQuantity(real()(2)) {
    for (k <- range(1, 2)) {
      result(k) := stan.lognormal(stan.log(z0(k)), sigma(k)).rng
    }
  }

  val y_rep = new GeneratedQuantity(real()(N, 2)) {
    for (k <- range(1, 2)) {
      for (n <- range(1, N)) {
        result(n, k) := stan.lognormal(stan.log(z(n, k)), sigma(k)).rng
      }
    }
  }

  val data = Seq(
    Seq(4.0, 30.0),
    Seq(6.1, 47.2),
    Seq(9.8, 70.2),
    Seq(35.2, 77.4),
    Seq(59.4, 36.3),
    Seq(41.7, 20.6),
    Seq(19.0, 18.1),
    Seq(13.0, 21.4),
    Seq(8.3, 22.0),
    Seq(9.1, 25.4),
    Seq(7.4, 27.1),
    Seq(8.0, 40.3),
    Seq(12.3, 57.0),
    Seq(19.5, 76.6),
    Seq(45.7, 52.3),
    Seq(51.1, 19.5),
    Seq(29.7, 11.2),
    Seq(15.8, 7.6),
    Seq(9.7, 14.6),
    Seq(10.1, 16.2),
    Seq(8.6, 24.7)
  )
  val years = Seq[Double](
    1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909,
    1910, 1911, 1912, 1913, 1914, 1915, 1916, 1917, 1918, 1919,
    1920
  )
  val results = model
    .withData(y, data)
    .withData(y0, data.head)
    .withData(ts, years.map(_ - 1899))
    .run()

  results.summary(System.out)
}
