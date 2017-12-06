package com.cibo.scalastan.examples

import com.cibo.scalastan.ScalaStan

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

  val N = data(int())       // Data points
  val z0 = data(real()(2))  // Initial populations
  val ts = data(real()(N))  // Solution times

  val theta = parameter(real()(4))

  val z = new ParameterTransform(real()(N, 2)) {
    result := stan.integrate_ode_rk45(dz_dt, z0, 0.0, ts, theta, stan.rep_array(0.0, 0),
      stan.rep_array(0, 0), 1e-6, 1e-4, 1000)
  }

  val model = new Model {}
  model.emit(System.out)

}
