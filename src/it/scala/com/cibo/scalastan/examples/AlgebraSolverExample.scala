package com.cibo.scalastan.examples

import com.cibo.scalastan.{RunMethod, StanModel}

object AlgebraSolverExample extends App {

  object model extends StanModel {

    val system = new Function(vector()) {
      val y = input(vector())
      val theta = input(vector())
      val x_r = input(real()())
      val x_i = input(int()())

      val z = local(vector(2))
      z(1) := y(1) - theta(1)
      z(2) := y(1) * y(2) + theta(2)
      output(z)
    }

    val y_guess = data(vector(2))
    val x_r = new TransformedData(real()(0)) { }
    val x_i = new TransformedData(int()(0)) { }

    val theta = new TransformedParameter(vector(2)) {
      result(1) := 3
      result(2) := 6
    }

    val y = new GeneratedQuantity(vector(2)) {
      result := stan.algebra_solver(system, y_guess, theta, x_r, x_i)
    }

    val z = new GeneratedQuantity(vector(2)) {
      result := system(y, theta, x_r, x_i)
    }
  }

  val results = model
    .withData(model.y_guess, Seq(1.0, 1.0))
    .run(chains = 1, method = RunMethod.Sample(samples = 1, algorithm = RunMethod.FixedParam()))

  results.summary(System.out, model.y, model.z)
}
