package com.cibo.scalastan.examples

import com.cibo.scalastan.models.SoftKMeans

object SoftKMeansExample extends App {

  val observations = Seq[Seq[Double]](
    Seq(1, 1, 1),
    Seq(1, 4, 1),
    Seq(1, 0, 1),
    Seq(1, 5, 1)
  )
  val model = SoftKMeans(2, observations)

  val results = model.compile.run()
  results.summary(System.out)

  println(model.clusterAssignments(results))
}
