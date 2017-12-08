package com.cibo.scalastan.examples

import com.cibo.scalastan.models.NaiveBayes

class NaiveBayesExampleSpec extends AppRunnerSpec(NaiveBayesExample)

object NaiveBayesExample extends App {

  // Mapping from group -> document -> words
  val documents = Seq[Map[Int, Set[Int]]](
    Map(
      1 -> Set(1, 2),
      2 -> Set(3),
      3 -> Set(3, 4),
      4 -> Set(4)
    ),
    Map(
      5 -> Set(5),
      6 -> Set(4, 5)
    )
  )

  val model = NaiveBayes(documents)
  val results = model.compile.run()
  results.summary(System.out)

  println(model.classify(Set(1, 2, 4), results))
  println(model.classify(Set(4, 5), results))
}
