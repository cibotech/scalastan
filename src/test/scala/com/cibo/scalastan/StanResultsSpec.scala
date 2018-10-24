package com.cibo.scalastan

import java.io._

class StanResultsSpec extends ScalaStanBaseSpec {

  private object TestModel extends StanModel {
    val n = data(int())
    val v1 = parameter(real())
    val v2 = parameter(vector(n))
    val v3 = parameter(vector(n)(n))

    v1 ~ stan.normal(0, n)
    v2 ~ stan.normal(0, 1)
    v3(1) ~ stan.normal(0, 1)
  }

  private val model = CompiledModel(TestModel, MockRunner())

  private val testData1 = Map[String, Int](
    "lp__" -> 1,
    TestModel.v1.emit -> 1,
    s"${TestModel.v2.emit}.1" -> 2,
    s"${TestModel.v2.emit}.2" -> 3,
    s"${TestModel.v2.emit}.3" -> 4,
    s"${TestModel.v3.emit}.1.1" -> 311,
    s"${TestModel.v3.emit}.1.2" -> 312,
    s"${TestModel.v3.emit}.2.1" -> 321,
    s"${TestModel.v3.emit}.2.2" -> 322,
    s"${TestModel.v3.emit}.3.1" -> 331,
    s"${TestModel.v3.emit}.3.2" -> 332
  ).mapValues(_.toString)

  private val testData2 = Map[String, Int](
    "lp__" -> 2,
    TestModel.v1.emit -> 0,
    s"${TestModel.v2.emit}.1" -> 1,
    s"${TestModel.v2.emit}.2" -> 2,
    s"${TestModel.v2.emit}.3" -> 3,
    s"${TestModel.v3.emit}.1.1" -> 311,
    s"${TestModel.v3.emit}.1.2" -> 312,
    s"${TestModel.v3.emit}.2.1" -> 321,
    s"${TestModel.v3.emit}.2.2" -> 322,
    s"${TestModel.v3.emit}.3.1" -> 331,
    s"${TestModel.v3.emit}.3.2" -> 332
  ).mapValues(_.toString)

  val mappedData: Map[String, Vector[Vector[String]]] = Seq(testData1, testData2).flatten.groupBy(_._1).mapValues(
    grouped => Vector(grouped.map{ case(k, v) => v }.toVector)
  )
  val results = StanResults(mappedData, Vector.empty, model, RunMethod.Sample())

  describe("parameters") {
    it("returns all parameters") {
      results.parameters.size shouldBe 3
    }
  }

  describe("elements") {
    it("returns all the elements of a parameter") {
      results.elements(TestModel.v3).size shouldBe 6
    }

    it("returns the right thing for an array") {
      results.elements(TestModel.v2).map(_.name) shouldBe Vector(
        TestModel.v2.get(1).name,
        TestModel.v2.get(2).name,
        TestModel.v2.get(3).name
      )
    }

    it("returns the right value for a scalar") {
      results.elements(TestModel.v1).map(_.name) shouldBe Seq(TestModel.v1.name)
    }
  }

  describe("data") {
    it("returns data") {
      results.data.size shouldBe 1
    }
  }

  describe("samples") {
    it("returns all scalar samples") {
      results.samples(TestModel.v1) shouldBe Seq(Seq(1, 0))
    }

    it("returns all vector samples") {
      results.samples(TestModel.v2) shouldBe Seq(Seq(Seq(2.0, 3.0, 4.0), Seq(1.0, 2.0, 3.0)))
    }

    it("returns all array of vector samples") {
      results.samples(TestModel.v3) shouldBe Seq(
        Seq(
          Seq(
            Seq(311, 312),
            Seq(321, 322),
            Seq(331, 332)
          ), Seq(
            Seq(311, 312),
            Seq(321, 322),
            Seq(331, 332)
          )
        )
      )
    }
  }

  describe("best index") {
    it("should return the right index") {
      results.bestIndex shouldBe 1
    }
  }

  describe("best") {
    it("returns the best scalar sample") {
      results.best(TestModel.v1) shouldBe 0
    }

    it("returns the best lp") {
      results.best(results.logPosterior) shouldBe 2
    }
  }

  describe("mean") {
    it("returns the mean scalar") {
      results.mean(TestModel.v1) shouldBe 0.5 // Note that this will always convert to double
    }

    it("returns the mean vector") {
      results.mean(TestModel.v2) shouldBe Vector(1.5, 2.5, 3.5)
    }

    it("returns the mean array of vectors") {
      results.mean(TestModel.v3) shouldBe Vector(Vector(311.0, 312.0), Vector(321.0, 322.0), Vector(331.0, 332.0))
    }

    it("returns the mean lp") {
      results.mean(results.logPosterior) shouldBe 1.5
    }
  }

  describe("variance") {
    it("returns the variance") {
      results.variance(TestModel.v1) shouldBe 0.5
    }

    it("returns the variance of lp") {
      results.variance(results.logPosterior) shouldBe 0.5
    }
  }

  describe("sd") {
    it("returns the standard deviation") {
      results.sd(TestModel.v1) shouldBe math.sqrt(0.5)
    }

    it("returns the sd of lp") {
      results.sd(results.logPosterior) shouldBe math.sqrt(0.5)
    }
  }

  describe("min") {
    it("returns the min") {
      results.min(TestModel.v1) shouldBe 0
    }

    it("returns the min lp") {
      results.min(results.logPosterior) shouldBe 1
    }
  }

  describe("max") {
    it("returns the max") {
      results.max(TestModel.v1) shouldBe 1
    }

    it("returns the max lp") {
      results.max(results.logPosterior) shouldBe 2
    }
  }

  describe("quantile") {
    it("returns 5%") {
      results.quantile(TestModel.v1, 0.05) shouldBe 0
    }

    it("returns 95%") {
      results.quantile(TestModel.v1, 0.95) shouldBe 1
    }

    it("returns the 5% lp") {
      results.quantile(results.logPosterior, 0.05) shouldBe 1
    }
  }

  describe("effectiveSampleSize") {
    it("should return the effective sample size") {
      results.effectiveSampleSize(TestModel.v1) shouldBe 2
    }
  }

  describe("mcse") {
    it("returns the mcse") {
      results.mcse(TestModel.v1) shouldBe 0.5
    }

    it("returns the mcse lp") {
      results.mcse(results.logPosterior) shouldBe 0.5
    }
  }

  describe("psrf") {
    it("returns a PSRF for a single chain") {
      results.psrf(Seq(Seq(1.0, 2.0, 3.0, 5.0))) should be > 1.0
    }
  }

  describe("summary") {
    it("should return a line for each parameter") {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      results.summary(pw)
      pw.close()
      val summary = sw.toString.split("\n").toSeq

      Seq("lp__", s"${TestModel.v1.emit}", s"${TestModel.v2.emit}", s"${TestModel.v3.emit}").foreach { name =>
        withClue(s"$name in $summary") {
          summary.exists(_.startsWith(name)) shouldBe true
        }
      }
    }

    it("should filter") {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      results.summary(pw, TestModel.v1)
      pw.close()
      val summary = sw.toString.split("\n").toSeq

      Seq("lp__", s"${TestModel.v1.emit}").foreach { name =>
        withClue(s"$name in $summary") {
          summary.exists(_.startsWith(name)) shouldBe true
        }
      }
    }
  }
}
