package com.cibo.scalastan

import java.io._

class StanResultsSpec extends ScalaStanBaseSpec {

  private implicit val ss = new ScalaStan {}
  private val model = CompiledModel(new File("."), ss)

  private val v1 = StanParameterDeclaration[StanInt](StanInt())
  private val v2 = StanParameterDeclaration[StanVector](StanVector(v1))
  private val v3 = StanParameterDeclaration[StanArray[StanVector]](StanArray(v1, StanVector(v1)))

  private val testData1 = Map[String, Int](
    "lp__" -> 1,
    v1.emit -> 1,
    s"${v2.emit}.1" -> 2,
    s"${v2.emit}.2" -> 3,
    s"${v2.emit}.3" -> 4,
    s"${v3.emit}.1.1" -> 311,
    s"${v3.emit}.1.2" -> 312,
    s"${v3.emit}.2.1" -> 321,
    s"${v3.emit}.2.2" -> 322,
    s"${v3.emit}.3.1" -> 331,
    s"${v3.emit}.3.2" -> 332
  ).mapValues(_.toString)

  private val testData2 = Map[String, Int](
    "lp__" -> 2,
    v1.emit -> 0,
    s"${v2.emit}.1" -> 1,
    s"${v2.emit}.2" -> 2,
    s"${v2.emit}.3" -> 3,
    s"${v3.emit}.1.1" -> 311,
    s"${v3.emit}.1.2" -> 312,
    s"${v3.emit}.2.1" -> 321,
    s"${v3.emit}.2.2" -> 322,
    s"${v3.emit}.3.1" -> 331,
    s"${v3.emit}.3.2" -> 332
  ).mapValues(_.toString)

  val results = StanResults(Vector(Vector(testData1, testData2)), ss, model)

  describe("samples") {
    it("returns all scalar samples") {
      results.samples(v1) shouldBe Seq(Seq(1, 0))
    }

    it("returns all vector samples") {
      results.samples(v2) shouldBe Seq(Seq(Seq(2.0, 3.0, 4.0), Seq(1.0, 2.0, 3.0)))
    }

    it("returns all array of vector samples") {
      results.samples(v3) shouldBe Seq(
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
      results.best(v1) shouldBe 0
    }
  }

  describe("mean") {
    it("returns the mean scalar") {
      results.mean(v1) shouldBe 0.5 // Note that this will always convert to double
    }

    it("returns the mean vector") {
      results.mean(v2) shouldBe Vector(1.5, 2.5, 3.5)
    }
  }

  describe("variance") {
    it("returns the variance") {
      results.variance(v1) shouldBe 0.5
    }
  }

  describe("sd") {
    it("returns the standard deviation") {
      results.sd(v1) shouldBe math.sqrt(0.5)
    }
  }

  describe("min") {
    it("returns the min") {
      results.min(v1) shouldBe 0
    }
  }

  describe("max") {
    it("returns the max") {
      results.max(v1) shouldBe 1
    }
  }

  describe("quantile") {
    it("returns 5%") {
      results.quantile(v1, 0.05) shouldBe 0
    }

    it("returns 95%") {
      results.quantile(v1, 0.95) shouldBe 1
    }
  }

  describe("effectiveSampleSize") {
    it("should return the effective sample size") {
      results.effectiveSampleSize(v1) shouldBe 2
    }
  }

  describe("mcse") {
    it("returns the mcse") {
      results.mcse(v1) shouldBe 0.5
    }
  }

  describe("summary") {
    it("should return a line for each parameter") {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      results.summary(pw)
      pw.close()
      val summary = sw.toString.split("\n").toSeq

      Seq("lp__", s"${v1.emit}", s"${v2.emit}", s"${v3.emit}").foreach { name =>
        withClue(s"$name in $summary") {
          summary.exists(_.startsWith(name)) shouldBe true
        }
      }
    }
  }
}
