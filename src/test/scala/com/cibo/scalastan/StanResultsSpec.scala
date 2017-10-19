package com.cibo.scalastan

class StanResultsSpec extends ScalaStanBaseSpec {

  private val v1 = StanDeclaration[StanInt, ParameterDeclarationType](StanInt())
  private val v2 = StanDeclaration[StanVector, ParameterDeclarationType](StanVector(v1))
  private val v3 = StanDeclaration[StanArray[StanVector], ParameterDeclarationType](StanArray(v1, StanVector(v1)))

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

  val results = StanResults(Seq(testData1, testData2))

  describe("samples") {
    it("returns all scalar samples") {
      results.samples(v1) shouldBe Vector(1, 0)
    }

    it("returns all vector samples") {
      results.samples(v2) shouldBe Vector(Vector(2.0, 3.0, 4.0), Vector(1.0, 2.0, 3.0))
    }

    it("returns all array of vector samples") {
      results.samples(v3) shouldBe Vector(
        Vector(
          Vector(311, 312),
          Vector(321, 322),
          Vector(331, 332)
        ), Vector(
          Vector(311, 312),
          Vector(321, 322),
          Vector(331, 332)
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
}
