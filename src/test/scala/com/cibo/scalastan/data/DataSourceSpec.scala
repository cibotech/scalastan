package com.cibo.scalastan.data

import org.scalatest.{FunSpec, Matchers}

class DataSourceSpec extends FunSpec with Matchers {
  describe("sorted") {
    it("should sort the specified columns") {
      val ds = DataSource()
        .withVector("one", Vector(4, 3, 2, 1))
        .withVector("two", Vector(5, 6, 7, 8))
        .withVector("three", Vector(1, 2, 3, 4))
        .sorted("one", "two")

      ds.readVector("one") shouldBe Vector(1.0, 2.0, 3.0, 4.0)
      ds.readVector("two") shouldBe Vector(8.0, 7.0, 6.0, 5.0)
      ds.readVector("three") shouldBe Vector(1.0, 2.0, 3.0, 4.0)
    }
  }

  describe("createEnumeration") {
    it("creates an enumeration") {
      val ds = DataSource()
        .withVector("x", Vector("one", "two"))
        .createEnumeration("x")

      ds.readVector("x") shouldBe Vector(0.0, 1.0)
    }
  }

  describe("createIndicators") {
    it("creates indicator vectors for one column") {
      val ds = DataSource()
        .withVector("x", Vector("one", "two", "one"))
        .createIndicators("x")
      ds.readVector("x.one") shouldBe Vector(1.0, 0.0, 1.0)
      ds.readVector("x.two") shouldBe Vector(0.0, 1.0, 0.0)
    }

    it("creates indicator vectors for two columns") {
      val ds = DataSource()
        .withVector("x", Vector("one", "two", "one"))
        .withVector("y", Vector("three", "four", "three"))
        .createIndicators("x", "y")
      ds.readVector("x.y.one.three") shouldBe Vector(1.0, 0.0, 1.0)
      ds.readVector("x.y.two.four") shouldBe Vector(0.0, 1.0, 0.0)
    }
  }

  describe("getIndicators") {
    it("returns a column for each value for one column") {
      val ds = DataSource().withVector("x", Vector("one", "two", "one"))
      ds.getIndicators("x") shouldBe Seq("x.one", "x.two")
    }

    it("returns a column for each value for two columns") {
      val ds = DataSource()
        .withVector("x", Vector("one", "two", "one"))
        .withVector("y", Vector("three", "four", "three"))
      ds.getIndicators("x", "y") shouldBe Seq("x.y.one.three", "x.y.two.four")
    }
  }
}
