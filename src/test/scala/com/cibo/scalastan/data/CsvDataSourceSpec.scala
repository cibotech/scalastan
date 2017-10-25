package com.cibo.scalastan.data

import com.cibo.scalastan._

class CsvDataSourceSpec extends ScalaStanBaseSpec {
  describe("CsvDataSource") {
    it("parses simple CSV") {
      val data = "one,two\n0.5,1.0\n1.5,2.5\n"
      val src = CsvDataSource.fromString(data)
      val n = StanDataDeclaration(int())
      val decl1 = StanDataDeclaration(vector(n))
      val decl2 = StanDataDeclaration(real()(n))

      src.read(decl1, "one") shouldBe Seq(0.5, 1.5)
      src.read(decl2, "two") shouldBe Seq(1.0, 2.5)
    }

    it("parses CSV with quoted headers") {
      val data = "\"one\", \"two\"\n0.5,1.0\n1.5,2.5\n"
      val src = CsvDataSource.fromString(data)
      val n = StanDataDeclaration(int())
      val decl1 = StanDataDeclaration(vector(n))
      val decl2 = StanDataDeclaration(real()(n))

      src.read(decl1, "one") shouldBe Seq(0.5, 1.5)
      src.read(decl2, "two") shouldBe Seq(1.0, 2.5)
    }
  }
}
