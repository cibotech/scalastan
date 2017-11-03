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

    it("parses int arrays") {
      val data = "one\n1\n2\n"
      val src = CsvDataSource.fromString(data)
      val n = StanDataDeclaration(int())
      val decl = StanDataDeclaration(int()(n))

      src.read(decl, "one") shouldBe Seq(1, 2)
    }

    it("parses real arrays") {
      val data = "one\n1.5\n2.5\n"
      val src = CsvDataSource.fromString(data)
      val n = StanDataDeclaration(int())
      val decl = StanDataDeclaration(real()(n))

      src.read(decl, "one") shouldBe Seq(1.5, 2.5)
    }

    it("returns column names") {
      val data = "one,two\n1,2\n"
      val src = CsvDataSource.fromString(data)

      src.columns shouldBe Set("one", "two")
    }

    it("returns row counts") {
      val data = "one\n1\n2\n3\n"
      val src = CsvDataSource.fromString(data)

      src.rows shouldBe 3
    }

    it("reads matrices") {
      val data = "one,two\n1,2\n3,4\n"
      val src = CsvDataSource.fromString(data)

      src.readMatrix(Seq("one", "two")) shouldBe Vector(Vector(1.0, 2.0), Vector(3.0, 4.0))
    }

    it("reads vectors") {
      val data = "one,two\n1,2\n3,4\n"
      val src = CsvDataSource.fromString(data)

      src.readVector("one") shouldBe Vector(1.0, 3.0)
    }

    it("strips whitespace from column names") {
      val data = "\"one  \",\" two \"\n1,2\n"
      val src = CsvDataSource.fromString(data)

      src.readVector("one") shouldBe Vector(1.0)
      src.readVector("two") shouldBe Vector(2.0)
    }

    it("removes quotes from values") {
      val data = "one\n\"1\"\n"
      val src = CsvDataSource.fromString(data)

      src.readVector("one") shouldBe Vector(1.0)
    }
  }
}
