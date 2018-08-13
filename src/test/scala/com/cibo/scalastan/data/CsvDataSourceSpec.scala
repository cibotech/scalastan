package com.cibo.scalastan.data

import com.cibo.scalastan._
import com.cibo.scalastan.ast.StanDataDeclaration

class CsvDataSourceSpec extends ScalaStanBaseSpec {

  describe("CsvDataSource") {
    it("parses simple CSV") {
      new ScalaStan {
        val data = "one,two\n0.5,1.0\n1.5,2.5\n"
        val src = CsvDataSource.fromString(data)
        val n = data(int())
        val decl1 = data(vector(n))
        val decl2 = data(real()(n))

        src.read(decl1, "one") shouldBe Seq(0.5, 1.5)
        src.read(decl2, "two") shouldBe Seq(1.0, 2.5)
      }
    }

    it("parses CSV with quoted headers") {
      new ScalaStan {
        val data = "\"one\", \"two\"\n0.5,1.0\n1.5,2.5\n"
        val src = CsvDataSource.fromString(data)
        val n = data(int())
        val decl1 = data(vector(n))
        val decl2 = data(real()(n))

        src.read(decl1, "one") shouldBe Seq(0.5, 1.5)
        src.read(decl2, "two") shouldBe Seq(1.0, 2.5)
      }
    }

    it("parses int arrays") {
      new ScalaStan {
        val data = "one\n1\n2\n"
        val src = CsvDataSource.fromString(data)
        val n = data(int())
        val decl = data(int()(n))

        src.read(decl, "one") shouldBe Seq(1, 2)
      }
    }

    it("parses real arrays") {
      new ScalaStan {
        val data = "one\n1.5\n2.5\n"
        val src = CsvDataSource.fromString(data)
        val n = data(int())
        val decl = data(real()(n))

        src.read(decl, "one") shouldBe Seq(1.5, 2.5)
      }
    }

    it("returns column names") {
      val data = "one,two\n1,2\n"
      val src = CsvDataSource.fromString(data)

      src.names shouldBe Set("one", "two")
    }

    it("returns row counts") {
      val data = "one\n1\n2\n3\n"
      val src = CsvDataSource.fromString(data)

      src.dims("one") shouldBe Seq(3)
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

    it("can read raw string values") {
      val data = "one\na\nb"
      val src = CsvDataSource.fromString(data)

      src.readRaw("one") shouldBe Vector("a", "b")
    }
  }
}
