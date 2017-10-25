package com.cibo.scalastan.data

import com.cibo.scalastan.{StanDataDeclaration, StanType}

object CsvDataSource {
  private case class CsvDataSource(values: Seq[Map[String, String]]) extends DataSource {
    def read[T <: StanType, R](
      decl: StanDataDeclaration[T],
      name: String
    )(implicit ev: R =:= T#SCALA_TYPE): R = {
      values.map(_.apply(name).toDouble).asInstanceOf[R]
    }
  }

  def fromString(content: String): DataSource = {
    val lines = content.split("\n")
    val header = lines.head.split(",").map(_.trim).map { str =>
      val updated = if (str.head == '\"') str.tail else str
      if (updated.last == '\"') updated.dropRight(1) else updated
    }
    CsvDataSource(lines.tail.map(line => header.zip(line.split(",")).toMap))
  }

  def fromFile(fileName: String): DataSource = {
    val content = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
    fromString(content)
  }
}
