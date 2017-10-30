package com.cibo.scalastan.data

import com.cibo.scalastan._

case class CsvDataSource private (values: Seq[Map[String, String]]) extends DataSource {
  def columns: Set[String] = values.head.keySet
  def rows: Int = values.size

  def readMatrix(columns: Seq[String]): Vector[Vector[Double]] = {
    Vector.tabulate[Vector[Double]](values.size) { i =>
      Vector.tabulate[Double](columns.size) { j =>
        values(i)(columns(j)).toDouble
      }
    }
  }

  def readVector(column: String): Vector[Double] = {
    Vector.tabulate[Double](values.size) { i => values(i)(column).toDouble }
  }

  def read[T <: StanType, R](
    decl: StanDataDeclaration[T],
    name: String
  )(implicit ev: R =:= T#SCALA_TYPE): R = {
    decl.typeConstructor match {
      case v: StanVectorLike => values.map(_.apply(name).toDouble).asInstanceOf[R]
      case a: StanArray[_]   =>
        a.inner match {
          case i: StanInt  => values.map(_.apply(name).toInt).asInstanceOf[R]
          case d: StanReal => values.map(_.apply(name).toDouble).asInstanceOf[R]
          case _           =>
            throw new IllegalStateException(s"unsupported type for CsvDataSource: ${decl.typeConstructor}")
        }
      case _                 =>
        throw new IllegalStateException(s"unsupported type for CsvDataSource: ${decl.typeConstructor}")
    }
  }
}

object CsvDataSource {
  def fromString(content: String, separator: Char = ','): CsvDataSource = {
    val lines = content.split('\n')
    val header = lines.head.split(separator).map(_.trim).map { str =>
      val updated = if (str.head == '\"') str.tail else str
      if (updated.last == '\"') updated.dropRight(1) else updated
    }
    CsvDataSource(lines.tail.map(line => header.zip(line.split(',')).toMap))
  }

  def fromFile(fileName: String, separator: Char = ','): CsvDataSource = {
    val content = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
    fromString(content, separator)
  }
}
