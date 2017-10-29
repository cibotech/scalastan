package com.cibo.scalastan.data

import com.cibo.scalastan._

object CsvDataSource {
  private case class CsvDataSource(values: Seq[Map[String, String]]) extends DataSource {
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

  def fromString(content: String, separator: Char = ','): DataSource = {
    val lines = content.split('\n')
    val header = lines.head.split(separator).map(_.trim).map { str =>
      val updated = if (str.head == '\"') str.tail else str
      if (updated.last == '\"') updated.dropRight(1) else updated
    }
    CsvDataSource(lines.tail.map(line => header.zip(line.split(',')).toMap))
  }

  def fromFile(fileName: String, separator: Char = ','): DataSource = {
    val content = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
    fromString(content, separator)
  }
}
