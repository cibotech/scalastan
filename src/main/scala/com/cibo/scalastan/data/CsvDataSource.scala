package com.cibo.scalastan.data

import java.io.{BufferedReader, FileReader}
import scala.collection.JavaConverters._
import com.cibo.scalastan.{StanDataDeclaration, StanType}

case class CsvDataSource(fileName: String) extends DataSource {

  private lazy val data: Seq[Map[String, String]] = {
    val reader = new BufferedReader(new FileReader(fileName))
    try {
      val lines = reader.lines.iterator.asScala.filterNot(_.startsWith("#")).toVector
      val header: Seq[String] = lines.head.split(",")
      lines.tail.map { line: String =>
        header.zip(line.split(",")).toMap
      }
    } finally {
      reader.close()
    }
  }

  def read[T <: StanType, R](
    decl: StanDataDeclaration[T],
    name: String
  )(implicit ev: =:=[R, T#SCALA_TYPE]): R = {
    ???
  }

}
