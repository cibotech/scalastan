/*
 * Copyright (c) 2017 - 2020 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.data
import kantan.csv._
import kantan.csv.ops._

object CsvDataSource {

  private def strip(str: String): String = {
    val str1 = str.trim
    val str2 = if (str1.nonEmpty && str1.head == '\"') str1.tail else str1
    val str3 = if (str2.nonEmpty && str2.last == '\"') str2.dropRight(1) else str2
    str3.trim
  }

  def fromString(content: String, separator: Char = ','): DataSource = {
    val config = rfc.copy(cellSeparator = separator)
    val lines = content.asUnsafeCsvReader[Vector[String]](config).toSeq
    val header = lines.head.map(strip)
    val body = lines.tail.toVector
    val len = body.length
    val values = header.zipWithIndex.map { case (name, index) =>
       DataValue(name, Vector(len), body.map(_.apply(index)))
    }
    DataSource(values)
  }

  def fromFile(fileName: String, separator: Char = ','): DataSource = {
    val content = scala.io.Source.fromFile(fileName).getLines.mkString("\n")
    fromString(content, separator)
  }
}
