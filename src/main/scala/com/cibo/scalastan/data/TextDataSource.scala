/*
 * Copyright (c) 2017 - 2021 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.data

object TextDataSource {
  def fromString(content: String): DataSource = {
    val lines = content.split('\n').map(_.split(' ').toVector).toVector.filter(_.nonEmpty)
    val value = if (lines.length == 1 && lines.head.length == 1) {
      // Scalar
      DataValue("", Vector.empty, lines.flatten)
    } else if (lines.length == 1 || lines.head.length == 1) {
      // Vector
      DataValue("", Vector(math.max(lines.length, lines.head.length)), lines.flatten)
    } else {
      // Matrix
      DataValue("", Vector(lines.length, lines.head.length), lines.flatten)
    }
    DataSource(Seq(value))
  }

  def fromFile(fileName: String): DataSource = {
    fromString(scala.io.Source.fromFile(fileName).getLines.mkString("\n"))
  }
}
