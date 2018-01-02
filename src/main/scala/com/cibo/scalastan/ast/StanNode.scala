/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.ast

import scala.language.existentials
import java.io.PrintWriter

import com.cibo.scalastan._

// StanNode is the base class for elements in the DSL (statements, values, distributions, etc).
abstract class StanNode {
  private[scalastan] lazy val id = StanNode.getNextId
}

object StanNode {
  private var nextId: Int = 0

  private def getNextId: Int = {
    synchronized {
      val id = nextId
      nextId += 1
      id
    }
  }
}

// A range (x:n in Stan), used with for loops.
case class StanValueRange(
  private[scalastan] val start: StanValue[StanInt],
  private[scalastan] val end: StanValue[StanInt]
)(implicit ss: ScalaStan) extends StanNode {

  private[scalastan] def inputs: Seq[StanDeclaration[_]] = start.inputs ++ end.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = start.outputs ++ end.outputs

  // This foreach will get called automatically when a for comprehension is used with ValueRange.
  def foreach(f: StanValue[StanInt] => Unit)(implicit ev: TemporaryValue[StanInt], builder: CodeBuilder): Unit = {
    val temp = ev.create()
    val decl = StanLocalDeclaration[StanInt](temp)
    builder.enter()
    f(decl)
    builder.leave(children => StanForLoop(decl, this, StanBlock(children)))
  }

  private[scalastan] def emit: String = s"${start.emit}:${end.emit}"
}

case class StanFunctionDeclaration private[scalastan] (
  returnValue: StanLocalDeclaration[_ <: StanType],
  inputs: Seq[StanLocalDeclaration[_ <: StanType]],
  code: StanStatement
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
    val params = inputs.map(_.emitFunctionDeclaration).mkString(",")
    writer.println(s"  ${returnValue.typeConstructor.emitFunctionDeclaration} ${returnValue.emit}($params) {")
    code.emit(writer, 2)
    writer.println("  }")
  }
}

case class StanTransformedParameter private[scalastan] (
  result: StanParameterDeclaration[_ <: StanType],
  code: StanStatement
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = code.emit(writer, 1)
}

case class StanTransformedData private[scalastan] (
  result: StanLocalDeclaration[_ <: StanType],
  code: StanStatement
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = code.emit(writer, 1)
}

case class StanGeneratedQuantity private[scalastan] (
  result: StanParameterDeclaration[_ <: StanType],
  code: StanStatement
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = code.emit(writer, 1)
}
