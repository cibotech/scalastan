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
  val id: Int
}

object StanNode {
  private var nextId: Int = 0

  private[scalastan] def getNextId: Int = {
    synchronized {
      val id = nextId
      nextId += 1
      id
    }
  }
}

// A range (x:n in Stan), used with for loops.
case class StanValueRange(
  start: StanValue[StanInt],
  end: StanValue[StanInt],
  id: Int = StanNode.getNextId
)(implicit context: StanContext) extends StanNode {

  def inputs: Seq[StanDeclaration[_ <: StanType]] = start.inputs ++ end.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty

  private[scalastan] def export(builder: StanProgramBuilder): Unit = {
    start.export(builder)
    end.export(builder)
  }

  // This foreach will get called automatically when a for comprehension is used with ValueRange.
  def foreach(f: StanValue[StanInt] => Unit)(implicit builder: StanProgramBuilder): Unit = {
    val decl = StanLocalDeclaration[StanInt](StanInt(), context.newName)
    builder.enter()
    f(decl)
    builder.leave(children => StanForLoop(decl, this, StanBlock(children)))
  }

  private[scalastan] def emit: String = s"${start.emit}:${end.emit}"
}

case class StanFunctionDeclaration private[scalastan] (
  returnValue: StanLocalDeclaration[_ <: StanType],
  inputs: Seq[StanLocalDeclaration[_ <: StanType]],
  code: StanStatement,
  id: Int = StanNode.getNextId
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
    val params = inputs.map(_.emitFunctionDeclaration).mkString(",")
    writer.println(s"  ${returnValue.returnType.emitFunctionDeclaration} ${returnValue.emit}($params) {")
    code.emitDeclarations(writer, 2)
    code.emit(writer, 2)
    writer.println("  }")
  }
}

case class StanTransformedParameter private[scalastan] (
  result: StanParameterDeclaration[_ <: StanType],
  code: StanStatement,
  id: Int = StanNode.getNextId
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
    writer.println("  {")
    code.emitDeclarations(writer, 2)
    code.emit(writer, 2)
    writer.println("  }")
  }
}

case class StanTransformedData private[scalastan] (
  result: StanLocalDeclaration[_ <: StanType],
  code: StanStatement,
  id: Int = StanNode.getNextId
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
    writer.println("  {")
    code.emitDeclarations(writer, 2)
    code.emit(writer, 2)
    writer.println("  }")
  }
}

case class StanGeneratedQuantity private[scalastan] (
  result: StanParameterDeclaration[_ <: StanType],
  code: StanStatement,
  id: Int = StanNode.getNextId
) extends StanNode {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
    writer.println("  {")
    code.emitDeclarations(writer, 2)
    code.emit(writer, 2)
    writer.println("  }")
  }
}
