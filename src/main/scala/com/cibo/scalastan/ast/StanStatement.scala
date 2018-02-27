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

import java.io.PrintWriter
import scala.language.existentials

import com.cibo.scalastan.{StanInt, StanType}

// A statement in a Stan program.
sealed abstract class StanStatement extends StanNode {
  private[scalastan] def inputs: Seq[StanDeclaration[_]]
  private[scalastan] def outputs: Seq[StanDeclaration[_]]
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit
  protected def indented(indent: Int, str: String): String = "  " * indent + str
  protected def write(pw: PrintWriter, indent: Int, line: String): Unit = {
    pw.println(indented(indent, line))
  }
}

// Container for Stan statements (a basic block).
case class StanBlock private[scalastan] (
  private[scalastan] val children: Seq[StanStatement]
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    children.foreach { child => child.emit(pw, indent) }
  }
}

// Container for a base expression (primarily for void function calls).
case class StanValueStatement(
  private[scalastan] val expr: StanValue[_ <: StanType]
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = expr.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = expr.outputs
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"${expr.emit};")
  }
}

// Assignment.
case class StanAssignment private[scalastan] (
  private[scalastan] val lhs: StanValue[_ <: StanType],
  private[scalastan] val rhs: StanValue[_ <: StanType],
  private[scalastan] val op: StanAssignment.Operator = StanAssignment.Assign
) extends StanStatement {

  private def assignedValue(v: StanValue[_]): StanDeclaration[_ <: StanType] = v match {
    case d: StanDeclaration[_] => d
    case a: Assignable[_]      => assignedValue(a.value)
    case _                     => throw new IllegalStateException(s"invalid assigned value: $v")
  }

  private[scalastan] def inputs: Seq[StanDeclaration[_]] = lhs.inputs ++ rhs.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq(assignedValue(lhs))
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"${lhs.emit} ${op.name} ${rhs.emit};")
  }
}

object StanAssignment {
  sealed abstract class Operator(val name: String)
  case object Assign extends Operator("=")
  case object Add extends Operator("+=")
  case object Subtract extends Operator("-=")
  case object Multiply extends Operator("*=")
  case object Divide extends Operator("/=")
}

// "for" loop
case class StanForLoop private[scalastan] (
  private[scalastan] val decl: StanLocalDeclaration[StanInt],
  private[scalastan] val range: StanValueRange,
  private[scalastan] val body: StanStatement
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = decl +: range.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = decl +: range.outputs
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"for(${decl.emit} in ${range.emit}) {")
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "while" loop
case class StanWhileLoop private[scalastan] (
  private[scalastan] val cond: StanValue[StanInt],
  private[scalastan] val body: StanStatement
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = cond.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = cond.outputs
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"while(${cond.emit}) {")
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "if" (or "when") statement
case class StanIfStatement private[scalastan] (
  private[scalastan] val conds: Seq[(StanValue[StanInt], StanStatement)],
  private[scalastan] val otherwise: Option[StanStatement]
) extends StanStatement {
  require(conds.nonEmpty)
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = conds.flatMap(_._1.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = conds.flatMap(_._1.outputs)
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"if(${conds.head._1.emit}) {")
    conds.head._2.emit(pw, indent + 1)
    conds.tail.foreach { case (cond, body) =>
      write(pw, indent, s"} else if(${cond.emit}) {")
      body.emit(pw, indent + 1)
    }
    otherwise.foreach { o =>
      write(pw, indent, "} else {")
      o.emit(pw, indent + 1)
    }
    write(pw, indent, "}")
  }
}

// "break" statement
case class StanBreakStatement private[scalastan] () extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "break;")
}

// "continue" statement
case class StanContinueStatement private[scalastan] () extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "continue;")
}

// Sample from a distribution: "var ~ dist()"
case class StanSampleStatement[T <: StanType, R <: StanType] private[scalastan] (
  private val left: StanValue[T],
  private val right: StanDistribution[T, R]
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = left.inputs ++ right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = left.outputs ++ right.outputs
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"${left.emit} ~ ${right.emit};")
}

// A return (or output) statement.
case class StanReturnStatement private[scalastan] (
  protected val result: StanValue[_ <: StanType]
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = result.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = result.outputs
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"return ${result.emit};")
}

// Local variable declaration.
case class StanInlineDeclaration private[scalastan] (
  protected val decl: StanLocalDeclaration[_ <: StanType]
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"${decl.emitDeclaration};")
  private[scalastan] def isDerivedFromData: Boolean = false
}
