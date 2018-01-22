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
  private[scalastan] def children: Seq[StanStatement]
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit
  protected final def indented(indent: Int, str: String): String = "  " * indent + str
  protected final def write(pw: PrintWriter, indent: Int, line: String): Unit = {
    pw.println(indented(indent, s"$line // $id"))
  }
}

// Container for Stan statements (a basic block).
case class StanBlock private[scalastan] (
  children: Seq[StanStatement] = Seq.empty,
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = {
    children.foreach(_.emitDeclarations(pw, indent))
  }
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    children.foreach(_.emit(pw, indent))
  }
}

// Container for a base expression (primarily for void function calls).
case class StanValueStatement(
  expr: StanValue[_ <: StanType],
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = expr.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"${expr.emit};")
  }
}

// Assignment.
case class StanAssignment private[scalastan] (
  lhs: StanValue[_ <: StanType],
  rhs: StanValue[_ <: StanType],
  op: StanAssignment.Operator = StanAssignment.Assign,
  id: Int = StanNode.getNextId
) extends StanStatement {

  private def assignedValue(v: StanValue[_ <: StanType]): Option[StanDeclaration[_]] = v match {
    case d: StanDeclaration[_]         => Some(d)
    case i: StanIndexOperator[_, _, _] => assignedValue(i.value)
    case s: StanSliceOperator[_, _]    => assignedValue(s.value)
    case _                             => None
  }

  private def assignedInputs(v: StanValue[_ <: StanType]): Seq[StanDeclaration[_]] = {
    val updatedValue = if (op == StanAssignment.Assign) None else assignedValue(v)
    v match {
      case i: StanIndexOperator[_, _, _] => i.inputs ++ updatedValue.toSeq
      case s: StanSliceOperator[_, _]    => s.inputs ++ updatedValue.toSeq
      case _                             => updatedValue.toSeq
    }
  }

  private[scalastan] def inputs: Seq[StanDeclaration[_]] = assignedInputs(lhs) ++ rhs.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = assignedValue(lhs).toSeq
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
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

sealed abstract class StanLoop extends StanStatement {
  private[scalastan] val body: StanStatement
}

// "for" loop
case class StanForLoop private[scalastan] (
  decl: StanLocalDeclaration[StanInt],
  range: StanValueRange,
  body: StanStatement,
  id: Int = StanNode.getNextId
) extends StanLoop {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = range.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq(decl)
  private[scalastan] def children: Seq[StanStatement] = Seq(body)
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"for(${decl.emit} in ${range.emit}) {")
    body.emitDeclarations(pw, indent + 1)
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "while" loop
case class StanWhileLoop private[scalastan] (
  cond: StanValue[StanInt],
  body: StanStatement,
  id: Int = StanNode.getNextId
) extends StanLoop {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = cond.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq(body)
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"while(${cond.emit}) {")
    body.emitDeclarations(pw, indent + 1)
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "if" (or "when") statement
case class StanIfStatement private[scalastan] (
  conds: Seq[(StanValue[StanInt], StanStatement)],
  otherwise: Option[StanStatement],
  id: Int = StanNode.getNextId
) extends StanStatement {
  require(conds.nonEmpty)
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = conds.flatMap(_._1.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = conds.map(_._2) ++ otherwise.toSeq
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"if(${conds.head._1.emit}) {")
    conds.head._2.emitDeclarations(pw, indent + 1)
    conds.head._2.emit(pw, indent + 1)
    conds.tail.foreach { case (cond, body) =>
      write(pw, indent, s"} else if(${cond.emit}) {")
      body.emitDeclarations(pw, indent + 1)
      body.emit(pw, indent + 1)
    }
    otherwise.foreach { o =>
      write(pw, indent, "} else {")
      o.emitDeclarations(pw, indent + 1)
      o.emit(pw, indent + 1)
    }
    write(pw, indent, "}")
  }
}

// "break" statement
case class StanBreakStatement private[scalastan] (
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "break;")
}

// "continue" statement
case class StanContinueStatement private[scalastan] (
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "continue;")
}

// Sample from a distribution: "var ~ dist()"
case class StanSampleStatement[T <: StanType, R <: StanType] private[scalastan] (
  private val left: StanValue[T],
  private val right: StanDistribution[T],
  protected val id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = left.inputs ++ right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"${left.emit} ~ ${right.emit};")
}

// A return (or output) statement.
case class StanReturnStatement private[scalastan] (
  result: StanValue[_ <: StanType],
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = result.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"return ${result.emit};")
}

// Local variable declaration.
case class StanInlineDeclaration private[scalastan] (
  decl: StanLocalDeclaration[_ <: StanType],
  id: Int = StanNode.getNextId
) extends StanStatement {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def children: Seq[StanStatement] = Seq.empty
  private[scalastan] def emitDeclarations(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"${decl.emitDeclaration};")
  }
  private[scalastan] def emit(pw: PrintWriter, indent: Int): Unit = ()
  private[scalastan] def isDerivedFromData: Boolean = false
}
