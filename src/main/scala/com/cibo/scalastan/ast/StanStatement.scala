/*
 * Copyright (c) 2017 - 2019 CiBO Technologies - All Rights Reserved
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
import com.cibo.scalastan.{StanProgramBuilder, StanInt, StanType}

// A statement in a Stan program.
sealed abstract class StanStatement extends StanNode {
  def inputs: Seq[StanDeclaration[_ <: StanType]]
  def outputs: Seq[StanDeclaration[_ <: StanType]]
  def values: Seq[StanValue[_ <: StanType]]
  def children: Seq[StanStatement]
  def export(builder: StanProgramBuilder): Unit
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit
  def emit(pw: PrintWriter, indent: Int): Unit
  protected final def indented(indent: Int, str: String): String = "  " * indent + str
  protected final def write(pw: PrintWriter, indent: Int, line: String): Unit = {
    pw.println(indented(indent, line))
  }
}

// Container for Stan statements (a basic block).
case class StanBlock(
  children: Seq[StanStatement] = Seq.empty,
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq.empty
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = {
    children.foreach(_.emitDeclarations(pw, indent))
  }
  def export(builder: StanProgramBuilder): Unit = children.foreach(_.export(builder))
  def emit(pw: PrintWriter, indent: Int): Unit = {
    children.foreach(_.emit(pw, indent))
  }
}

// Container for a base expression (primarily for void function calls).
case class StanValueStatement(
  expr: StanValue[_ <: StanType],
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = expr.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq(expr)
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = expr.export(builder)
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"${expr.emit};")
  }
}

// Assignment.
case class StanAssignment(
  lhs: StanValue[_ <: StanType],
  rhs: StanValue[_ <: StanType],
  op: StanAssignment.Operator = StanAssignment.Assign,
  id: Int = StanNode.getNextId
) extends StanStatement {

  private def assignedValue(v: StanValue[_ <: StanType]): Option[StanDeclaration[_ <: StanType]] = v match {
    case d: StanDeclaration[_]         => Some(d)
    case i: StanIndexOperator[_, _, _] => assignedValue(i.value)
    case s: StanSliceOperator[_, _]    => assignedValue(s.value)
    case _                             => None
  }

  private def assignedInputs(v: StanValue[_ <: StanType]): Seq[StanDeclaration[_ <: StanType]] = {
    val updatedValue = if (op == StanAssignment.Assign) None else assignedValue(v)
    v match {
      case i: StanIndexOperator[_, _, _] => i.inputs ++ updatedValue.toSeq
      case s: StanSliceOperator[_, _]    => s.inputs ++ updatedValue.toSeq
      case _                             => updatedValue.toSeq
    }
  }

  def inputs: Seq[StanDeclaration[_ <: StanType]] = assignedInputs(lhs) ++ rhs.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = assignedValue(lhs).toSeq
  def values: Seq[StanValue[_ <: StanType]] = Seq(lhs, rhs)
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = {
    lhs.export(builder)
    rhs.export(builder)
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = {
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
  val body: StanStatement
}

// "for" loop
case class StanForLoop(
  decl: StanLocalDeclaration[StanInt],
  range: StanValueRange,
  body: StanStatement,
  id: Int = StanNode.getNextId
) extends StanLoop {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = range.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq(decl)
  def values: Seq[StanValue[_ <: StanType]] = Seq(range.start, range.end)
  def children: Seq[StanStatement] = Seq(body)
  def export(builder: StanProgramBuilder): Unit = {
    decl.export(builder)
    range.export(builder)
    body.export(builder)
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"for(${decl.emit} in ${range.emit}) {")
    body.emitDeclarations(pw, indent + 1)
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "while" loop
case class StanWhileLoop(
  cond: StanValue[StanInt],
  body: StanStatement,
  id: Int = StanNode.getNextId
) extends StanLoop {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = cond.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq(cond)
  def children: Seq[StanStatement] = Seq(body)
  def export(builder: StanProgramBuilder): Unit = {
    cond.export(builder)
    body.export(builder)
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = {
    write(pw, indent, s"while(${cond.emit}) {")
    body.emitDeclarations(pw, indent + 1)
    body.emit(pw, indent + 1)
    write(pw, indent, "}")
  }
}

// "if" (or "when") statement
case class StanIfStatement(
  conds: Seq[(StanValue[StanInt], StanStatement)],
  otherwise: Option[StanStatement],
  id: Int = StanNode.getNextId
) extends StanStatement {
  require(conds.nonEmpty)
  def inputs: Seq[StanDeclaration[_ <: StanType]] = conds.flatMap(_._1.inputs)
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = conds.map(_._1)
  def children: Seq[StanStatement] = conds.map(_._2) ++ otherwise.toSeq
  def export(builder: StanProgramBuilder): Unit = {
    conds.foreach { case (v, s) =>
      v.export(builder)
      s.export(builder)
    }
    otherwise.foreach(_.export(builder))
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = {
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
case class StanBreakStatement(
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq.empty
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = ()
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "break;")
}

// "continue" statement
case class StanContinueStatement(
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq.empty
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = ()
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, "continue;")
}

// Sample from a distribution: "var ~ dist()"
case class StanSampleStatement[T <: StanType, SUPPORT <: StanType](
  left: StanValue[T],
  right: StanDistribution[T, SUPPORT],
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = left.inputs ++ right.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = left +: right.values
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = {
    right.export(builder)
    left.export(builder)
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"${left.emit} ~ ${right.emit};")
}

// A return (or output) statement.
case class StanReturnStatement(
  result: StanValue[_ <: StanType],
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = result.inputs
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  def children: Seq[StanStatement] = Seq.empty
  def values: Seq[StanValue[_ <: StanType]] = Seq(result)
  def export(builder: StanProgramBuilder): Unit = result.export(builder)
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = ()
  def emit(pw: PrintWriter, indent: Int): Unit = write(pw, indent, s"return ${result.emit};")
}

// Local variable declaration.
case class StanInlineDeclaration(
  decl: StanLocalDeclaration[_ <: StanType],
  initialValue: Option[StanValue[_ <: StanType]] = None,
  id: Int = StanNode.getNextId
) extends StanStatement {
  def inputs: Seq[StanDeclaration[_ <: StanType]] = initialValue.toSeq.flatMap(_.inputs)
  def outputs: Seq[StanDeclaration[_ <: StanType]] = initialValue.toSeq.flatMap(_.outputs)
  def values: Seq[StanValue[_ <: StanType]] = initialValue.toSeq
  def children: Seq[StanStatement] = Seq.empty
  def export(builder: StanProgramBuilder): Unit = {
    initialValue.foreach(_.export(builder))
    decl.export(builder)
  }
  def emitDeclarations(pw: PrintWriter, indent: Int): Unit = {
    initialValue match {
      case Some(v) => write(pw, indent, s"${decl.emitDeclaration} = ${v.emit};")
      case None    => write(pw, indent, s"${decl.emitDeclaration};")
    }
  }
  def emit(pw: PrintWriter, indent: Int): Unit = ()
  def isDerivedFromData: Boolean = false
}
