package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

abstract class StanNode {
  def emit: String
  val terminator: String = ";"
}

abstract class EnterScope extends StanNode {
  override val terminator: String = ""
}

case class ForLoop[T <: StanType](
  decl: StanDeclaration[T, LocalDeclarationType],
  range: ValueRange
) extends EnterScope {
  def emit: String = s"for(${decl.emit} in ${range.emit}) {"
}

case class IfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"if(${cond.emit}) {"
}

case class ElseIfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"else if(${cond.emit}) {"
}

case object ElseStatement extends EnterScope {
  def emit: String = s"else {"
}

case object LeaveScope extends StanNode {
  def emit: String = "}"
  override val terminator: String = ""
}

case class SampleNode[T <: StanType](
  left: StanValue[T],
  right: StanDistribution[T]
) extends StanNode {
  def emit: String = s"${left.emit} ~ ${right.emit}"
}

case class StanDistribution[T <: StanType](
  name: String,
  args: StanValue[_]*
) extends StanNode {
  def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name($argStr)"
  }
}

case class ReturnNode[T <: StanType](
  result: StanValue[T]
) extends StanNode {
  def emit: String = s"return ${result.emit}"
}

case class ValueRange(start: StanValue[StanInt], end: StanValue[StanInt]) extends StanNode {

  def foreach(f: StanValue[StanInt] => Unit)(implicit ev: TemporaryValue[StanInt], code: ArrayBuffer[StanNode]): Unit = {
    val temp = ev.create()
    val decl = StanDeclaration[StanInt, LocalDeclarationType](temp)
    code += ForLoop(decl, this)
    f(decl)
    code += LeaveScope
  }

  def emit: String = s"${start.emit}:${end.emit}"
}

