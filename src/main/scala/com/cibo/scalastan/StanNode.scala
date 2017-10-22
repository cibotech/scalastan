package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

// StanNode is the base class for elements in the DSL.
// It represents code that can be emitted.
abstract class StanNode {

  // Generate the Stan representation for this node (without a terminating ";") .
  def emit: String

  // The terminator for this statement (overridden to omit).
  val terminator: String = ";"
}

// Base class for elements that create a new scope.
abstract class EnterScope extends StanNode {
  override val terminator: String = ""
}

// "for" loop
case class ForLoop[T <: StanType](
  decl: StanLocalDeclaration[T],
  range: ValueRange
) extends EnterScope {
  def emit: String = s"for(${decl.emit} in ${range.emit}) {"
}

// "while" loop
case class WhileLoop(
  cond: StanValue[StanInt]
) extends EnterScope {
  def emit: String = s"while(${cond.emit}) {"
}

// "if" (or "when") statement
case class IfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"if(${cond.emit}) {"
}

// "else if" (or ".when") statement
case class ElseIfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"else if(${cond.emit}) {"
}

// "else" (or "otherwise") statement
case object ElseStatement extends EnterScope {
  def emit: String = s"else {"
}

// The end of a scope.
case class LeaveScope() extends StanNode {
  def emit: String = "}"
  override val terminator: String = ""
}

// Sample from a distribution: "var ~ dist()"
case class SampleNode[T <: StanType](
  left: StanValue[T],
  right: StanDistribution[T]
) extends StanNode {
  def emit: String = s"${left.emit} ~ ${right.emit}"
}

// A distribution (Normal, etc.)
abstract class StanDistribution[T <: StanType] extends StanNode {
  val name: String
  val args: Seq[StanValue[_]]
  def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name($argStr)"
  }

  def rng(implicit gen: InGeneratedQuantityBlock): FunctionNode[T] = FunctionNode(s"${name}_rng", args: _*)
}

case class StanContinuousDistribution[T <: StanType](
  name: String,
  args: StanValue[_]*
) extends StanDistribution[T] {
  def lpdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lpdf", y, "|", args)
  def cdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lccdf", y, "|", args)
}

abstract class StanDiscreteDistribution[T <: StanType] extends StanDistribution[T] {
  def lpmf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lpmf", y, "|", args)
}

case class StanDiscreteDistributionWithoutCdf[T <: StanType](
  name: String,
  args: StanValue[_]*
) extends StanDistribution[T]

case class StanDiscreteDistributionWithCdf[T <: StanType](
  name: String,
  args: StanValue[_]*
) extends StanDiscreteDistribution[T] {
  def cdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[T] = DistributionFunctionNode(s"${name}_lccdf", y, "|", args)
}

// A return (or output) statement.
case class ReturnNode[T <: StanType](
  result: StanValue[T]
) extends StanNode {
  def emit: String = s"return ${result.emit}"
}

// A range (x:n in Stan), used with for loops.
case class ValueRange(
  start: StanValue[StanInt],
  end: StanValue[StanInt]
)(implicit ss: ScalaStan) extends StanNode {

  // This foreach will get called automatically when a for comprehension is used with ValueRange.
  def foreach(f: StanValue[StanInt] => Unit)(implicit ev: TemporaryValue[StanInt], code: ArrayBuffer[StanNode]): Unit = {
    val temp = ev.create()
    val decl = StanLocalDeclaration[StanInt](temp)
    code += ForLoop(decl, this)
    f(decl)
    code += LeaveScope()
  }

  def emit: String = s"${start.emit}:${end.emit}"
}

