/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

// StanNode is the base class for elements in the DSL.
// It represents code that can be emitted.
abstract class StanNode {

  // Generate the Stan representation for this node (without a terminating ";") .
  private[scalastan] def emit: String

  // The terminator for this statement (overridden to omit).
  private[scalastan] val terminator: String = ";"
}

// Base class for elements that create a new scope.
abstract class EnterScope extends StanNode {
  override private[scalastan] val terminator: String = ""
}

// "for" loop
case class ForLoop[T <: StanType] private[scalastan] (
  private val decl: StanLocalDeclaration[T],
  private val range: ValueRange
) extends EnterScope {
  private[scalastan] def emit: String = s"for(${decl.emit} in ${range.emit}) {"
}

// "while" loop
case class WhileLoop private[scalastan] (
  private val cond: StanValue[StanInt]
) extends EnterScope {
  private[scalastan] def emit: String = s"while(${cond.emit}) {"
}

// "if" (or "when") statement
case class IfStatement[T <: StanType] private[scalastan] (
  private val cond: StanValue[T]
) extends EnterScope {
  private[scalastan] def emit: String = s"if(${cond.emit}) {"
}

// "else if" (or ".when") statement
case class ElseIfStatement[T <: StanType] private[scalastan] (
  private val cond: StanValue[T]
) extends EnterScope {
  private[scalastan] def emit: String = s"else if(${cond.emit}) {"
}

// "else" (or "otherwise") statement
case class ElseStatement private[scalastan] () extends EnterScope {
  private[scalastan] def emit: String = s"else {"
}

// The end of a scope.
case class LeaveScope private[scalastan] () extends StanNode {
  private[scalastan] def emit: String = "}"
  override private[scalastan] val terminator: String = ""
}

// "break" statement
case class BreakNode private[scalastan] () extends StanNode {
  private[scalastan] def emit: String = "break"
}

// "continue" statement
case class ContinueNode private[scalastan] () extends StanNode {
  private[scalastan] def emit: String = "continue"
}

// Sample from a distribution: "var ~ dist()"
case class SampleNode[T <: StanType] private[scalastan] (
  private val left: StanValue[T],
  private val right: StanDistribution[T]
) extends StanNode {
  private[scalastan] def emit: String = s"${left.emit} ~ ${right.emit}"
}

// A distribution (Normal, etc.)
abstract class StanDistribution[T <: StanType] extends StanNode {
  protected val name: String
  protected val args: Seq[StanValue[_]]
  protected val lowerOpt: Option[StanValue[_]]
  protected val upperOpt: Option[StanValue[_]]

  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    val truncateStr = (lowerOpt, upperOpt) match {
      case (Some(lower), Some(upper)) => s" T[${lower.emit},${upper.emit}]"
      case (Some(lower), None)        => s" T[${lower.emit},]"
      case (None, Some(upper))        => s" T[,${upper.emit}]"
      case (None, None)               => ""
    }
    s"$name($argStr)$truncateStr"
  }
}

case class StanContinuousDistribution[T <: StanType, R <: StanType] private[scalastan] (
  protected val name: String,
  protected val args: Seq[StanValue[_]],
  protected val lowerOpt: Option[StanValue[R]] = None,
  protected val upperOpt: Option[StanValue[R]] = None
) extends StanDistribution[T] {
  def lpdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lpdf", y, "|", args)
  def cdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: Option[StanValue[R]] = None,
    upper: Option[StanValue[R]] = None
  ): StanContinuousDistribution[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    StanContinuousDistribution(name, args, lowerOpt = lower, upperOpt = upper)
  }
  def rng(implicit gen: InGeneratedQuantityBlock): FunctionNode[R] = FunctionNode(s"${name}_rng", args: _*)
}

abstract class StanDiscreteDistribution[T <: StanType, R <: StanType] extends StanDistribution[T] {
  def lpmf(
    y: StanValue[T]
  ): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lpmf", y, "|", args)
  def rng(implicit gen: InGeneratedQuantityBlock): FunctionNode[R] = FunctionNode(s"${name}_rng", args: _*)
}

case class StanDiscreteDistributionWithoutCdf[T <: StanType, R <: StanType] private[scalastan] (
  protected val name: String,
  protected val args: Seq[StanValue[_]],
  protected val lowerOpt: Option[StanValue[R]] = None,
  protected val upperOpt: Option[StanValue[R]] = None
) extends StanDiscreteDistribution[T, R] {
  def truncate(
    lower: Option[StanValue[R]] = None,
    upper: Option[StanValue[R]] = None
  ): StanDiscreteDistributionWithoutCdf[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    StanDiscreteDistributionWithoutCdf(name, args, lowerOpt = lower, upperOpt = upper)
  }
}

case class StanDiscreteDistributionWithCdf[T <: StanType, R <: StanType] private[scalastan] (
  protected val name: String,
  protected val args: Seq[StanValue[_]],
  protected val lowerOpt: Option[StanValue[T]] = None,
  protected val upperOpt: Option[StanValue[T]] = None
) extends StanDiscreteDistribution[T, R] {
  def cdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = DistributionFunctionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: Option[StanValue[T]] = None,
    upper: Option[StanValue[T]] = None
  ): StanDiscreteDistributionWithCdf[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    StanDiscreteDistributionWithCdf(name, args, lowerOpt = lower, upperOpt = upper)
  }
}

// A return (or output) statement.
case class ReturnNode[T <: StanType](
  protected val result: StanValue[T]
) extends StanNode {
  private[scalastan] def emit: String = s"return ${result.emit}"
}

// A range (x:n in Stan), used with for loops.
case class ValueRange(
  private[scalastan] val start: StanValue[StanInt],
  private[scalastan] val end: StanValue[StanInt]
)(implicit ss: ScalaStan) extends StanNode {

  // This foreach will get called automatically when a for comprehension is used with ValueRange.
  def foreach(f: StanValue[StanInt] => Unit)(implicit ev: TemporaryValue[StanInt], code: ArrayBuffer[StanNode]): Unit = {
    val temp = ev.create()
    val decl = StanLocalDeclaration[StanInt](temp)
    code += ForLoop(decl, this)
    f(decl)
    code += LeaveScope()
  }

  private[scalastan] def emit: String = s"${start.emit}:${end.emit}"
}

