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

import com.cibo.scalastan._

// A distribution (Normal, etc.)
abstract class StanDistribution[T <: StanType, R <: StanType] extends StanNode {
  protected val name: String
  protected val args: Seq[StanValue[_]]
  protected val lowerOpt: Option[StanValue[_]]
  protected val upperOpt: Option[StanValue[_]]

  private[scalastan] def inputs: Seq[StanDeclaration[_]] =
    args.flatMap(_.inputs) ++ lowerOpt.toSeq.flatMap(_.inputs) ++ upperOpt.toSeq.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] =
    args.flatMap(_.outputs) ++ lowerOpt.toSeq.flatMap(_.outputs) ++ upperOpt.toSeq.flatMap(_.outputs)

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
) extends StanDistribution[T, R] {
  def lpdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lpdf", y, "|", args)
  def cdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: Option[StanValue[R]] = None,
    upper: Option[StanValue[R]] = None
  ): StanContinuousDistribution[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    StanContinuousDistribution(name, args, lowerOpt = lower, upperOpt = upper)
  }
  def rng(implicit gen: InGeneratedQuantityBlock): StanCall[R] = StanCall(s"${name}_rng", args: _*)
}

abstract class StanDiscreteDistribution[T <: StanType, R <: StanType] extends StanDistribution[T, R] {
  def lpmf(
    y: StanValue[T]
  ): StanValue[StanReal] = StanDistributionNode(s"${name}_lpmf", y, "|", args)
  def rng(implicit gen: InGeneratedQuantityBlock): StanCall[R] = StanCall(s"${name}_rng", args: _*)
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
  def cdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: Option[StanValue[T]] = None,
    upper: Option[StanValue[T]] = None
  ): StanDiscreteDistributionWithCdf[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    StanDiscreteDistributionWithCdf(name, args, lowerOpt = lower, upperOpt = upper)
  }
}


