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
sealed abstract class StanDistribution[T <: StanType, R <: StanType] extends StanNode {
  protected val name: String
  val args: Seq[StanValue[_ <: StanType]]
  protected val lowerOpt: Option[StanValue[_ <: StanType]]
  protected val upperOpt: Option[StanValue[_ <: StanType]]

  def inputs: Seq[StanDeclaration[_ <: StanType]] =
    args.flatMap(_.inputs) ++ lowerOpt.toSeq.flatMap(_.inputs) ++ upperOpt.toSeq.flatMap(_.inputs)
  def outputs: Seq[StanDeclaration[_ <: StanType]] =
    args.flatMap(_.outputs) ++ lowerOpt.toSeq.flatMap(_.outputs) ++ upperOpt.toSeq.flatMap(_.outputs)
  def values: Seq[StanValue[_ <: StanType]] = args ++ lowerOpt.toSeq ++ upperOpt.toSeq

  private[scalastan] def export(builder: StanProgramBuilder): Unit = {
    args.foreach(_.export(builder))
    lowerOpt.foreach(_.export(builder))
    upperOpt.foreach(_.export(builder))
  }

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

case class StanContinuousDistribution[T <: StanType, R <: StanType](
  name: String,
  rngType: R,
  args: Seq[StanValue[_ <: StanType]],
  lowerOpt: Option[StanValue[StanReal]] = None,
  upperOpt: Option[StanValue[StanReal]] = None,
  id: Int = StanNode.getNextId
) extends StanDistribution[T, R] {
  def lpdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lpdf", y, "|", args)
  def cdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: StanValue[StanReal] = StanUnknownReal,
    upper: StanValue[StanReal] = StanUnknownReal
  ): StanContinuousDistribution[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    copy(lowerOpt = StanUnknown.boundOpt(lower), upperOpt = StanUnknown.boundOpt(upper))
  }
  def rng(implicit gen: RngAvailable): StanCall[R] = StanCall(rngType, s"${name}_rng", args)
}

sealed abstract class StanDiscreteDistribution[T <: StanType, R <: StanType] extends StanDistribution[T, R] {
  val rngType: R
  def lpmf(
    y: StanValue[T]
  ): StanValue[StanReal] = StanDistributionNode(s"${name}_lpmf", y, "|", args)

  def rng(implicit gen: RngAvailable): StanCall[R] = StanCall(rngType, s"${name}_rng", args)

}

case class StanDiscreteDistributionWithoutCdf[T <: StanType, R <: StanType] private[scalastan] (
  name: String,
  rngType: R,
  args: Seq[StanValue[_ <: StanType]],
  lowerOpt: Option[StanValue[StanInt]] = None,
  upperOpt: Option[StanValue[StanInt]] = None,
  id: Int = StanNode.getNextId
) extends StanDiscreteDistribution[T, R] {
  def truncate(
    lower: StanValue[StanInt] = StanUnknownInt,
    upper: StanValue[StanInt] = StanUnknownInt
  ): StanDiscreteDistributionWithoutCdf[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    copy(lowerOpt = StanUnknown.boundOpt(lower), upperOpt = StanUnknown.boundOpt(upper))
  }
}

case class StanDiscreteDistributionWithCdf[T <: StanType, R <: StanType] private[scalastan] (
  name: String,
  rngType: R,
  args: Seq[StanValue[_ <: StanType]],
  lowerOpt: Option[StanValue[StanInt]] = None,
  upperOpt: Option[StanValue[StanInt]] = None,
  id: Int = StanNode.getNextId
) extends StanDiscreteDistribution[T, R] {
  def cdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_cdf", y, ",", args)
  def lcdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lcdf", y, "|", args)
  def lccdf(y: StanValue[T]): StanValue[StanReal] = StanDistributionNode(s"${name}_lccdf", y, "|", args)
  def truncate(
    lower: StanValue[StanInt] = StanUnknownInt,
    upper: StanValue[StanInt] = StanUnknownInt
  ): StanDiscreteDistributionWithCdf[T, R] = {
    require(lowerOpt.isEmpty && upperOpt.isEmpty, "Distribution already truncated")
    copy(lowerOpt = StanUnknown.boundOpt(lower), upperOpt = StanUnknown.boundOpt(upper))
  }
}


