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

sealed abstract class StanDeclaration[T <: StanType] extends StanValue[T] {
  val returnType: T
  val name: String

  def emit: String = name
  def emitDeclaration: String = returnType.emitDeclaration(name)
  def emitFunctionDeclaration: String = returnType.emitFunctionDeclaration(name)

  def size(implicit ev: T <:< StanCompoundType): StanValue[StanInt] = dims.head

  def range(implicit ev: T <:< StanCompoundType, ss: ScalaStan): StanValueRange = StanValueRange(1, size)

  def dims: Seq[StanValue[StanInt]] = returnType.getIndices

  def children: Seq[StanValue[_ <: StanType]] = Seq.empty
}

case class StanDataDeclaration[T <: StanType](
  returnType: T,
  name: String,
  id: Int = StanNode.getNextId
) extends StanDeclaration[T] {
  require(returnType.isDerivedFromData,
    "data declaration bounds must be derived from other data declarations or constant")
  type DECL_TYPE = StanDataDeclaration[T]
  def isDerivedFromData: Boolean = true
  def export(builder: CodeBuilder): Unit = {
    returnType.lower.foreach(_.export(builder))
    returnType.upper.foreach(_.export(builder))
    returnType.getIndices.foreach(_.export(builder))
    builder.append(this)
  }
  def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq(this)
  def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
}

case class StanParameterDeclaration[T <: StanType](
  returnType: T,
  name: String,
  rootOpt: Option[StanParameterDeclaration[_ <: StanType]] = None,
  indices: Seq[Int] = Seq.empty,
  owner: Option[ScalaStan#TransformBase[_, _]] = None,
  id: Int = StanNode.getNextId
) extends StanDeclaration[T] with Updatable[T] {
  require(returnType.isDerivedFromData,
    "parameter declaration bounds must be derived from data declarations or constant")
  val value: StanDeclaration[_ <: StanType] = this
  type DECL_TYPE = StanParameterDeclaration[T]
  def isDerivedFromData: Boolean = false
  def export(builder: CodeBuilder): Unit = {
    indices.foreach(_.export(builder))
    returnType.lower.foreach(_.export(builder))
    returnType.upper.foreach(_.export(builder))
    returnType.getIndices.foreach(_.export(builder))
    owner match {
      case Some(code) => code.export(builder)
      case None       => builder.append(this)
    }
  }
  override def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq(this)
  override def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq(this)

  private[scalastan] def root: StanParameterDeclaration[_ <: StanType] = rootOpt.getOrElse(this)

  def get(index: Int)(implicit ev: IsCompoundType[T]): StanParameterDeclaration[T#NEXT_TYPE] = {
    val newName = s"$name[$index]"
    StanParameterDeclaration(returnType.next, newName, Some(root), indices :+ index)
  }

  def get(
    index1: Int,
    index2: Int
  )(implicit ev: IsCompoundType[T#NEXT_TYPE]): StanParameterDeclaration[T#NEXT_TYPE#NEXT_TYPE] = {
    val args = Seq(index1, index2)
    val newName = args.mkString(s"$name[", ",", "]")
    StanParameterDeclaration(returnType.next.next, newName, Some(root), indices ++ args)
  }

  def get(
    index1: Int, index2: Int, index3: Int
  )(implicit ev: IsCompoundType[T#NEXT_TYPE#NEXT_TYPE]): StanParameterDeclaration[T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    val args = Seq(index1, index2, index3)
    val newName = args.mkString(s"$name[", ",", "]")
    StanParameterDeclaration(returnType.next.next.next, newName, Some(root), indices ++ args)
  }

  def get(
    index1: Int, index2: Int, index3: Int, index4: Int
  )(
    implicit ev: IsCompoundType[T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE]
  ): StanParameterDeclaration[T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    val args = Seq(index1, index2, index3, index4)
    val newName = args.mkString(s"$name[", ",", "]")
    StanParameterDeclaration(returnType.next.next.next.next, newName, Some(root), indices ++ args)
  }
}

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  returnType: T,
  name: String,
  derivedFromData: Boolean = false,
  owner: Option[ScalaStan#TransformBase[_, _]] = None,
  id: Int = StanNode.getNextId
) extends StanDeclaration[T] with Updatable[T] {
  val value: StanValue[_ <: StanType] = this
  type DECL_TYPE = StanLocalDeclaration[T]
  def isDerivedFromData: Boolean = derivedFromData
  def export(builder: CodeBuilder): Unit = {
    returnType.lower.foreach(_.export(builder))
    returnType.upper.foreach(_.export(builder))
    returnType.getIndices.foreach(_.export(builder))
    owner match {
      case Some(code) => code.export(builder)
      case None       => ()
    }
  }
  override def emitDeclaration: String = returnType.unconstrained.emitDeclaration(name)
  override def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq(this)
  override def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq(this)
}
