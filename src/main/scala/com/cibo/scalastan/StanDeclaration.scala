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

sealed abstract class StanDeclaration[T <: StanType](implicit ss: ScalaStan) extends StanValue[T] with NameLookup {
  private[scalastan] val typeConstructor: T

  protected val internalNameFunc: Function0[Option[String]]
  protected def _userName: Option[String] = internalNameFunc().orElse(NameLookup.lookupName(this))
  protected val _ss: ScalaStan = ss

  private[scalastan] def emit: String = _internalName
  private[scalastan] def emitDeclaration: String = typeConstructor.emitDeclaration(_internalName)

  def size(implicit ev: T <:< StanCompoundType): StanValue[StanInt] = dims.head

  def range(implicit ev: T <:< StanCompoundType): ValueRange = ValueRange(1, size)

  def dims: Seq[StanValue[StanInt]] = typeConstructor.getIndices
}

case class StanDataDeclaration[T <: StanType] private[scalastan] (
  private[scalastan] val typeConstructor: T,
  protected val internalNameFunc: () => Option[String] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with ReadOnlyIndex[T] {
  type DECL_TYPE = StanDataDeclaration[T]
}

case class StanParameterDeclaration[T <: StanType] private[scalastan] (
  private[scalastan] val typeConstructor: T,
  protected val internalNameFunc: () => Option[String] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T] {
  type DECL_TYPE = StanParameterDeclaration[T]
}

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  private[scalastan] val typeConstructor: T,
  protected val internalNameFunc: () => Option[String] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T] {
  type DECL_TYPE = StanLocalDeclaration[T]
}

case class StanInlineDeclaration[T <: StanType](
  protected val decl: StanLocalDeclaration[T]
) extends StanValue[T] {
  private[scalastan] def emit: String = decl.emitDeclaration
}


