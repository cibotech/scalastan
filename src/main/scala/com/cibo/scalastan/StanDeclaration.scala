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
)(implicit ss: ScalaStan) extends StanDeclaration[T] with ReadOnlyIndex[T]

case class StanParameterDeclaration[T <: StanType] private[scalastan] (
  private[scalastan] val typeConstructor: T,
  protected val internalNameFunc: () => Option[String] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  private[scalastan] val typeConstructor: T,
  protected val internalNameFunc: () => Option[String] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanInlineDeclaration[T <: StanType](
  protected val decl: StanLocalDeclaration[T]
) extends StanValue[T] {
  private[scalastan] def emit: String = decl.emitDeclaration
}


