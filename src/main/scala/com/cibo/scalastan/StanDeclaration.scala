package com.cibo.scalastan

sealed abstract class StanDeclaration[T <: StanType](implicit ss: ScalaStan) extends StanValue[T] with NameLookup {
  val typeConstructor: T

  protected val internalNameFunc: Function0[Option[String]]
  protected def _userName: Option[String] = internalNameFunc().orElse(NameLookup.lookupName(this))

  def emit: String = _internalName
  def emitDeclaration: String = typeConstructor.emitDeclaration(_internalName)
}

case class StanDataDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with ReadOnlyIndex[T]

case class StanParameterDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanInlineDeclaration[T <: StanType](
  decl: StanLocalDeclaration[T]
) extends StanValue[T] {
  def emit: String = decl.emitDeclaration
}


