package com.cibo.scalastan

import scala.reflect.{ClassTag, classTag}

sealed abstract class StanDeclaration[T <: StanType](implicit ss: ScalaStan) extends StanValue[T] with NameLookup {
  val typeConstructor: T

  protected val internalNameFunc: Function0[Option[String]]
  private[scalastan] lazy val name: String = internalNameFunc().getOrElse(NameLookup.lookupName(this))

  def emit: String = _internalName
  def emitDeclaration: String = typeConstructor.emitDeclaration(_internalName)
}

case class StanDataDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with ReadOnlyIndex[T] {
  protected val _ctag: ClassTag[_] = classTag[StanDataDeclaration[T]]
}

case class StanParameterDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T] {
  protected val _ctag: ClassTag[_] = classTag[StanParameterDeclaration[T]]
}

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T,
  protected val internalNameFunc: Function0[Option[String]] = () => None
)(implicit ss: ScalaStan) extends StanDeclaration[T] with Assignable[T] with Updatable[T] {
  protected val _ctag: ClassTag[_] = classTag[StanLocalDeclaration[T]]
}

case class StanInlineDeclaration[T <: StanType](
  decl: StanLocalDeclaration[T]
) extends StanValue[T] {
  def emit: String = decl.emitDeclaration
}


