package com.cibo.scalastan

trait StanDeclaration[T <: StanType] extends StanValue[T] {
  val typeConstructor: T
  private val name: String = StanDeclaration.getName
  def emit: String = name
  def emitDeclaration: String = typeConstructor.emitDeclaration(name)

}

object StanDeclaration {
  private var counter: Int = 0

  private def getName: String = {
    counter += 1
    s"v$counter"
  }
}

case class StanDataDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T
) extends StanDeclaration[T] with ReadOnlyIndex[T]

case class StanParameterDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T
) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanLocalDeclaration[T <: StanType] private[scalastan] (
  typeConstructor: T
) extends StanDeclaration[T] with Assignable[T] with Updatable[T]

case class StanInlineDeclaration[T <: StanType](
  decl: StanLocalDeclaration[T]
) extends StanValue[T] {
  def emit: String = decl.emitDeclaration
}


