package com.cibo.scalastan

sealed trait DeclarationType
final class DataDeclarationType extends DeclarationType
final class ParameterDeclarationType extends DeclarationType
final class LocalDeclarationType extends DeclarationType

case class StanDeclaration[T <: StanType, D <: DeclarationType] private[scalastan] (
  typeConstructor: T
) extends StanValue[T] {
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

case class StanInlineDeclaration[T <: StanType](
  decl: StanDeclaration[T, LocalDeclarationType]
) extends StanValue[T] {
  def emit: String = decl.emitDeclaration
}


