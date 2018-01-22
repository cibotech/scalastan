package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, StanType}
import com.cibo.scalastan.ast.{StanDeclaration, StanStatement, StanValue}

case class SubstituteVariables(
  substitutions: Map[(Int, StanDeclaration[_]), StanDeclaration[_]]
)(
  implicit ss: ScalaStan
) extends StanTransform {

  private var currentStatement: Int = -1

  override protected def dispatch(statement: StanStatement): StanStatement = {
    currentStatement = statement.id
    super.dispatch(statement)
  }

  override protected def handleVariable[T <: StanType](decl: StanDeclaration[T]): StanValue[T] = {
    substitutions.getOrElse((currentStatement, decl), decl).asInstanceOf[StanValue[T]]
  }

}
