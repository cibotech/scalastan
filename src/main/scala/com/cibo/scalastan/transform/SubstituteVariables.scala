/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.{StanContext, StanType}
import com.cibo.scalastan.ast.{StanDeclaration, StanStatement, StanValue}

case class SubstituteVariables(
  substitutions: Map[(Int, StanDeclaration[_]), StanDeclaration[_]]
) extends StanTransform[Int] {

  override def initialState: Int = -1

  override def dispatch(statement: StanStatement)(implicit context: StanContext): State[StanStatement] = for {
    _ <- State.put(statement.id)
    newStatement <- super.dispatch(statement)
  } yield newStatement

  override def handleVariable[T <: StanType](
    decl: StanDeclaration[T]
  )(implicit context: StanContext): State[StanValue[T]] = for {
    currentStatement <- State.get
  } yield substitutions.getOrElse((currentStatement, decl), decl).asInstanceOf[StanValue[T]]
}
