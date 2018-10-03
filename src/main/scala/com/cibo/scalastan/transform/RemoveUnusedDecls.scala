/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.StanContext
import com.cibo.scalastan.ast.{StanBlock, StanInlineDeclaration, StanProgram, StanStatement}

// Remove unused inline declarations.
case class RemoveUnusedDecls() extends StanTransform[Set[Int]] {

  def initialState: Set[Int] = Set.empty

  private def createState(root: StanStatement): Set[Int] = {
    StanProgram.getStatements(root).flatMap { st =>
      st.inputs.map(_.id) ++ st.outputs.map(_.id)
    }.toSet
  }

  override def handleRoot(root: StanStatement)(implicit context: StanContext): State[StanStatement] = for {
    _ <- State.put(createState(root))
    newRoot <- super.handleRoot(root)
  } yield newRoot

  override def handleDecl(d: StanInlineDeclaration)(implicit context: StanContext): State[StanStatement] = for {
    usedDecls <- State.get
  } yield if (usedDecls.contains(d.decl.id)) d else StanBlock()
}
