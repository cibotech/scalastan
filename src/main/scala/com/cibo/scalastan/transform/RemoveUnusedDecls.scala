package com.cibo.scalastan.transform

import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.ast.{StanBlock, StanInlineDeclaration, StanProgram, StanStatement}

// Remove unused inline declarations.
case class RemoveUnusedDecls()(implicit ss: ScalaStan) extends StanTransform {

  private var usedDecls: Set[Int] = Set.empty

  override protected def handleRoot(root: StanStatement): StanStatement = {
    usedDecls = StanProgram.getStatements(root).flatMap { st =>
      st.inputs.map(_.id) ++ st.outputs.map(_.id)
    }.toSet
    super.handleRoot(root)
  }

  override protected def handleDecl(d: StanInlineDeclaration): StanStatement = {
    if (usedDecls.contains(d.decl.id)) d else StanBlock()
  }
}
