package com.cibo.scalastan.transform
import com.cibo.scalastan.ast.{StanBlock, StanForLoop, StanIfStatement, StanStatement}

// Simplify the AST (remove empty blocks, etc.).
object AstSimplifier extends StanTransform[Unit] {
  protected val initialState: Unit = ()

  private def isEmpty(s: StanStatement): Boolean = s match {
    case b: StanBlock => b.children.isEmpty
    case _            => false
  }

  override protected def handleBlock(b: StanBlock, state: Unit): StanStatement = {
    if (b.children.size == 1) b.children.head else b
  }

  override protected def handleIf(i: StanIfStatement, state: Unit): StanStatement = {
    val newConds = i.conds.map { case (cond, body) =>
      cond -> dispatch(body, state)
    }.filterNot(s => isEmpty(s._2))
    val newOtherwise = i.otherwise.map(o => dispatch(o, state)).filterNot(isEmpty)
    if (newConds.nonEmpty || newOtherwise.isDefined) {
      StanIfStatement(newConds, newOtherwise)
    } else {
      StanBlock(Seq.empty)
    }
  }

  override protected def handleFor(f: StanForLoop, state: Unit): StanStatement = {
    val newBody = dispatch(f.body, state)
    if (isEmpty(newBody)) {
      newBody
    } else {
      f.copy(body = newBody)
    }
  }
}
