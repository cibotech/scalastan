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
import com.cibo.scalastan.ast.{StanBlock, StanForLoop, StanIfStatement, StanStatement}

// Simplify the AST (remove empty blocks, etc.).
object AstSimplifier extends StanTransform {
  type STATE = Unit
  protected val initialState: Unit = ()

  private def isEmpty(s: StanStatement): Boolean = s match {
    case b: StanBlock => b.children.isEmpty
    case _            => false
  }

  override protected def handleBlock(b: StanBlock, state: Unit): StanStatement = {
    val newChildren = b.children.map(c => dispatch(c, state))
    if (newChildren.size == 1) newChildren.head else StanBlock(newChildren)
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
