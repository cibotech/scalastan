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
import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.ast.{StanBlock, StanForLoop, StanIfStatement, StanStatement}

// Simplify the AST (remove empty blocks, etc.).
case class AstSimplifier()(implicit val ss: ScalaStan) extends StanTransform[Unit] {

  def initialState: Unit = ()

  private def isEmpty(s: StanStatement): Boolean = s match {
    case b: StanBlock => b.children.isEmpty
    case _            => false
  }

  override def handleBlock(b: StanBlock): State[StanStatement] = for {
    newChildren <- State.sequence(b.children)(dispatch)
  } yield if (newChildren.size == 1) newChildren.head else StanBlock(newChildren)

  override def handleIf(i: StanIfStatement): State[StanStatement] = for {
    conds <- State.sequence(i.conds)(c => dispatch(c._2).map(x => c._1 -> x))
    newConds = conds.filterNot(s => isEmpty(s._2))
    otherwise <- dispatchOption(i.otherwise)
    newOtherwise = otherwise.flatMap(o => if (isEmpty(o)) None else Some(o))
  } yield {
    if (newConds.nonEmpty || newOtherwise.isDefined) {
      StanIfStatement(newConds, newOtherwise)
    } else {
      StanBlock(Seq.empty)
    }
  }

  override def handleFor(f: StanForLoop): State[StanStatement] = for {
    newBody <- dispatch(f.body)
  } yield {
    if (isEmpty(newBody)) {
      newBody
    } else {
      f.copy(body = newBody)
    }
  }
}
