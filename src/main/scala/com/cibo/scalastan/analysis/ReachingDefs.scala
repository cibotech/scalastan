/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast._

protected case class ReachingDef(decl: Int, statement: Int)

case class ReachingDefs(_root: StanStatement) extends BitVectorAnalysis[ReachingDef](_root) {

  val forward: Boolean = true

  def init(node: StanStatement): Set[ReachingDef] = node.inputs.filter { i =>
    // Parameters and data are special, so we assume they come from the declaration.
    i.isInstanceOf[StanDataDeclaration[_]] || i.isInstanceOf[StanParameterDeclaration[_]]
  }.map(d => ReachingDef(d.id, d.id)).toSet

  def gen(node: StanStatement, in: Set[ReachingDef]): Set[ReachingDef] =
    node.outputs.map(d => ReachingDef(d.id, node.id)).toSet

  def kill(node: StanStatement, in: Set[ReachingDef]): Set[ReachingDef] = node match {
    case a: StanAssignment =>
      // Kill if the LHS is a matching decl.
      // It may also be an index, in which case we leave the reaching definition.
      a.lhs match {
        case decl: StanDeclaration[_] => in.filter(_.decl == decl.id)
        case _                        => Set.empty
      }
    case _                 => Set.empty
  }

  def meet(a: Set[ReachingDef], b: Set[ReachingDef]): Set[ReachingDef] = a.union(b)
}
