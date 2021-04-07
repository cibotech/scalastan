/*
 * Copyright (c) 2017 - 2021 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast.{StanAssignment, StanDeclaration, StanStatement}

case class AvailableExpressions(_root: StanStatement) extends BitVectorAnalysis[Int](_root) {

  val forward: Boolean = true

  // Start with no available expressions.
  def init(node: StanStatement): Set[Int] = Set.empty

  // Each new (complete) assignment introduces an available expression.
  def gen(node: StanStatement, in: Set[Int]): Set[Int] = node match {
    case a: StanAssignment if a.lhs.isInstanceOf[StanDeclaration[_]]    => Set(a.id)
    case _                                                              => Set.empty
  }

  // We need to remove expressions if we update an input.
  def kill(node: StanStatement, in: Set[Int]): Set[Int] = {
    val outputSet = node.outputs.map(_.id).toSet
    in.filter { i =>
      outputSet.intersect(statementMap(i).inputs.map(_.id).toSet).nonEmpty
    }
  }

  // An expression is available if it's available from all incoming control flows.
  def meet(a: Set[Int], b: Set[Int]): Set[Int] = a.intersect(b)
}
