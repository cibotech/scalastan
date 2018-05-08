/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast.StanStatement

case class LiveVariables(_root: StanStatement) extends StanAnalysis[Int](_root) {
  protected val forward: Boolean = false

  protected def init(node: StanStatement): Set[Int] = Set.empty

  protected def gen(node: StanStatement, in: Set[Int]): Set[Int] = node.inputs.map(_.id).toSet

  protected def kill(node: StanStatement, in: Set[Int]): Set[Int] = node.outputs.map(_.id).toSet

  protected def meet(a: Set[Int], b: Set[Int]): Set[Int] = a.union(b)
}
