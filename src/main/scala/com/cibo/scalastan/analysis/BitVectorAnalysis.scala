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

abstract class BitVectorAnalysis[T](_root: StanStatement) extends StanAnalysis[Set[T]](_root) {

  val forward: Boolean

  def init(node: StanStatement): Set[T]
  def gen(node: StanStatement, in: Set[T]): Set[T]
  def kill(node: StanStatement, in: Set[T]): Set[T]

  def transfer(node: StanStatement, in: Set[T]): Set[T] = (in -- kill(node, in)) ++ gen(node, in)

}
