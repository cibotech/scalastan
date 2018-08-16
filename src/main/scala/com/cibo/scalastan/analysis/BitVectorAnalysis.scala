package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast.StanStatement

abstract class BitVectorAnalysis[T](_root: StanStatement) extends StanAnalysis[Set[T]](_root) {

  val forward: Boolean

  def init(node: StanStatement): Set[T]
  def gen(node: StanStatement, in: Set[T]): Set[T]
  def kill(node: StanStatement, in: Set[T]): Set[T]

  def transfer(node: StanStatement, in: Set[T]): Set[T] = (in -- kill(node, in)) ++ gen(node, in)

}
