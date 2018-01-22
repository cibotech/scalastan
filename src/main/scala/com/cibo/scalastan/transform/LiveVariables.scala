package com.cibo.scalastan.transform

import com.cibo.scalastan.ast.StanStatement

case class LiveVariables(_root: StanStatement) extends StanAnalysis[Int](_root) {
  protected val forward: Boolean = false

  protected def init(node: StanStatement): Set[Int] = Set.empty

  protected def gen(node: StanStatement, in: Set[Int]): Set[Int] = node.inputs.map(_.id).toSet

  protected def kill(node: StanStatement, in: Set[Int]): Set[Int] = node.outputs.map(_.id).toSet

  protected def meet(a: Set[Int], b: Set[Int]): Set[Int] = a.union(b)
}
