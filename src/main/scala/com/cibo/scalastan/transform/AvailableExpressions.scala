package com.cibo.scalastan.transform
import com.cibo.scalastan.ast.{StanAssignment, StanDeclaration, StanStatement}

case class AvailableExpressions(_root: StanStatement) extends StanAnalysis[Int](_root) {

  protected val forward: Boolean = true

  // Start with no available expressions.
  protected def init(node: StanStatement): Set[Int] = Set.empty

  // Each new (complete) assignment introduces an available expression.
  protected def gen(node: StanStatement, in: Set[Int]): Set[Int] = node match {
    case a: StanAssignment if a.lhs.isInstanceOf[StanDeclaration[_]]    => Set(a.id)
    case _                                                              => Set.empty
  }

  // We need to remove expressions if we update an input.
  protected def kill(node: StanStatement, in: Set[Int]): Set[Int] = {
    val outputSet = node.outputs.map(_.id).toSet
    in.filter { i =>
      outputSet.intersect(statementMap(i).inputs.map(_.id).toSet).nonEmpty
    }
  }

  // An expression is available if it's available from all incoming control flows.
  protected def meet(a: Set[Int], b: Set[Int]): Set[Int] = a.intersect(b)
}
