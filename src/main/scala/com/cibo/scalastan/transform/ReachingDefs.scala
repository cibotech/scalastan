package com.cibo.scalastan.transform

import com.cibo.scalastan.ast.{StanAssignment, StanDeclaration, StanStatement}

class ReachingDefs extends StanAnalysis {

  case class ReachingDef(decl: Int, statement: Int)

  type T = ReachingDef

  protected val forward: Boolean = true

  protected def init(node: StanStatement): Set[T] = Set.empty

  protected def gen(node: StanStatement, in: Set[T]): Set[T] = node.outputs.map(d => ReachingDef(d.id, node.id)).toSet

  protected def kill(node: StanStatement, in: Set[T]): Set[T] = node match {
    case a: StanAssignment =>
      // Kill if the RHS is a matching decl.
      // It may also be an index, in which case we leave the reaching definition.
      a.rhs match {
        case decl: StanDeclaration[_] => in.filter(_.decl == decl.id)
        case _                        => Set.empty
      }
    case _                 => Set.empty
  }

  protected def meet(a: Set[T], b: Set[T]): Set[T] = a.union(b)

}
