package com.cibo.scalastan.transform

import com.cibo.scalastan.ast._

protected case class ReachingDef(decl: Int, statement: Int)

case class ReachingDefs(_root: StanStatement) extends StanAnalysis[ReachingDef](_root) {

  protected val forward: Boolean = true

  protected def init(node: StanStatement): Set[ReachingDef] = node.inputs.filter { i =>
    // Parameters and data are special, so we assume they come from the declaration.
    i.isInstanceOf[StanDataDeclaration[_]] || i.isInstanceOf[StanParameterDeclaration[_]]
  }.map(d => ReachingDef(d.id, d.id)).toSet

  protected def gen(node: StanStatement, in: Set[ReachingDef]): Set[ReachingDef] =
    node.outputs.map(d => ReachingDef(d.id, node.id)).toSet

  protected def kill(node: StanStatement, in: Set[ReachingDef]): Set[ReachingDef] = node match {
    case a: StanAssignment =>
      // Kill if the LHS is a matching decl.
      // It may also be an index, in which case we leave the reaching definition.
      a.lhs match {
        case decl: StanDeclaration[_] => in.filter(_.decl == decl.id)
        case _                        => Set.empty
      }
    case _                 => Set.empty
  }

  protected def meet(a: Set[ReachingDef], b: Set[ReachingDef]): Set[ReachingDef] = a.union(b)

}
