package com.cibo.scalastan.transform

import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.ast._

// Propagate copies to eliminate unnecessary temporaries.
case class CopyPropagation()(implicit val ss: ScalaStan) extends StanTransform {

  private var root: Option[StanStatement] = None
  private var substitutions: Map[(Int, StanDeclaration[_]), StanDeclaration[_]] = Map.empty

  override def run(program: StanProgram): StanProgram = {
    val updated = super.run(program)
    RemoveUnusedDecls().run(SubstituteVariables(substitutions).run(updated))
  }

  override protected def handleRoot(r: StanStatement): StanStatement = {
    root = Some(r)
    dispatch(r)
  }

  override protected def handleAssignment(a: StanAssignment): StanStatement = (a.lhs, a.rhs) match {
    case (lhs: StanDeclaration[_], rhs: StanDeclaration[_]) if a.op == StanAssignment.Assign =>

      // This assignment is a candidate for removal by replacing the LHS with the RHS in the program.
      // We need to check that:
      //  - All reaching defs of the LHS reference only this assignment.
      //  - The reaching defs of the RHS are the same for all reaching defs of the LHS.
      // Assuming this is true, we remove this assignment and substitute the RHS where the LHS is used.

      // Look up uses of the LHS to be replaced.
      val statements = StanProgram.getStatements(root.get)
      val useDefinitions = UseDefinitions(root.get)
      val uses = useDefinitions.du.getOrElse(a.id, Set.empty)
      val lhsUses = statements.filter(s => uses.contains(s.id))

      // Check that all reaching defs of the LHS reference only this assignment.
      val reachingDefs = ReachingDefs(root.get).solve
      val refsOk = lhsUses.forall { other =>
        reachingDefs.lookup(other).filter(_.decl == lhs.id).forall(_.statement == a.id)
      }

      // Check that the reaching defs of the RHS are the same for all reaching defs of the LHS.
      val rhsDefs = reachingDefs.lookup(a).filter(_.decl == rhs.id).map(_.statement)
      val rhsOk = lhsUses.forall { other =>
        reachingDefs.lookup(other).filter(_.decl == rhs.id).map(_.statement) == rhsDefs
      }

      if (refsOk && rhsOk && uses.nonEmpty) {
        lhsUses.foreach { use =>
          substitutions += ((use.id, lhs) -> rhs)
        }
        StanBlock(Seq.empty)
      } else {
        a
      }
    case _                        => a
  }
}
