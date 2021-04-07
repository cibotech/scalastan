/*
 * Copyright (c) 2017 - 2021 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.{StanContext, StanType}
import com.cibo.scalastan.analysis.{ReachingDefs, UseDefinitions}
import com.cibo.scalastan.ast._

case class CopyPropagationState(
  root: Option[StanStatement] = None,
  substitutions: Map[(Int, StanDeclaration[_]), StanDeclaration[_]] = Map.empty
)

// Propagate copies to eliminate unnecessary temporaries.
case class CopyPropagation() extends StanTransform[CopyPropagationState] {

  def initialState: CopyPropagationState = CopyPropagationState()

  override def handleProgram(program: StanProgram)(implicit context: StanContext): State[StanProgram] = {
    for {
      newProgram <- super.handleProgram(program)
      substitutions <- State.get.map(_.substitutions)
    } yield RemoveUnusedDecls().run(SubstituteVariables(substitutions).run(newProgram))
  }

  override def handleRoot(r: StanStatement)(implicit context: StanContext): State[StanStatement] = for {
    _ <- State.modify(_.copy(root = Some(r)))
    newRoot <- dispatch(r)
  } yield newRoot

  private def updateSubtitutions(
    shouldUpdate: Boolean,
    uses: Seq[StanStatement],
    lhs: StanDeclaration[_ <: StanType],
    rhs: StanDeclaration[_ <: StanType]
  ): State[Unit] = {
    if (shouldUpdate) {
      val newMappings = uses.map { use => (use.id, lhs) -> rhs }
      State.modify(s => s.copy(substitutions = s.substitutions ++ newMappings))
    } else {
      State.pure(())
    }
  }

  override def handleAssignment(
    a: StanAssignment
  )(implicit context: StanContext): State[StanStatement] = (a.lhs, a.rhs) match {
    case (lhs: StanDeclaration[_], rhs: StanDeclaration[_]) if a.op == StanAssignment.Assign =>

      // This assignment is a candidate for removal by replacing the LHS with the RHS in the program.
      // We need to check that:
      //  - All reaching defs of the LHS reference only this assignment.
      //  - The reaching defs of the RHS are the same for all reaching defs of the LHS.
      // Assuming this is true, we remove this assignment and substitute the RHS where the LHS is used.

      // Look up uses of the LHS to be replaced.
      for {
        state <- State.get
        statements = StanProgram.getStatements(state.root.get)
        useDefinitions = UseDefinitions(state.root.get)
        uses = useDefinitions.du.getOrElse(a.id, Set.empty)
        lhsUses = statements.filter(s => uses.contains(s.id))

        // Check that all reaching defs of the LHS reference only this assignment.
        reachingDefs = ReachingDefs(state.root.get).solve
        refsOk = lhsUses.forall { other =>
          reachingDefs.lookup(other).filter(_.decl == lhs.id).forall(_.statement == a.id)
        }

        // Check that the reaching defs of the RHS are the same for all reaching defs of the LHS.
        rhsDefs = reachingDefs.lookup(a).filter(_.decl == rhs.id).map(_.statement)
        rhsOk = lhsUses.forall { other =>
          reachingDefs.lookup(other).filter(_.decl == rhs.id).map(_.statement) == rhsDefs
        }

        shouldUpdate = refsOk && rhsOk && uses.nonEmpty
         _ <- updateSubtitutions(shouldUpdate, lhsUses, lhs, rhs)

      } yield if (shouldUpdate) StanBlock(Seq.empty) else a
    case _                        => State.pure(a)
  }
}
