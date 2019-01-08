/*
 * Copyright (c) 2017 - 2019 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast.{StanProgram, StanStatement}

case class UseDefinitions(root: StanStatement) {

  private lazy val reaching = ReachingDefs(root).solve

  private var useDefs: Map[Int, Set[Int]] = Map.empty   // variable usage -> set of defs
  private var defUses: Map[Int, Set[Int]] = Map.empty   // variable def -> set of usages

  private def addDefs(statement: StanStatement): Unit = {
    val defs = reaching.lookup(statement)
    val u = statement.id
    statement.inputs.foreach { input =>
      defs.filter(_.decl == input.id).foreach { rd =>
        val d = rd.statement
        useDefs = useDefs.updated(u, useDefs.getOrElse(u, Set.empty) + d)
        defUses = defUses.updated(d, defUses.getOrElse(d, Set.empty) + u)
      }
    }
  }

  StanProgram.getStatements(root).foreach(addDefs)

  def ud: Map[Int, Set[Int]] = useDefs
  def du: Map[Int, Set[Int]] = defUses
}
