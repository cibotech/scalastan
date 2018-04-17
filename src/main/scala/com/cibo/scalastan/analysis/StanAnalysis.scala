package com.cibo.scalastan.analysis

import com.cibo.scalastan.ast._

case class AnalysisResult[T](mapping: Map[Int, Set[T]]) {
  def lookup(node: StanNode): Set[T] = mapping(node.id)
}

abstract class StanAnalysis[T](root: StanStatement) {

  // True if this if the analysis should go forward, false for backward.
  protected val forward: Boolean

  protected def init(node: StanStatement): Set[T]
  protected def gen(node: StanStatement, in: Set[T]): Set[T]
  protected def kill(node: StanStatement, in: Set[T]): Set[T]
  protected def meet(a: Set[T], b: Set[T]): Set[T]

  private def transfer(node: StanStatement, in: Set[T]): Set[T] = (in -- kill(node, in)) ++ gen(node, in)

  private def addLink(src: StanStatement, dest: StanStatement, links: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    links.updated(src.id, links.getOrElse(src.id, Set.empty) + dest.id)
  }

  // Determine if this statement is a jump or if it will fall through to the next statement.
  private def isJump(statement: StanStatement): Boolean = statement match {
    case _: StanContinueStatement => true
    case _: StanBreakStatement    => true
    case _: StanReturnStatement   => true
    case block: StanBlock         =>
      // A block is a jump if it contains a jump.
      block.children.exists(c => isJump(c))
    case cond: StanIfStatement    =>
      // An "if" statement is a jump if all branches result in a jump.
      cond.otherwise.exists(o => isJump(o)) && cond.conds.forall(c => isJump(c._2))
    case _                        => false
  }

  private def getControlFlow(
    code: StanStatement,                          // Current statement.
    previousOpt: Option[StanStatement] = None,    // Previous statement in the graph.
    nextOpt: Option[StanStatement] = None,        // Next statement in the graph.
    breakOpt: Option[StanStatement] = None,       // Next statement for a break.
    continueOpt: Option[StanStatement] = None,    // Next statement for a continue.
    links: Map[Int, Set[Int]] = Map.empty
  ): Map[Int, Set[Int]] = {
    // Connect the previous link to this statement unless it's a jump statement, in which case we
    // will have already handled it.
    val newLinks = previousOpt.map { src =>
      if (isJump(src)) links else addLink(src, code, links)
    }.getOrElse(links)

    // Handle each type of statement.
    code match {
      case block: StanBlock          =>
        // Walk each statement in the block.
        // We connect the previous link to the block (already done), the block to the first statement, and
        // the last statement to the next link.
        val start: (Map[Int, Set[Int]], StanStatement) = (newLinks, code)
        val (childrenLinks, newPrev) = block.children.sliding(2).foldLeft(start) { case ((oldLinks, prev), cs) =>
          val child = cs.head
          val newNext = cs.tail.headOption.orElse(nextOpt)
          (getControlFlow(child, Some(prev), newNext, breakOpt, continueOpt, oldLinks), child)
        }
        if (block.children.nonEmpty) {
          getControlFlow(block.children.last, Some(newPrev), nextOpt, breakOpt, continueOpt, childrenLinks)
        } else {
          childrenLinks
        }
      case loop: StanLoop            =>
        // We connect the previous statement to the loop (already done),
        // the loop to the first statement in the body (done by calling getControlFlow on the body with
        // previousOpt set to the loop), and the last statement in the loop body back to the loop (done by
        // having nextOpt set to the loop as well).  We also set loopOpt to the loop so that continue statements
        // can be updated to point to the correct loop.
        getControlFlow(loop.body, Some(loop), Some(loop), nextOpt, Some(loop), newLinks)
      case cond: StanIfStatement     =>
        // We connect the previous statement to the conditional (already done),
        // the conditional to each branch (by setting the conditional as previousOpt), and each conditional
        // to the next statement (by propagating nextOpt).
        val withConds = cond.conds.map(_._2).foldLeft(newLinks) { case (before, child) =>
          getControlFlow(child, Some(cond), nextOpt, breakOpt, continueOpt, before)
        }
        cond.otherwise.map { dest =>
          getControlFlow(dest, Some(cond), nextOpt, breakOpt, continueOpt, withConds)
        }.getOrElse(withConds)
      case _: StanBreakStatement     =>
        // A break connects to the statement after the loop.
        // Note that there may be no such statement if the loop is the last statement in the program.
        breakOpt.foldLeft(newLinks) { case (ls, b) =>
          addLink(code, b, ls)
        }
      case _: StanContinueStatement  =>
        // A continue connects to the loop header.
        addLink(code, continueOpt.get, newLinks)
      case _: StanReturnStatement    =>
        // A return statement is not connected to the next statement.
        newLinks
      case _    =>
        // Other statement types do not alter the control flow, so we simply connect them to the next statement.
        nextOpt.map(next => addLink(code, next, newLinks)).getOrElse(newLinks)
    }
  }

  protected lazy val statementMap: Map[Int, StanStatement] = StanProgram.getStatements(root).map { statement =>
    statement.id -> statement
  }.toMap

  lazy val solve: AnalysisResult[T] = {
    val outLinks: Map[Int, Set[Int]] = getControlFlow(root)
    val inLinks: Map[Int, Set[Int]] = outLinks.toSeq.flatMap { case (parent, children) =>
      children.map(child => child -> parent)
    }.groupBy(_._1).mapValues(vs => vs.map(_._2).toSet)

    val inputs: Map[Int, Set[Int]] = if (forward) inLinks else outLinks     // Inputs to a statement.
    val outputs: Map[Int, Set[Int]] = if (forward) outLinks else inLinks    // Outputs of a statement.

    var before = Map[Int, Set[T]]()   // State before the statement
    var after = Map[Int, Set[T]]()    // State after the statement
    var todo = Set[Int]()             // Work list.

    statementMap.values.foreach { statement =>
      before += (statement.id -> init(statement))
      after += (statement.id -> init(statement))
      todo += statement.id
    }

    while (todo.nonEmpty) {
      val statement = statementMap(todo.head)
      todo -= statement.id
      val in = inputs.getOrElse(statement.id, Set.empty)
      val temp = if (in.nonEmpty) {
        in.tail.foldLeft(after(in.head)) { (a, b) => meet(a, after(b)) }
      } else {
        before(statement.id)
      }
      val updated = transfer(statement, temp)
      if (after(statement.id) != updated) {
        before += statement.id -> temp
        after += statement.id -> updated
        todo ++= outputs.getOrElse(statement.id, Set.empty)
      }
    }

    val result = if (forward) before else after
    AnalysisResult(result)
  }

}
