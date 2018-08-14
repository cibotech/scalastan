/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.ScalaStan
import com.cibo.scalastan.ast._

// Make sure all "break" and "continue" statements are in loops.
class LoopChecker(implicit val ss: ScalaStan) extends StanTransform[Int] {

  def initialState: Int = 0

  def increment: State[Int] = State { s => (s, s + 1) }
  def decrement: State[Int] = State { s => (s, s - 1) }

  override def handleWhile(w: StanWhileLoop): State[StanStatement] = for {
    _ <- increment
    newWhile <- super.handleWhile(w)
    _ <- decrement
  } yield newWhile

  override def handleFor(f: StanForLoop): State[StanStatement] = for {
    _ <- increment
    newFor <- super.handleFor(f)
    _ <- decrement
  } yield newFor

  override def handleBreak(b: StanBreakStatement): State[StanStatement] = for {
    current <- State.get
    _ = if (current == 0) throw new IllegalStateException("'break' statement must be in a loop")
    newBreak <- super.handleBreak(b)
  } yield newBreak

  override def handleContinue(c: StanContinueStatement): State[StanStatement] = for {
    current <- State.get
    _ = if (current == 0) throw new IllegalStateException("'continue' statement must be in a loop")
    newContinue <- super.handleContinue(c)
  } yield newContinue
}
