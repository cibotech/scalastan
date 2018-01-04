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

import com.cibo.scalastan.ast._

// Make sure all "break" and "continue" statements are in loops.
object LoopChecker extends StanTransform[Int] {
  protected val initialState = 0 // Number of loops

  override protected def handleWhile(w: StanWhileLoop, state: Int): StanStatement = {
    super.handleWhile(w, state + 1)
  }

  override protected def handleFor(f: StanForLoop, state: Int): StanStatement = {
    super.handleFor(f, state + 1)
  }

  override protected def handleBreak(b: StanBreakStatement, state: Int): StanStatement = {
    if (state == 0) {
      throw new IllegalStateException("'break' statement must be in a loop")
    }
    super.handleBreak(b, state)
  }

  override protected def handleContinue(c: StanContinueStatement, state: Int): StanStatement = {
    if (state == 0) {
      throw new IllegalStateException("'continue' statement must be in a loop")
    }
    super.handleContinue(c, state)
  }
}
