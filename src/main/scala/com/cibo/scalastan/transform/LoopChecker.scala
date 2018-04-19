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
class LoopChecker(implicit val ss: ScalaStan) extends StanTransform {

  private var state = 0

  override protected def handleWhile(w: StanWhileLoop): StanStatement = {
    state += 1
    val newWhile = super.handleWhile(w)
    state -= 1
    newWhile
  }

  override protected def handleFor(f: StanForLoop): StanStatement = {
    state += 1
    val newFor = super.handleFor(f)
    state -= 1
    newFor
  }

  override protected def handleBreak(b: StanBreakStatement): StanStatement = {
    if (state == 0) {
      throw new IllegalStateException("'break' statement must be in a loop")
    }
    super.handleBreak(b)
  }

  override protected def handleContinue(c: StanContinueStatement): StanStatement = {
    if (state == 0) {
      throw new IllegalStateException("'continue' statement must be in a loop")
    }
    super.handleContinue(c)
  }
}
