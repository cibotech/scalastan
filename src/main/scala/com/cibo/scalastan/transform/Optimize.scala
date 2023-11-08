/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.StanContext
import com.cibo.scalastan.ast.StanProgram

case class Optimize() extends StanTransform[Unit] {
  def initialState: Unit = ()

  private val steps: Seq[StanTransform[_]] = Seq(
    SplitExpressions(),
    CSE(),
    CopyPropagation()
  )

  override def run(program: StanProgram)(implicit context: StanContext): StanProgram = {
    steps.foldLeft(program) { (input, step) => step.run(input) }
  }
}
