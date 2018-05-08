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
import com.cibo.scalastan.ast.StanProgram

case class Optimize()(implicit val ss: ScalaStan) extends StanTransform {
  private val steps: Seq[StanTransform] = Seq(
    SplitExpressions(),
    CSE(),
    CopyPropagation()
  )

  override def run(program: StanProgram): StanProgram = {
    steps.foldLeft(program) { (input, step) => step.run(input) }
  }
}
