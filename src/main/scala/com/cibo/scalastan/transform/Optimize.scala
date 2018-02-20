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
