package com.cibo.scalastan

import com.cibo.scalastan.ast.StanDeclaration

abstract class StanTransformBase[T <: StanType, D <: StanDeclaration[T]] extends StanCodeBlock {
  val result: D
  def export(builder: StanProgramBuilder): Unit
  val name: String
}
