/*
 * Copyright (c) 2017 - 2019 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.PrintWriter

import com.cibo.scalastan.ast.StanProgram
import com.cibo.scalastan.run.StanCompiler
import com.cibo.scalastan.transform.{LoopChecker, StanTransform}

case class TransformedModel(
  model: StanModel,
  transformations: Vector[StanTransform[_]] = Vector(LoopChecker())
) extends StanModel {
  override def emit(writer: PrintWriter): Unit = program.emit(writer)
  override def program: StanProgram = transformations.foldLeft(model.program) { (p, t) => t.run(p) }
  override def transform(t: StanTransform[_]): TransformedModel = copy(transformations = transformations :+ t)
  override def compile(implicit compiler: StanCompiler): CompiledModel = compiler.compile(model)
}

