/*
 * Copyright (c) 2017 - 2021 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.ast

import java.io.PrintWriter

import com.cibo.scalastan.StanType
import com.cibo.scalastan.transform.StanTransform

case class StanProgram(
  data: Seq[StanDataDeclaration[_ <: StanType]] = Seq.empty,
  parameters: Seq[StanParameterDeclaration[_ <: StanType]] = Seq.empty,
  functions: Seq[StanFunctionDeclaration] = Seq.empty,
  transformedData: Seq[StanTransformedData] = Seq.empty,
  transformedParameters: Seq[StanTransformedParameter] = Seq.empty,
  generatedQuantities: Seq[StanGeneratedQuantity] = Seq.empty,
  model: StanStatement = StanBlock(Seq.empty)
) {

  def emit(writer: PrintWriter): Unit = {
    if (functions.nonEmpty) {
      writer.println("functions {")
      functions.foreach(f => f.emit(writer))
      writer.println("}")
    }
    if (data.nonEmpty) {
      writer.println("data {")
      data.foreach(d => writer.println(s"  ${d.emitDeclaration};"))
      writer.println("}")
    }
    if (transformedData.nonEmpty) {
      writer.println("transformed data {")
      transformedData.foreach(d => writer.println(s"  ${d.result.emitDeclaration};"))
      transformedData.foreach(_.emit(writer))
      writer.println("}")
    }
    if (parameters.nonEmpty) {
      writer.println("parameters {")
      parameters.foreach(d => writer.println(s"  ${d.emitDeclaration};"))
      writer.println("}")
    }
    if (transformedParameters.nonEmpty) {
      writer.println("transformed parameters {")
      transformedParameters.foreach(d => writer.println(s"  ${d.result.emitDeclaration};"))
      transformedParameters.foreach(_.emit(writer))
      writer.println("}")
    }
    writer.println("model {")
    model.emitDeclarations(writer, 1)
    model.emit(writer, 1)
    writer.println("}")
    if (generatedQuantities.nonEmpty) {
      writer.println("generated quantities {")
      generatedQuantities.foreach(d => writer.println(s"  ${d.result.emitDeclaration};"))
      generatedQuantities.foreach(_.emit(writer))
      writer.println("}")
    }
  }
}

object StanProgram {
  def getStatements(code: StanStatement): Seq[StanStatement] = {
    val inner = code match {
      case block: StanBlock      => block.children.flatMap(child => getStatements(child))
      case loop: StanLoop        => getStatements(loop.body)
      case cond: StanIfStatement =>
        cond.conds.flatMap(c => getStatements(c._2)) ++ cond.otherwise.map(getStatements).getOrElse(Vector.empty)
      case _                     => Vector.empty
    }
    code +: inner
  }

  def getStatements(program: StanProgram): Seq[StanStatement] = {
    getStatements(program.model) ++
      program.generatedQuantities.flatMap(q => getStatements(q.code)) ++
      program.transformedData.flatMap(d => getStatements(d.code)) ++
      program.transformedParameters.flatMap(p => getStatements(p.code))
  }
}
