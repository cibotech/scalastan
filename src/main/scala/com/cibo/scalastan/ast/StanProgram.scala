package com.cibo.scalastan.ast

import java.io.PrintWriter

case class StanProgram(
  data: Seq[StanDataDeclaration[_]],
  parameters: Seq[StanParameterDeclaration[_]],
  functions: Seq[StanFunctionDeclaration],
  transformedData: Seq[StanTransformedData],
  transformedParameters: Seq[StanTransformedParameter],
  generatedQuantities: Seq[StanGeneratedQuantity],
  model: StanStatement
) {
  private[scalastan] def emit(writer: PrintWriter): Unit = {
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
