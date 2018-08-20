/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.{File, Writer}

import com.cibo.scalastan.ast.{StanDataDeclaration, StanParameterDeclaration}

abstract class CompiledModel {
  private[scalastan] val model: ScalaStan#Model
  protected val dataMapping: Map[String, DataMapping[_]]
  protected val initialValues: Map[String, DataMapping[_]]

  protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CompiledModel
  protected def updateInitialValue(name: String, value: DataMapping[_]): CompiledModel
  protected def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults

  private def emitMapping(mapping: Map[String, DataMapping[_]], writer: Writer): Unit = {
    mapping.values.foreach { value =>
      writer.write(value.emit)
      writer.write("\n")
    }
  }

  private[scalastan] final def emitData(writer: Writer): Unit = emitMapping(dataMapping, writer)
  private[scalastan] final def emitInitialValues(writer: Writer): Unit = emitMapping(initialValues, writer)

  /** Get the specified input data. */
  final def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = dataMapping(decl.emit).values.asInstanceOf[T#SCALA_TYPE]

  /** Reset all data bindings. */
  final def reset: CompiledModel = replaceMapping(Map.empty)

  /** Look up and set size declarations. */
  private def setSizes[T <: StanType](valueType: T, data: Any): CompiledModel = {
    valueType.getIndices.foldLeft((this, data)) { case ((old, d), dim) =>
      val ds = d.asInstanceOf[Seq[_]]
      val next = if (ds.nonEmpty) ds.head else Seq.empty
      dim match {
        case indexDecl: StanDataDeclaration[StanInt] => (old.withData(indexDecl, ds.size), next)
        case _                                       => (old, next)
      }
    }._1
  }

  /** Add a data binding. */
  final def withData[T <: StanType, V](
    decl: StanDataDeclaration[T],
    data: V
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = {
    val conv = data.asInstanceOf[T#SCALA_TYPE]

    // Check if this parameter has already been assigned and throw an exception if the values are conflicting.
    dataMapping.get(decl.emit) match {
      case Some(s) if s.values != data =>
        throw new IllegalStateException(s"conflicting values assigned to ${decl.name}")
      case _                           => ()
    }

    // Insert/check size declarations.
    val withDecls = setSizes(decl.returnType, conv)

    // Insert the binding.
    withDecls.replaceMapping(withDecls.dataMapping.updated(decl.emit, DataMapping[T](decl, conv)))
  }

  /** Add a binding from a data source. */
  final def withData[T <: StanType, V](
    value: (StanDataDeclaration[T], V)
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = withData(value._1, value._2)

  /** Set the initial value for a parameter. */
  final def withInitialValue[T <: StanType, V](
    decl: StanParameterDeclaration[T],
    value: V
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = {
    val conv = value.asInstanceOf[T#SCALA_TYPE]

    // Insert/check size declarations
    val withDecls = setSizes(decl.returnType, conv)

    // Record the initial value.
    withDecls.updateInitialValue(decl.emit, DataMapping[T](decl, conv))
  }

  /** Run the model and get results. */
  final def run(
    chains: Int = 4,
    seed: Int = -1,
    cache: Boolean = true,
    method: RunMethod.Method = RunMethod.Sample()
  ): StanResults = {
    require(chains > 0, s"Must run at least one chain")

    // Make sure all the necessary data is provided.
    model.program.data.filterNot(v => dataMapping.contains(v.emit)).foreach { v =>
      throw new IllegalStateException(s"data not supplied for ${v.name}")
    }

    runChecked(chains, seed, cache, method)
  }
}

