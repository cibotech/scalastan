/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.Writer

import com.cibo.scalastan.ast.{StanDataDeclaration, StanParameterDeclaration}
import com.cibo.scalastan.run.StanRunner

sealed trait InitialValue

case object DefaultInitialValue extends InitialValue
case class InitialValueDouble(v: Double) extends InitialValue
case class InitialValueMapping(mapping: Map[String, DataMapping[_]]) extends InitialValue

case class CompiledModel(
  model: StanModel,
  runner: StanRunner,
  dataMapping: Map[String, DataMapping[_]] = Map.empty,
  initialValue: InitialValue = DefaultInitialValue
) {

  private def emitMapping(mapping: Map[String, DataMapping[_]], writer: Writer): Unit = {
    mapping.values.foreach { value =>
      writer.write(value.emit)
      writer.write("\n")
    }
  }

  final def emitData(writer: Writer): Unit = emitMapping(dataMapping, writer)
  final def emitInitialValues(writer: Writer): Unit = initialValue match {
    case InitialValueMapping(mapping) => emitMapping(mapping, writer)
    case _                            => ()
  }

  /** Get the specified input data. */
  final def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = dataMapping(decl.emit).values.asInstanceOf[T#SCALA_TYPE]

  /** Reset all bindings. */
  final def reset: CompiledModel = copy(
    dataMapping = Map.empty,
    initialValue = DefaultInitialValue
  )

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
    withDecls.copy(
      dataMapping = withDecls.dataMapping.updated(decl.emit, DataMapping[T](decl, conv))
    )
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
    val newValue = decl.emit -> DataMapping[T](decl, conv)
    initialValue match {
      case DefaultInitialValue          => withDecls.copy(initialValue = InitialValueMapping(Map(newValue)))
      case InitialValueMapping(mapping) => withDecls.copy(initialValue = InitialValueMapping(mapping + newValue))
      case InitialValueDouble(_)        =>
        throw new IllegalStateException("Initial value already set.")
    }
  }

  /** Set the bounds on initial values. */
  final def withInitialValue(value: Double): CompiledModel = {
    require(value >= 0, s"The upper bound on the initial value must be >= 0, got $value")
    initialValue match {
      case DefaultInitialValue => copy(initialValue = InitialValueDouble(value))
      case _                   => throw new IllegalStateException("Initial value already set.")
    }
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

    runner.run(
      compiledModel = this,
      chains = chains,
      seed = seed,
      cache = cache,
      method = method
    )
  }
}

