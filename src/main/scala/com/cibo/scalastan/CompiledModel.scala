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

import com.cibo.scalastan.ast.StanDataDeclaration

abstract class CompiledModel {
  private[scalastan] val ss: ScalaStan
  protected val dataMapping: Map[String, DataMapping[_]]

  protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CompiledModel
  protected def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults

  private[scalastan] final def emitData(writer: Writer): Unit = {
    ss.dataValues.foreach { value =>
      val mapping = dataMapping.getOrElse(value.emit,
        throw new IllegalStateException(s"no data provided for ${value.emit}")
      )
      writer.write(mapping.emit)
      writer.write("\n")
    }
  }

  /** Get the specified input data. */
  final def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = dataMapping(decl.emit).values.asInstanceOf[T#SCALA_TYPE]

  /** Reset all data bindings. */
  final def reset: CompiledModel = replaceMapping(Map.empty)

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

    // Look up and set the size parameters.
    val (withDecls, _) = decl.typeConstructor.getIndices.foldLeft((this, conv: Any)) { case ((old, d), dim) =>
      val ds = d.asInstanceOf[Seq[_]]
      val next = if (ds.nonEmpty) ds.head else Seq.empty
      dim match {
        case indexDecl: StanDataDeclaration[StanInt] => (old.withData(indexDecl, ds.size), next)
        case _                                       => (old, next)
      }
    }

    // Insert the binding.
    withDecls.replaceMapping(withDecls.dataMapping.updated(decl.emit, DataMapping[T](decl, conv)))
  }

  /** Add a binding from a data source. */
  final def withData[T <: StanType, V](
    value: (StanDataDeclaration[T], V)
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = withData(value._1, value._2)

  /** Run the model and get results. */
  final def run(
    chains: Int = 4,
    seed: Int = -1,
    cache: Boolean = true,
    method: RunMethod.Method = RunMethod.Sample()
  ): StanResults = {
    require(chains > 0, s"Must run at least one chain")

    // Make sure all the necessary data is provided.
    ss.dataValues.filterNot(v => dataMapping.contains(v.emit)).foreach { v =>
      throw new IllegalStateException(s"data not supplied for ${v.name}")
    }

    runChecked(chains, seed, cache, method)
  }
}

/** A compiled model for CmdStan. */
protected case class CmdStanCompiledModel private[scalastan] (
  private[scalastan] val dir: File,
  private[scalastan] val ss: ScalaStan,
  protected val dataMapping: Map[String, DataMapping[_]] = Map.empty
) extends CompiledModel {
  protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CmdStanCompiledModel =
    copy(dataMapping = newMapping)
  protected def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults = {
    StanRunner.CmdStanRunner.run(
      model = this,
      chains = chains,
      seed = seed,
      cache = cache,
      method = method
    )
  }
}
