/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.run

import java.io._

import com.cibo.scalastan._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.JavaConverters._

class CmdStanRunner(modelDir: File) extends StanRunner with LazyLogging {

  private val modelExecutable: String = CmdStanCompiler.modelExecutable
  private val dataFileName: String = "input.R"
  private val initialValueFileName: String = "initial.R"

  private def writeData(model: CompiledModel, method: RunMethod.Method): String = {
    logger.info(s"writing data to $dataFileName")
    val dataWriter = ShaWriter(new PrintWriter(new File(s"$modelDir/$dataFileName")))
    model.emitData(dataWriter)
    dataWriter.close()
    dataWriter.sha.update(method.toString).digest
  }

  private def writeInitialValue(model: CompiledModel, runHash: String): String = {
    model.initialValue match {
      case InitialValueMapping(_) =>
        logger.info(s"writing initial values to $initialValueFileName")
        val writer = ShaWriter(new PrintWriter(new File(s"$modelDir/$initialValueFileName")))
        model.emitInitialValues(writer)
        writer.close()
        writer.sha.update(runHash).digest
      case InitialValueDouble(v) =>
        SHA().update(v.toString).update(runHash).digest
      case _ => runHash
    }
  }

  private def initialValueArguments(model: CompiledModel): Vector[String] = model.initialValue match {
    case DefaultInitialValue => Vector.empty
    case InitialValueMapping(_) => Vector(s"init=$initialValueFileName")
    case InitialValueDouble(d) => Vector(s"init=$d")
  }

  private def readIterations(fileName: String): Map[String, Vector[String]] = {
    val reader = new BufferedReader(new FileReader(fileName))
    try {
      val lines = reader.lines.iterator.asScala.filterNot(_.startsWith("#")).toVector
      if (lines.nonEmpty) {
        val header = lines.head.split(',').toVector
        val columns = lines.tail.map(_.split(',').toVector).transpose
        header.zip(columns).toMap
      } else {
        Map.empty
      }
    } finally {
      reader.close()
    }
  }

  def run(
    model: CompiledModel,
    chains: Int,
    seed: Int,
    cache: Boolean,
    method: RunMethod.Method
  ): StanResults = {

    // Only allow one instance of a model to run at a time.
    model.model.synchronized {

      // Emit the data file.
      val dataHash = writeData(model, method)

      // Emit initial value file.
      val runHash = writeInitialValue(model, dataHash)

      val initArguments = initialValueArguments(model)
      val baseSeed = if (seed < 0) (System.currentTimeMillis % Int.MaxValue).toInt else seed
      val results = (0 until chains).par.flatMap { i =>
        val chainSeed = baseSeed + i
        val name = s"$runHash-$seed-$i.csv"
        val fileName = s"$modelDir/$name"
        val cachedResults = if (cache && new File(fileName).exists) {
          Some(readIterations(fileName))
        } else None
        cachedResults match {
          case Some(r) if r.nonEmpty =>
            logger.info(s"Found cached results: $name")
            Some(r)
          case _                     =>
            val command = Vector(
              s"./$modelExecutable",
              "data", s"file=$dataFileName",
              "output", s"file=$name",
              "random", s"seed=$chainSeed"
            ) ++ initArguments ++ method.arguments
            logger.info("Running " + command.mkString(" "))
            val rc = CommandRunner.runCommand(modelDir, command)
            if (rc != 0) {
              logger.error(s"model returned $rc")
              None
            } else {
              Some(readIterations(fileName))
            }
        }
      }.seq.toVector

      val parameterChains: Map[String, Vector[Vector[String]]] = results.flatten.groupBy(_._1).mapValues(_.map(_._2))
      StanResults(parameterChains, model)
    }
  }
}

