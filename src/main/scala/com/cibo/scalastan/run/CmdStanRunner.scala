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

case class CmdStanChainContext(
  compiledModel: CompiledModel,
  method: RunMethod.Method,
  modelDir: File,
  outputFile: File,
  modelExecutable: File,
  dataFile: File,
  initialValueFile: Option[File],
  chainSeed: Int,
  runHash: String
) extends LazyLogging {

  def initialValueArguments: Vector[String] = compiledModel.initialValue match {
    case DefaultInitialValue => Vector.empty
    case InitialValueMapping(_) => Vector(s"init=${initialValueFile.get.getName}")
    case InitialValueDouble(d) => Vector(s"init=$d")
  }

  def run(): Int = {
    val command = Vector(
      s"./$modelExecutable",
      "data", s"file=${dataFile.getName}",
      "output", s"file=${outputFile.getName}",
      "random", s"seed=$chainSeed"
    ) ++ initialValueArguments ++ method.arguments
    logger.info("Running " + command.mkString(" "))
    CommandRunner.runCommand(modelDir, command)
  }

}

class CmdStanRunner(
  val modelDir: File,
  val modelHash: String
) extends StanRunner with LazyLogging {

  private val modelExecutable: String = CmdStanCompiler.modelExecutable

  def dataFileName: String = s"$modelDir/input.R"
  def initialValueFileName: String = s"$modelDir/initial.R"

  def writeData(model: CompiledModel, method: RunMethod.Method): String = {
    logger.info(s"writing data to $dataFileName")
    val dataWriter = ShaWriter(new PrintWriter(new File(dataFileName)))
    model.emitData(dataWriter)
    dataWriter.close()
    dataWriter.sha.update(method.toString).digest
  }

  private def writeInitialValue(model: CompiledModel, runHash: String): String = {
    model.initialValue match {
      case InitialValueMapping(_) =>
        logger.info(s"writing initial values to $initialValueFileName")
        val writer = ShaWriter(new PrintWriter(new File(initialValueFileName)))
        model.emitInitialValues(writer)
        writer.close()
        writer.sha.update(runHash).digest
      case InitialValueDouble(v) =>
        SHA().update(v.toString).update(runHash).digest
      case _ => runHash
    }
  }

  def initialValueFile(model: CompiledModel): Option[File] = model.initialValue match {
    case InitialValueMapping(_) => Some(new File(initialValueFileName))
    case _                      => None
  }

  def readIterations(file: File): Map[String, Vector[String]] = {
    val reader = new BufferedReader(new FileReader(file))
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

  def outputFileName(runHash: String, seed: Int, chainIndex: Int): String = s"$runHash-$seed-$chainIndex.csv"

  def loadFromCache(file: File): Option[Map[String, Vector[String]]] = {
    if (file.exists) Some(readIterations(file)) else None
  }

  // Write data and initial value files and return the run hash.
  def writeFiles(compiledModel: CompiledModel, method: RunMethod.Method): String = {
    // Emit the data and initial value files.
    val dataHash = writeData(compiledModel, method)
    writeInitialValue(compiledModel, dataHash)
  }

  def runChain(context: CmdStanChainContext): Option[Map[String, Vector[String]]] = {
    val rc = context.run()
    if (rc != 0) {
      logger.error(s"model returned $rc")
      None
    } else {
      Some(readIterations(context.outputFile))
    }
  }

  def run(
    compiledModel: CompiledModel,
    chains: Int,
    seed: Int,
    cache: Boolean,
    method: RunMethod.Method
  ): StanResults = {

    // Only allow one instance of a model to run at a time.
    compiledModel.model.synchronized {

      val runHash = writeFiles(compiledModel, method)
      val baseSeed = if (seed < 0) (System.currentTimeMillis % Int.MaxValue).toInt else seed
      val results = Vector.range(0, chains).par.flatMap { chainIndex =>
        val chainSeed = ((baseSeed.toLong + chainIndex) % Int.MaxValue).toInt
        val outputName = outputFileName(runHash, seed, chainIndex)
        val outputFile = new File(s"$modelDir/$outputName")
        val context = CmdStanChainContext(
          compiledModel = compiledModel,
          method = method,
          modelDir = modelDir,
          outputFile = outputFile,
          modelExecutable = new File(s"./$modelExecutable"),
          dataFile = new File(dataFileName),
          initialValueFile = initialValueFile(compiledModel),
          chainSeed = chainSeed,
          runHash = runHash
        )
        val cachedResults = if (cache) loadFromCache(outputFile) else None
        cachedResults match {
          case Some(r) if r.nonEmpty =>
            logger.info(s"Found cached results: $outputName")
            Some(r)
          case _ => runChain(context)
        }
      }.seq

      val parameterChains: Map[String, Vector[Vector[String]]] = results.flatten.groupBy(_._1).mapValues(_.map(_._2))
      StanResults(parameterChains, compiledModel)
    }
  }
}

