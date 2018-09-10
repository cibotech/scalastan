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
import java.nio.file._

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
    val temp = Files.createTempFile(outputFile.getParentFile.toPath, "iterations", ".csv").toFile
    val command = Vector(
      s"./$modelExecutable",
      "data", s"file=${dataFile.getName}",
      "output", s"file=${temp.getName}",
      "random", s"seed=$chainSeed"
    ) ++ initialValueArguments ++ method.arguments
    logger.info("Running " + command.mkString(" "))
    val rc = CommandRunner.runCommand(modelDir, command)
    if (rc == 0) {
      Files.move(temp.toPath, outputFile.toPath, StandardCopyOption.ATOMIC_MOVE)
    } else {
      temp.delete()
    }
    rc
  }

}

class CmdStanRunner(
  val modelDir: File,
  val modelHash: String
) extends StanRunner with LazyLogging {

  private val modelExecutable: String = CmdStanCompiler.modelExecutable

  private val initialValuePrefix: String = "initial"
  private val dataPrefix: String = "input"

  case class ChainResult(
    iterations: Map[String, Vector[String]],
    inverseMassMatrixDiagonals: Vector[Double]
  )

  def initialValueFileName(hash: String): String = s"$modelDir/$initialValuePrefix-$hash.R"

  def dataFileName(hash: String): String = s"$modelDir/$dataPrefix-$hash.R"

  /** Write to a file whose name contains the hash. */
  def writeToHashedFileName(prefix: String, emit: Writer => Unit): String = {

    // Write the data with a temporary file name to get the hash.
    val temp = Files.createTempFile(modelDir.toPath, prefix, ".R").toFile
    val writer = ShaWriter(new FileWriter(temp))
    emit(writer)
    writer.close()
    val hash = writer.sha.digest

    // Move the temp file to the expected name.
    val fileName = s"$modelDir/$prefix-$hash.R"
    logger.info(s"writing $prefix to $fileName")
    Files.move(temp.toPath, Paths.get(fileName), StandardCopyOption.ATOMIC_MOVE)

    hash
  }

  /** Write the data file. Returns the data hash. */
  def writeData(model: CompiledModel): String = writeToHashedFileName(dataPrefix, model.emitData)

  /** Write the initial value file. Returns the hash of the initial value (if any). */
  def writeInitialValue(model: CompiledModel): Option[String] = {
    model.initialValue match {
      case InitialValueMapping(_) =>
        Some(writeToHashedFileName(initialValuePrefix, model.emitInitialValues))
      case InitialValueDouble(d) => Some(SHA.hash(d.toString))
      case _ => None
    }
  }

  /** Compute the run hash (used for the output file name). */
  def computeRunHash(dataHash: String, initialValueHash: Option[String], method: RunMethod.Method): String = {
    val sha = SHA()
    sha.update(dataHash)
    initialValueHash.foreach(sha.update)
    method.arguments.foreach(sha.update)
    sha.digest
  }

  def initialValueFile(
    model: CompiledModel,
    initialValueHash: Option[String]
  ): Option[File] = model.initialValue match {
    case InitialValueMapping(_) => Some(new File(initialValueFileName(initialValueHash.get)))
    case _                      => None
  }

  private def loadInverseMassMatrixDiagonals(rawLines: Vector[String]): Vector[Double] = {
    val diagonalHeader = "# Diagonal elements of inverse mass matrix:"
    val i = rawLines.indexWhere(_.startsWith(diagonalHeader))
    if (i > 0 && i + 1 < rawLines.length) {
      rawLines(i + 1).drop(1).split(',').map(_.toDouble).toVector
    } else {
      Vector.empty
    }
  }

  def readIterations(file: File): ChainResult = {
    val reader = new BufferedReader(new FileReader(file))
    try {
      val rawLines = reader.lines.iterator.asScala.toVector
      val lines = rawLines.filterNot(_.startsWith("#"))
      if (lines.nonEmpty) {
        val header = lines.head.split(',').toVector
        val columns = lines.tail.map(_.split(',').toVector).transpose
        ChainResult(header.zip(columns).toMap, loadInverseMassMatrixDiagonals(rawLines))
      } else {
        ChainResult(Map.empty, Vector.empty)
      }
    } finally {
      reader.close()
    }
  }

  def outputFileName(runHash: String, seed: Int, chainIndex: Int): String = s"$runHash-$seed-$chainIndex.csv"

  def loadFromCache(file: File): Option[ChainResult] = {
    if (file.exists) Some(readIterations(file)) else None
  }

  def runChain(context: CmdStanChainContext): Option[ChainResult] = {
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

    val dataHash = writeData(compiledModel)
    val initialValueHash = writeInitialValue(compiledModel)
    val runHash = computeRunHash(dataHash, initialValueHash, method)
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
        dataFile = new File(dataFileName(dataHash)),
        initialValueFile = initialValueFile(compiledModel, initialValueHash),
        chainSeed = chainSeed,
        runHash = runHash
      )
      val cachedResults = if (cache) loadFromCache(outputFile) else None
      cachedResults match {
        case Some(r) if r.iterations.nonEmpty =>
          logger.info(s"Found cached results: $outputName")
          Some(r)
        case _ => runChain(context)
      }
    }.seq

    val parameterChains = results.flatMap(_.iterations).groupBy(_._1).mapValues(_.map(_._2))
    val inverseMassMatrixDiagonals = results.map(_.inverseMassMatrixDiagonals)
    StanResults(parameterChains, inverseMassMatrixDiagonals, compiledModel, method)
  }
}

