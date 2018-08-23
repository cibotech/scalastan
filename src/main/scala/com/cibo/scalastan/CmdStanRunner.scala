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

import scala.collection.JavaConverters._
import java.io._
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging

/** A compiled model for CmdStan. */
case class CmdStanCompiledModel(
  dir: File,
  model: ScalaStan#Model,
  dataMapping: Map[String, DataMapping[_]] = Map.empty,
  initialValue: InitialValue = DefaultInitialValue
) extends CompiledModel {
  def replaceMapping(newMapping: Map[String, DataMapping[_]]): CmdStanCompiledModel = copy(dataMapping = newMapping)
  def replaceInitialValue(newInitialValue: InitialValue): CompiledModel = copy(initialValue = newInitialValue)
  def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults = {
    CmdStanRunner.run(
      model = this,
      chains = chains,
      seed = seed,
      cache = cache,
      method = method
    )
  }
}

object CmdStanRunner extends StanRunner[CmdStanCompiledModel] with LazyLogging {

  private val modelExecutable: String = "model"
  private val stanFileName: String = s"$modelExecutable.stan"
  private lazy val CMDSTAN_HOME: Option[String] = sys.env.get("CMDSTAN_HOME")

  // Find a file in the path.
  // Returns the full path to the file.
  private def findInPath(file: String): Option[String] = {
    System.getenv("PATH").split(Pattern.quote(File.pathSeparator)).map { p =>
      Paths.get(p).resolve(file)
    }.find(Files.isExecutable).map(_.toFile.getCanonicalPath)
  }

  // Look up the absolute path to "stanc", if it's in PATH.
  private def findStancInPath: Option[String] = {
    Stream("stanc", "stanc.exe").view.map(findInPath).collectFirst {
      case Some(p) => p
    }
  }

  // The Stan home directory.
  lazy val stanHome: Option[String] = {
    // First check if CMDSTAN_HOME is set.
    // If CMDSTAN_HOME is not set, we attempt to infer it by looking up stanc.
    CMDSTAN_HOME.orElse {
      findStancInPath.map(p => new File(p).getParentFile.getParentFile.getAbsolutePath)
    }
  }

  // Location of the "stanc" program.
  lazy val stanc: Option[String] = stanHome.map(h => s"$h/bin/stanc")

  // The make program to use.
  lazy val make: Option[String] = {
    Stream("gmake", "make", "gmake.exe", "make.exe").map(findInPath).collectFirst {
      case Some(p) => p
    }
  }

  private def runCommand(dir: File, command: Seq[String]): Int = {
    val pb = new ProcessBuilder(command: _*).directory(dir).redirectErrorStream(true)
    val process = pb.start()
    io.Source.fromInputStream(process.getInputStream).getLines.foreach { line =>
      logger.info(line)
    }
    process.waitFor()
  }

  private def runStanc(dir: File): Unit = {
    val stancCommand = stanc.getOrElse {
      throw new IllegalStateException(s"Could not locate stanc.")
    }
    val rc = runCommand(dir, Seq(stancCommand, stanFileName))
    if (rc != 0) {
      throw new IllegalStateException(s"$stanc returned $rc")
    }
  }

  private def runMake(dir: File): Unit = {
    val target = s"$dir/$modelExecutable"
    val makeCommand = make.getOrElse {
      throw new IllegalStateException("Could not locate make.")
    }
    val stanPath = stanHome.getOrElse {
      throw new IllegalStateException("Could not locate Stan.")
    }
    val rc = runCommand(new File(stanPath), Seq(makeCommand, target))
    if (rc != 0) {
      throw new IllegalStateException(s"$make returned $rc")
    }
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

  private val dataFileName: String = "input.R"
  private val initialValueFileName: String = "initial.R"

  def compile(ss: ScalaStan, model: ScalaStan#Model): CmdStanCompiledModel = {
    val stanPath = stanHome.getOrElse {
      throw new IllegalStateException("Could not locate Stan.")
    }
    logger.info(s"found stan in $stanPath")

    model.synchronized {
      val dir = model.generate
      if (new File(s"${dir.getPath}/$modelExecutable").canExecute) {
        logger.info(s"found existing executable: ${dir.getPath}/$modelExecutable")
      } else {
        runStanc(dir)
        runMake(dir)
      }
      CmdStanCompiledModel(dir, model)
    }
  }

  private def writeData(model: CmdStanCompiledModel, method: RunMethod.Method): String = {
    logger.info(s"writing data to $dataFileName")
    val dataWriter = ShaWriter(new PrintWriter(new File(s"${model.dir}/$dataFileName")))
    model.emitData(dataWriter)
    dataWriter.close()
    dataWriter.sha.update(method.toString).digest
  }

  private def writeInitialValue(model: CmdStanCompiledModel, runHash: String): String = {
    model.initialValue match {
      case InitialValueMapping(_) =>
        logger.info(s"writing initial values to $initialValueFileName")
        val writer = ShaWriter(new PrintWriter(new File(s"${model.dir}/$initialValueFileName")))
        model.emitInitialValues(writer)
        writer.close()
        writer.sha.update(runHash).digest
      case _ => runHash
    }
  }

  private def initialValueArguments(model: CmdStanCompiledModel): Vector[String] = model.initialValue match {
    case DefaultInitialValue => Vector.empty
    case InitialValueMapping(_) => Vector(s"init=$initialValueFileName")
    case InitialValueDouble(d) => Vector(s"init=$d")
  }

  def run(
    model: CmdStanCompiledModel,
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
        val fileName = s"${model.dir}/$name"
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
            val rc = runCommand(model.dir, command)
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
