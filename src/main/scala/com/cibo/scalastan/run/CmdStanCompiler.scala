/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.run

import java.io.{File, PrintWriter, StringWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern

import com.cibo.scalastan._
import com.typesafe.scalalogging.LazyLogging

object CmdStanCompiler extends StanCompiler with CommandRunner with LazyLogging {

  lazy val CMDSTAN_HOME: Option[String] = sys.env.get("CMDSTAN_HOME")
  val modelExecutable: String = "model"
  val stanFileName: String = s"$modelExecutable.stan"
  val cacheVersion: Int = 1

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

  def runStanc(dir: File): Unit = {
    val stancCommand = stanc.getOrElse {
      throw new IllegalStateException(s"Could not locate stanc.")
    }
    val rc = runCommand(dir, Seq(stancCommand, stanFileName))
    if (rc != 0) {
      throw new IllegalStateException(s"$stanc returned $rc")
    }
  }

  def runMake(dir: File): Unit = {
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

  lazy val basePath: Path = {
    Option(System.getenv("HOME")) match {
      case Some(home) => Paths.get(home).resolve(".scalastan")
      case None       => Paths.get("/tmp").resolve("scalastan")
    }
  }

  def modelDirString(modelHash: String): String = s"$modelHash-$cacheVersion"

  def modelDir(modelHash: String): File = basePath.resolve(modelDirString(modelHash)).toFile

  def cleanOldModels(modelHash: String, maxCacheSize: Int): Unit = {
    if (basePath.toFile.exists) {
      val dirString = modelDirString(modelHash)
      val dirs = basePath.toFile.listFiles.filter { f =>
        f.isDirectory && f.getName != dirString
      }.sortBy(_.lastModified).dropRight(maxCacheSize - 1)
      dirs.foreach { dir =>
        logger.info(s"removing old cache directory ${dir.getAbsolutePath}")
        try {
          Option(dir.listFiles).map { files =>
            files.foreach(_.delete())
            dir.delete()
          }
        } catch {
          case ex: Exception =>
            logger.warn("unable to remove cache directory", ex)
        }
      }
    }
  }

  def generate(model: StanModel, maxCacheSize: Int): String = {

    val writer = new StringWriter()
    model.emit(new PrintWriter(writer))
    writer.close()
    val str = writer.toString
    logger.debug(s"code:\n$str")

    val modelHash = SHA.hash(str)
    cleanOldModels(modelHash, maxCacheSize)

    val dir = modelDir(modelHash)
    if (!dir.exists || !dir.listFiles().exists(_.getName == stanFileName)) {
      logger.info(s"writing code to $dir/$stanFileName")
      Files.createDirectories(dir.toPath)
      val codeFile = new File(s"${dir.getPath}/$stanFileName")
      val codeWriter = new PrintWriter(codeFile)
      codeWriter.print(str)
      codeWriter.close()
    } else {
      logger.info(s"found existing code in $dir/$stanFileName")
    }
    modelHash
  }

  def compile(model: StanModel): CompiledModel = {
    val stanPath = stanHome.getOrElse {
      throw new IllegalStateException("Could not locate Stan.")
    }
    logger.info(s"found stan in $stanPath")

    model.synchronized {
      val modelHash = generate(model, model.maxCacheSize)
      val dir = modelDir(modelHash)
      if (new File(s"${dir.getPath}/$modelExecutable").canExecute) {
        logger.info(s"found existing executable: ${dir.getPath}/$modelExecutable")
      } else {
        runStanc(dir)
        runMake(dir)
      }
      CompiledModel(model, new CmdStanRunner(dir, modelHash))
    }
  }

}

