/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io._

import scala.collection.JavaConverters._

protected trait StanRunner[M <: CompiledModel] {
  def compile(ss: ScalaStan, model: ScalaStan#Model): CompiledModel
  def run(
    model: M,
    chains: Int,
    seed: Int,
    cache: Boolean,
    method: RunMethod.Method
  ): StanResults
}

protected object StanRunner {

  // Use CmdStan to compile and run the model.
  implicit object CmdStanRunner extends StanRunner[CmdStanCompiledModel] {

    private val modelExecutable: String = "model"
    private val stanFileName: String = s"$modelExecutable.stan"

    private def findStan: String = {
      val pb = new ProcessBuilder("which", "stanc")
      val process = pb.start()
      val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
      val rc = process.waitFor()
      if (rc != 0) {
        throw new IllegalStateException("stanc not found")
      }
      val stancFile = new File(reader.lines.iterator.asScala.toStream.last).getCanonicalFile
      stancFile.getParentFile.getParentFile.getAbsolutePath
    }

    private def stanc(dir: File): Unit = {
      val pb = new ProcessBuilder("stanc", stanFileName).directory(dir).inheritIO()
      val rc = pb.start().waitFor()
      if (rc != 0) {
        throw new IllegalStateException(s"stanc returned $rc")
      }
    }

    private def make(dir: File, stanDir: String): Unit = {
      val target = s"$dir/$modelExecutable"
      val pb = new ProcessBuilder("make", target).directory(new File(stanDir)).inheritIO()
      val rc = pb.start().waitFor()
      if (rc != 0) {
        throw new IllegalStateException(s"make returned $rc")
      }
    }

    private def readIterations(fileName: String): Vector[Map[String, String]] = {
      val reader = new BufferedReader(new FileReader(fileName))
      try {
        val lines = reader.lines.iterator.asScala.filterNot(_.startsWith("#")).toVector
        if (lines.nonEmpty) {
          val header: Seq[String] = lines.head.split(',')
          lines.tail.map { sample =>
            header.zip(sample.split(',')).toMap
          }
        } else {
          Vector.empty
        }
      } finally {
        reader.close()
      }
    }

    private var dataFileIndex: Int = 0

    private def getNextDataFileName: String = {
      synchronized {
        dataFileIndex += 1
        s"data$dataFileIndex.R"
      }
    }

    def compile(ss: ScalaStan, model: ScalaStan#Model): CmdStanCompiledModel = {
      val stanDir = findStan
      println(s"found stan in $stanDir")

      // Generate the code.
      val dir = model.generate

      if (new File(s"${dir.getPath}/$modelExecutable").canExecute) {
        println("found existing executable")
      } else {
        stanc(dir)
        make(dir, stanDir)
      }

      CmdStanCompiledModel(dir, ss)
    }

    def run(
      model: CmdStanCompiledModel,
      chains: Int,
      seed: Int,
      cache: Boolean,
      method: RunMethod.Method
    ): StanResults = {
      // Emit the data file.
      val dataFileName = getNextDataFileName
      println(s"writing data to $dataFileName")
      val dataWriter = ShaWriter(new PrintWriter(new File(s"${model.dir}/$dataFileName")))
      model.emitData(dataWriter)
      dataWriter.close()
      val runHash = dataWriter.sha.update(method.toString).digest

      val baseSeed = if (seed < 0) (System.currentTimeMillis % Int.MaxValue).toInt else seed
      val results = (0 until chains).par.flatMap { i =>
        val chainSeed = baseSeed + i
        val name = s"$runHash-$seed-$i.csv"
        val fileName = s"${model.dir}/$name"
        if (cache && new File(fileName).exists) {
          println(s"Found cached results: $name")
          Some(readIterations(fileName))
        } else {
          val command = Vector(
            s"./$modelExecutable",
            "data", s"file=$dataFileName",
            "output", s"file=$name",
            "random", s"seed=$chainSeed"
          ) ++ method.arguments
          println("Running " + command.mkString(" "))
          val pb = new ProcessBuilder(command: _*).directory(model.dir).inheritIO()
          val rc = pb.start().waitFor()
          if (rc != 0) {
            println(s"ERROR: model returned $rc")
            None
          } else {
            Some(readIterations(fileName))
          }
        }
      }.seq.toVector

      StanResults(results, model)
    }
  }
}
