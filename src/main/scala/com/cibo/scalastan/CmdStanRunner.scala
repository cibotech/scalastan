package com.cibo.scalastan

import scala.collection.JavaConverters._
import java.io._
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

/** A compiled model for CmdStan. */
case class CmdStanCompiledModel(
  dir: File,
  model: ScalaStan#Model,
  dataMapping: Map[String, DataMapping[_]] = Map.empty
) extends CompiledModel {
  def replaceMapping(newMapping: Map[String, DataMapping[_]]): CmdStanCompiledModel = copy(dataMapping = newMapping)
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

object CmdStanRunner extends StanRunner[CmdStanCompiledModel] {

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

  private def runStanc(dir: File): Unit = {
    val stancCommand = stanc.getOrElse {
      throw new IllegalStateException(s"Could not locate stanc.")
    }
    val pb = new ProcessBuilder(stancCommand, stanFileName).directory(dir).inheritIO()
    val rc = pb.start().waitFor()
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
    val pb = new ProcessBuilder(makeCommand, target).directory(new File(stanPath)).inheritIO()
    val rc = pb.start().waitFor()
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

  def compile(ss: ScalaStan, model: ScalaStan#Model): CmdStanCompiledModel = {
    val stanPath = stanHome.getOrElse {
      throw new IllegalStateException("Could not locate Stan.")
    }
    println(s"found stan in $stanPath")

    model.synchronized {
      val dir = model.generate
      if (new File(s"${dir.getPath}/$modelExecutable").canExecute) {
        println("found existing executable")
      } else {
        runStanc(dir)
        runMake(dir)
      }
      CmdStanCompiledModel(dir, model)
    }
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
        val cachedResults = if (cache && new File(fileName).exists) {
          Some(readIterations(fileName))
        } else None
        cachedResults match {
          case Some(r) if r.nonEmpty =>
            println(s"Found cached results: $name")
            Some(r)
          case _                     =>
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

      val parameterChains: Map[String, Vector[Vector[String]]] = results.flatten.groupBy(_._1).mapValues(_.map(_._2))
      StanResults(parameterChains, model)
    }
  }
}
