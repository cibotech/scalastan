package com.cibo.scalastan

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import scala.collection.JavaConverters._

case class CompiledModel private[scalastan] (
  private val dir: File,
  private val code: ScalaStan,
  private val dataMapping: Map[String, DataMapping[_]] = Map.empty
) {

  private def emitData(fileName: String): SHA = {
    val dataFile = new File(s"$dir/$fileName")
    val dataWriter = ShaWriter(new PrintWriter(dataFile))
    code.dataValues.foreach { value =>
      val mapping = dataMapping.getOrElse(value.emit,
        throw new IllegalStateException(s"no data provided for ${value.emit}")
      )
      dataWriter.println(mapping.emit)
    }
    dataWriter.close()
    dataWriter.sha
  }

  def reset: CompiledModel = copy(dataMapping = Map.empty)

  def withData[T <: StanType, V](
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
    withDecls.copy(dataMapping = withDecls.dataMapping.updated(decl.emit, DataMapping[T](decl, conv)))
  }

  def withData[T <: StanType, V](
    value: (StanDataDeclaration[T], V)
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = withData(value._1, value._2)

  private def readIterations(fileName: String): Vector[Map[String, String]] = {
    val reader = new BufferedReader(new FileReader(s"$dir/$fileName"))
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

  def run(
    chains: Int = 1,
    seed: Int = -1,
    cache: Boolean = true,
    method: RunMethod.Method = RunMethod.Sample()
  ): StanResults = {
    require(chains > 0, s"Must run at least one chain")

    // Make sure all the necessary data is provided.
    code.dataValues.filterNot(v => dataMapping.contains(v.emit)).foreach { v =>
      throw new IllegalStateException(s"data not supplied for ${v.name}")
    }

    // Emit the data file.
    val dataFileName = CompiledModel.getNextDataFileName
    println(s"writing data to $dataFileName")
    val runSha = emitData(dataFileName)
    val runHash = runSha.update(method.toString).digest

    val baseSeed = if (seed < 0) (System.currentTimeMillis % Int.MaxValue).toInt else seed
    val results = (0 until chains).par.flatMap { i =>
      val chainSeed = baseSeed + i
      val name = s"$runHash-$seed-$i.csv"
      if (cache && new File(s"$dir/$name").exists) {
        println(s"Found cached results: $name")
        Some(readIterations(name))
      } else {
        val command = Vector(
          "./model",
          "data", s"file=$dataFileName",
          "output", s"file=$name",
          "random", s"seed=$chainSeed"
        ) ++ method.arguments
        println("Running " + command.mkString(" "))
        val pb = new ProcessBuilder(command: _*).directory(dir).inheritIO()
        val rc = pb.start().waitFor()
        if (rc != 0) {
          println(s"ERROR: model returned $rc")
          None
        } else {
          Some(readIterations(name))
        }
      }
    }.seq.toVector

    StanResults(results, code)
  }
}

object CompiledModel {
  private var dataFileIndex: Int = 0

  private def getNextDataFileName: String = {
    synchronized {
      dataFileIndex += 1
      s"data$dataFileIndex.R"
    }
  }
}
