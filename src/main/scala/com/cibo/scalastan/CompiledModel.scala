package com.cibo.scalastan

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import scala.collection.JavaConverters._

case class CompiledModel private[scalastan] (
  dir: File,
  code: ScalaStan,
  dataMapping: Map[String, DataMapping[_]] = Map.empty
) {
  private def emitData(fileName: String): Unit = {
    val dataFile = new File(s"$dir/$fileName")
    val dataWriter = new PrintWriter(dataFile)
    code.dataValues.foreach { value =>
      val mapping = dataMapping.getOrElse(value.emit,
        throw new IllegalStateException(s"no data provided for ${value.emit}")
      )
      dataWriter.println(mapping.emit)
    }
    dataWriter.close()
  }

  def withData[T <: StanType, V](
    decl: StanDataDeclaration[T],
    data: V
  )(implicit ev: V =:= T#SCALA_TYPE): CompiledModel = {
    val conv = data.asInstanceOf[T#SCALA_TYPE]
    copy(dataMapping = dataMapping.updated(decl.emit, DataMapping[T](decl, conv)))
  }

  def withData[T <: StanType, V](
    value: (StanDataDeclaration[T], V)
  )(implicit ev: V =:= T#SCALA_TYPE): CompiledModel = withData(value._1, value._2)

  private def processOutput(fileName: String): StanResults = {
    val reader = new BufferedReader(new FileReader(s"$dir/$fileName"))
    try {
      val lines = reader.lines.iterator.asScala.filterNot(_.startsWith("#")).toVector
      val header: Seq[String] = lines.head.split(",")
      val values = lines.tail.map { sample =>
        header.zip(sample.split(",")).toMap
      }
      StanResults(values)
    } finally {
      reader.close()
    }
  }

  def run(method: RunMethod = SampleMethod()): StanResults = {
    val dataFileName = CompiledModel.getNextDataFileName
    val outputFileName = CompiledModel.getNextOutputFileName

    code.dataValues.filterNot(v => dataMapping.contains(v.emit)).foreach { v =>
      throw new IllegalStateException(s"data not supplied for ${v.emit}")
    }

    println(s"writing data to $dataFileName")
    emitData(dataFileName)

    val command = Vector(
      "./code",
      "data", s"file=$dataFileName",
      "output", s"file=$outputFileName"
    ) ++ method.arguments
    println("Running " + command.mkString(" "))
    val pb = new ProcessBuilder(command: _*).directory(dir).inheritIO()
    val rc = pb.start().waitFor()
    if (rc != 0) {
      throw new IllegalStateException(s"model returned $rc")
    }

    processOutput(outputFileName)
  }
}

object CompiledModel {
  private var dataFileIndex: Int = 0
  private var outputFileIndex: Int = 0

  private def getNextDataFileName: String = {
    dataFileIndex += 1
    s"data$dataFileIndex.R"
  }

  private def getNextOutputFileName: String = {
    outputFileIndex += 1
    s"output$dataFileIndex.csv"
  }
}
