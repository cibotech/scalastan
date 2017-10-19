package com.cibo.scalastan

import java.io._
import java.nio.file.Files

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

trait ScalaStan extends Implicits { stan =>

  private[scalastan] val dataValues = ArrayBuffer[StanDeclaration[_, DataDeclarationType]]()
  private val parameterValues = ArrayBuffer[StanDeclaration[_, ParameterDeclarationType]]()
  private val functions = ArrayBuffer[Function[_]]()
  private val dataTransforms = ArrayBuffer[DataTransform[_]]()
  private val parameterTransforms = ArrayBuffer[ParameterTransform[_]]()

  def data[T <: StanType](typeConstructor: T): StanDeclaration[T, DataDeclarationType] = {
    val v = StanDeclaration[T, DataDeclarationType](typeConstructor)
    dataValues += v
    v
  }

  def parameter[T <: StanType](typeConstructor: T): StanDeclaration[T, ParameterDeclarationType] = {
    val v = StanDeclaration[T, ParameterDeclarationType](typeConstructor)
    parameterValues += v
    v
  }

  def int(
    lower: Option[StanValue[StanInt]] = None,
    upper: Option[StanValue[StanInt]] = None
  ): StanInt = StanInt(lower, upper)

  def real(
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanReal = StanReal(lower, upper)

  def vector(
    dim: StanValue[StanInt],
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanVector = StanVector(dim, lower, upper)

  def rowVector(
    dim: StanValue[StanInt],
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanRowVector = StanRowVector(dim, lower, upper)

  def matrix(
    rows: StanValue[StanInt],
    cols: StanValue[StanInt],
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanMatrix = StanMatrix(rows, cols, lower, upper)

  def array[T <: StanType](
    dim: StanValue[StanInt],
    inner: T
  ): StanArray[T] = StanArray(dim, inner)

  implicit def dataTransform2Value[T <: StanType](transform: DataTransform[T]): StanValue[T] = {
    if (!dataTransforms.exists(_.result.emit == transform.result.emit)) {
      dataTransforms += transform
    }
    transform.result
  }

  implicit def paramTransform2Value[T <: StanType](transform: ParameterTransform[T]): StanValue[T] = {
    if (!parameterTransforms.exists(_.result.emit == transform.result.emit)) {
      parameterTransforms += transform
    }
    transform.result
  }

  implicit def compile(model: Model): CompiledModel = model.compile

  trait StanCode extends StanBuiltInFunctions with StanDistributions {

    protected implicit val code: ArrayBuffer[StanValue[_]] = ArrayBuffer[StanValue[_]]()

    def local[T <: StanType](typeConstructor: T): StanDeclaration[T, LocalDeclarationType] = {
      if (typeConstructor.lower.isDefined || typeConstructor.upper.isDefined) {
        throw new IllegalStateException("local variables may not have constraints")
      }
      val decl = StanDeclaration[T, LocalDeclarationType](typeConstructor)
      code += StanInlineDeclaration(decl)
      decl
    }

    case class when(cond: StanValue[StanInt])(block: => Unit) {
      code += IfStatement(cond)
      block
      code += LeaveScope

      def when(otherCond: StanValue[StanInt])(otherBlock: => Unit): when = {
        code += ElseIfStatement(otherCond)
        otherBlock
        code += LeaveScope
        this
      }

      def otherwise(otherBlock: => Unit): Unit = {
        code += ElseStatement
        otherBlock
        code += LeaveScope
      }
    }

    def range(start: StanValue[StanInt], end: StanValue[StanInt]): ValueRange = ValueRange(start, end)

    private[ScalaStan] def emitCode(writer: PrintWriter): Unit = {
      code.foreach { c =>
        writer.println(s"  ${c.emit}${c.terminator}")
      }
    }
  }

  abstract class Function[RETURN_TYPE <: StanType](returnType: RETURN_TYPE = StanVoid()) extends StanCode {

    private val result = StanDeclaration[RETURN_TYPE, LocalDeclarationType](returnType)
    private val inputs = new ArrayBuffer[StanDeclaration[_, LocalDeclarationType]]()

    def input[T <: StanType](typeConstructor: T): StanDeclaration[T, LocalDeclarationType] = {
      val decl = StanDeclaration[T, LocalDeclarationType](typeConstructor)
      inputs += decl
      decl
    }

    def output(value: StanValue[RETURN_TYPE]): Unit = {
      code += ReturnNode(value)
    }

    def apply(args: StanValue[_]*): FunctionNode[RETURN_TYPE] = {
      val name = result.emit
      if (!functions.exists(_.result.emit == name)) {
        functions += this
      }
      val node = FunctionNode[RETURN_TYPE](name, args)
      if (returnType == StanVoid()) {
        code += node
      }
      node
    }

    private[ScalaStan] def emit(writer: PrintWriter): Unit = {
      val params = inputs.map(_.emitDeclaration).mkString(",")
      writer.println(s"${returnType.typeName} ${result.emit}($params) {")
      emitCode(writer)
      writer.println("}")
    }
  }

  abstract class DataTransform[T <: StanType](typeConstructor: T) extends StanCode {
    val result: StanDeclaration[T, DataDeclarationType] =
      StanDeclaration[T, DataDeclarationType](typeConstructor)
  }

  abstract class ParameterTransform[T <: StanType](typeConstructor: T) extends StanCode {
    val result: StanDeclaration[T, ParameterDeclarationType] =
      StanDeclaration[T, ParameterDeclarationType](typeConstructor)
  }

  trait Model extends StanCode {

    // Log probability function.
    def target: StanValue[StanReal] = FunctionNode("target", Seq())

    private def emit(writer: PrintWriter): Unit = {

      writer.println("functions {")
      functions.foreach(f => f.emit(writer))
      writer.println("}")

      writer.println("data {")
      dataValues.foreach { v =>
        writer.println(s"  ${v.emitDeclaration};")
      }
      writer.println("}")

      writer.println("transformed data {")
      dataTransforms.foreach { v =>
        writer.println(s"  ${v.result.emitDeclaration};")
      }
      dataTransforms.foreach(t => t.emitCode(writer))
      writer.println("}")

      writer.println("parameters {")
      parameterValues.foreach { v =>
        writer.println(s"  ${v.emitDeclaration};")
      }
      writer.println("}")

      writer.println("transformed parameters {")
      parameterTransforms.foreach { v =>
        writer.println(s"  ${v.result.emitDeclaration};")
      }
      parameterTransforms.foreach(t => t.emitCode(writer))
      writer.println("}")

      writer.println("model {")
      emitCode(writer)
      writer.println("}")
    }

    private[scalastan] def getCode: String = {
      val writer = new StringWriter()
      emit(new PrintWriter(writer))
      writer.close()
      writer.toString
    }

    private def generate: File = {
      val str = getCode
      println(str)

      val md = java.security.MessageDigest.getInstance("SHA-1")
      val hashBytes = md.digest(str.getBytes).flatMap { b =>
        val digits = "0123456789abcdef"
        digits((b.toInt & 255) >> 4).toString + digits(b.toInt & 15).toString
      }
      val hash = new String(hashBytes)

      val dirName = s"/tmp/scalastan/$hash"
      val dir = new File(dirName)
      println(s"writing code to $dir")

      if (!dir.exists) {
        Files.createDirectories(dir.toPath)
        val codeFile = new File(s"${dir.getPath}/code.stan")
        val codeWriter = new PrintWriter(codeFile)
        emit(codeWriter)
        codeWriter.close()
      }
      dir
    }

    private def compile(dir: File): Unit = {
      val pb = new ProcessBuilder("stanc", "code.stan").directory(dir).inheritIO()
      val rc = pb.start().waitFor()
      if (rc != 0) {
        throw new IllegalStateException(s"stanc returned $rc")
      }
    }

    private def build(dir: File, stanDir: String): Unit = {
      val target = s"$dir/code"
      val pb = new ProcessBuilder("make", target).directory(new File(stanDir)).inheritIO()
      val rc = pb.start().waitFor()
      if (rc != 0) {
        throw new IllegalStateException(s"make returned $rc")
      }
    }

    private def findStan: String = {
      val pb = new ProcessBuilder("which", "stanc")
      val process = pb.start()
      val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
      val rc = process.waitFor()
      if (rc != 0) {
        throw new IllegalStateException(s"stanc not found")
      }
      val stancFile = new File(reader.lines.iterator.asScala.toStream.last).getCanonicalFile
      stancFile.getParentFile.getParentFile.getAbsolutePath
    }

    def compile: CompiledModel = {
      val stanDir = findStan
      println(s"found stan in $stanDir")

      // Generate the code.
      val dir = generate

      if (new File(s"${dir.getPath}/code").canExecute) {
        println("found existing executable")
      } else {
        compile(dir)
        build(dir, stanDir)
      }

      new CompiledModel(dir, stan)
    }
  }
}
