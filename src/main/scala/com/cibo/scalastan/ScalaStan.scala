package com.cibo.scalastan

import java.io._
import java.nio.file.Files

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}

trait ScalaStan extends Implicits { stan =>

  protected implicit val _scalaStan: ScalaStan = this

  private[scalastan] val dataValues = ArrayBuffer[StanDataDeclaration[_]]()
  private val parameterValues = ArrayBuffer[StanParameterDeclaration[_]]()
  private val functions = ArrayBuffer[Function[_]]()
  private val dataTransforms = ArrayBuffer[DataTransform[_]]()
  private val parameterTransforms = ArrayBuffer[ParameterTransform[_]]()
  private val generatedQuantities = ArrayBuffer[GeneratedQuantity[_]]()

  private[scalastan] def parameters: Seq[StanParameterDeclaration[_]] =
    parameterValues ++ parameterTransforms.map(_.result) ++ generatedQuantities.map(_.result)

  def data[T <: StanType](typeConstructor: T): StanDataDeclaration[T] = {
    val v = StanDataDeclaration[T](typeConstructor)
    dataValues += v
    v
  }

  def parameter[T <: StanType](typeConstructor: T): StanParameterDeclaration[T] = {
    val v = StanParameterDeclaration[T](typeConstructor)
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

  implicit def dataTransform2Value[T <: StanType](transform: DataTransform[T]): StanLocalDeclaration[T] = {
    transform.result
  }

  implicit def paramTransform2Value[T <: StanType](transform: ParameterTransform[T]): StanParameterDeclaration[T] = {
    transform.result
  }

  implicit def generatedQuntity2Value[T <: StanType](quantity: GeneratedQuantity[T]): StanParameterDeclaration[T] = {
    quantity.result
  }

  implicit def compile(model: Model): CompiledModel = model.compile

  trait StanCode extends StanBuiltInFunctions with StanDistributions {

    protected implicit val _codeBuffer: ArrayBuffer[StanNode] = ArrayBuffer[StanNode]()

    def local[T <: StanType](typeConstructor: T): StanLocalDeclaration[T] = {
      if (typeConstructor.lower.isDefined || typeConstructor.upper.isDefined) {
        throw new IllegalStateException("local variables may not have constraints")
      }

      // Local declarations need to go at the top of the block.
      // Here we insert after the last EnterScope, the last declaration, or at the beginning.
      val decl = StanLocalDeclaration[T](typeConstructor)
      val insertionPoint = _codeBuffer.lastIndexWhere {
        case _: EnterScope               => true
        case _: StanInlineDeclaration[_] => true
        case _                           => false
      }
      _codeBuffer.insert(insertionPoint + 1, StanInlineDeclaration(decl))
      decl
    }

    case class when(cond: StanValue[StanInt])(block: => Unit) {
      _codeBuffer += IfStatement(cond)
      block
      _codeBuffer += LeaveScope()

      def when(otherCond: StanValue[StanInt])(otherBlock: => Unit): when = {
        _codeBuffer += ElseIfStatement(otherCond)
        otherBlock
        _codeBuffer += LeaveScope()
        this
      }

      def otherwise(otherBlock: => Unit): Unit = {
        _codeBuffer += ElseStatement
        otherBlock
        _codeBuffer += LeaveScope()
      }
    }

    def range(start: StanValue[StanInt], end: StanValue[StanInt]): ValueRange = ValueRange(start, end)

    def loop(cond: StanValue[StanInt])(body: => Unit): Unit = {
      _codeBuffer += WhileLoop(cond)
      body
      _codeBuffer += LeaveScope()
    }

    private def inLoop: Boolean = {
      _codeBuffer.foldLeft(Seq.empty[StanNode]) { (stack, node) =>
        node match {
          case _: WhileLoop      => node +: stack
          case _: ForLoop[_]     => node +: stack
          case _: IfStatement[_] => node +: stack
          case _: LeaveScope     => stack.tail
          case _                 => stack
        }
      }.exists { node =>
        node.isInstanceOf[WhileLoop] || node.isInstanceOf[ForLoop[_]]
      }
    }

    def break: Unit = {
      if (!inLoop) {
        throw new IllegalStateException("'break' must be in a loop")
      }
      _codeBuffer += BreakNode()
    }

    def continue: Unit = {
      if (!inLoop) {
        throw new IllegalStateException("'continue' must be in a loop")
      }
      _codeBuffer += ContinueNode()
    }

    private[ScalaStan] def emitCode(writer: PrintWriter): Unit = {
      val indentSpaces = 2
      var indent: Int = 0
      _codeBuffer.foreach { c =>
        if (c.isInstanceOf[LeaveScope]) {
          indent -= 1
        }
        writer.print(" " * (indentSpaces * (indent + 1)))
        writer.print(c.emit)
        writer.println(c.terminator)
        if (c.isInstanceOf[EnterScope]) {
          indent += 1
        }
      }
      require(indent == 0)
    }
  }

  abstract class Function[RETURN_TYPE <: StanType](returnType: RETURN_TYPE = StanVoid()) extends StanCode {

    private val result = StanLocalDeclaration[RETURN_TYPE](returnType)
    private val inputs = new ArrayBuffer[StanLocalDeclaration[_]]()

    def input[T <: StanType](typeConstructor: T): StanLocalDeclaration[T] = {
      val decl = StanLocalDeclaration[T](typeConstructor)
      inputs += decl
      decl
    }

    def output(value: StanValue[RETURN_TYPE]): Unit = {
      _codeBuffer += ReturnNode(value)
    }

    def apply(args: StanValue[_]*): FunctionNode[RETURN_TYPE] = {
      val name = result.emit
      if (!functions.exists(_.result.emit == name)) {
        functions += this
      }
      val node = FunctionNode[RETURN_TYPE](name, args: _*)
      if (returnType == StanVoid()) {
        _codeBuffer += node
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

  abstract class TransformBase[T <: StanType, D <: StanDeclaration[T]] extends StanCode with NameLookup {
    val result: D
    protected def _userName: Option[String] = NameLookup.lookupName(this)(stan)
  }

  abstract class DataTransform[T <: StanType](typeConstructor: T) extends TransformBase[T, StanLocalDeclaration[T]] {
    lazy val result: StanLocalDeclaration[T] = StanLocalDeclaration[T](typeConstructor, () => _userName)

    if (!dataTransforms.exists(_._id == _id)) {
      dataTransforms += this
    }
  }

  abstract class ParameterTransform[T <: StanType](typeConstructor: T) extends TransformBase[T, StanParameterDeclaration[T]] {
    lazy val result: StanParameterDeclaration[T] = StanParameterDeclaration[T](typeConstructor, () => _userName)

    if (!parameterTransforms.exists(_._id == _id)) {
      parameterTransforms += this
    }
  }

  abstract class GeneratedQuantity[T <: StanType](typeConstructor: T) extends TransformBase[T, StanParameterDeclaration[T]] {
    lazy val result: StanParameterDeclaration[T] = StanParameterDeclaration[T](typeConstructor, () => _userName)
    protected implicit val _generatedQuantity: InGeneratedQuantityBlock = InGeneratedQuantityBlock

    if (!generatedQuantities.exists(_._id == _id)) {
      generatedQuantities += this
    }
  }

  private def emitDeclarations(
    writer: PrintWriter,
    decls: Seq[StanDeclaration[_]]
  ): Unit = {
    decls.foreach { decl =>
      writer.println(s"  ${decl.emitDeclaration}; // ${decl.name}")
    }
  }

  trait Model extends StanCode {

    // Log probability function.
    def target: TargetValue = TargetValue()

    private def emit(writer: PrintWriter): Unit = {

      writer.println("functions {")
      functions.foreach(f => f.emit(writer))
      writer.println("}")

      writer.println("data {")
      emitDeclarations(writer, dataValues)
      writer.println("}")

      writer.println("transformed data {")
      emitDeclarations(writer, dataTransforms.map(_.result))
      dataTransforms.foreach(t => t.emitCode(writer))
      writer.println("}")

      writer.println("parameters {")
      emitDeclarations(writer, parameterValues)
      writer.println("}")

      writer.println("transformed parameters {")
      emitDeclarations(writer, parameterTransforms.map(_.result))
      parameterTransforms.foreach(t => t.emitCode(writer))
      writer.println("}")

      writer.println("model {")
      emitCode(writer)
      writer.println("}")

      writer.println("generated quantities {")
      emitDeclarations(writer, generatedQuantities.map(_.result))
      generatedQuantities.foreach(g => g.emitCode(writer))
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
