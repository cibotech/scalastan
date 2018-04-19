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

import java.io._
import java.nio.file.{Files, Path, Paths}

import com.cibo.scalastan.ast._
import com.cibo.scalastan.transform.{LoopChecker, StanTransform}

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

trait ScalaStan extends Implicits { ss =>

  type ParameterDeclaration[T <: StanType] = StanParameterDeclaration[T]
  type DataDeclaration[T <: StanType] = StanDataDeclaration[T]

  protected object stan extends StanFunctions with StanDistributions

  // Maximum number of models to cache.
  protected val maxCacheSize: Int = 100

  private val modelExecutable: String = "model"
  private val stanFileName: String = s"$modelExecutable.stan"

  protected implicit val _scalaStan: ScalaStan = this

  private[scalastan] var identifiers: Set[String] = Set.empty

  private var idCounter: Int = 0

  private[scalastan] def nextId: Int = {
    synchronized {
      idCounter += 1
      idCounter
    }
  }

  def data[T <: StanType](typeConstructor: T): DataDeclaration[T] = StanDataDeclaration[T](typeConstructor)

  def parameter[T <: StanType](
    typeConstructor: T
  )(implicit ev: T#ELEMENT_TYPE =:= StanReal): ParameterDeclaration[T] = {
    StanParameterDeclaration[T](typeConstructor)
  }

  def int(
    lower: Option[StanValue[StanInt]] = None,
    upper: Option[StanValue[StanInt]] = None
  ): StanInt = StanInt(lower, upper)

  def categorical(): StanCategorical = StanCategorical()

  def real(
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanReal = StanReal(lower, upper)

  def vector(
    dim: StanValue[StanInt],
    lower: Option[StanValue[StanReal]] = None,
    upper: Option[StanValue[StanReal]] = None
  ): StanVector = StanVector(dim, lower, upper)

  def simplex(dim: StanValue[StanInt]): StanVector =
    StanVector(dim, constraint = VectorConstraint.Simplex)

  def unitVector(dim: StanValue[StanInt]): StanVector =
    StanVector(dim, constraint = VectorConstraint.UnitVector)

  def ordered(dim: StanValue[StanInt]): StanVector =
    StanVector(dim, constraint = VectorConstraint.Ordered)

  def positiveOrdered(dim: StanValue[StanInt]): StanVector =
    StanVector(dim, constraint = VectorConstraint.PositiveOrdered)

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

  def corrMatrix(
    dim: StanValue[StanInt]
  ): StanMatrix = StanMatrix(dim, dim, constraint = MatrixConstraint.CorrMatrix)

  def choleskyFactorCorr(
    dim: StanValue[StanInt]
  ): StanMatrix = StanMatrix(dim, dim, constraint = MatrixConstraint.CholeskyFactorCorr)

  def covMatrix(
    dim: StanValue[StanInt]
  ): StanMatrix = StanMatrix(dim, dim, constraint = MatrixConstraint.CovMatrix)

  def choleskyFactorCov(
    dim: StanValue[StanInt]
  ): StanMatrix = StanMatrix(dim, dim, constraint = MatrixConstraint.CholeskyFactorCov)

  implicit def compile[M <: CompiledModel](model: Model)(implicit runner: StanRunner[M]): CompiledModel = model.compile

  trait StanCode {

    private[scalastan] implicit val _code: CodeBuilder = new CodeBuilder

    implicit def dataTransform2Value[T <: StanType](transform: TransformedData[T]): StanLocalDeclaration[T] = {
      _code.append(transform)
      transform.result
    }

    implicit def paramTransform2Value[T <: StanType](transform: TransformedParameter[T]): ParameterDeclaration[T] = {
      _code.append(transform)
      transform.result
    }

    implicit def generatedQuntity2Value[T <: StanType](quantity: GeneratedQuantity[T]): ParameterDeclaration[T] = {
      _code.append(quantity)
      quantity.result
    }

    def local[T <: StanType](typeConstructor: T): StanLocalDeclaration[T] = {
      if (typeConstructor.lower.isDefined || typeConstructor.upper.isDefined) {
        throw new IllegalStateException("local variables may not have constraints")
      }

      val decl = StanLocalDeclaration[T](typeConstructor)
      _code.insert(StanInlineDeclaration(decl))
      decl
    }

    case class when(cond: StanValue[StanInt])(block: => Unit) {
      _code.enter()
      block
      _code.leave(code => ast.StanIfStatement(Seq((cond, StanBlock(code))), None))

      def when(cond: StanValue[StanInt])(otherBlock: => Unit): when = {
        _code.enter()
        otherBlock
        _code.handleElseIf(cond)
        this
      }

      def otherwise(otherBlock: => Unit): Unit = {
        _code.enter()
        otherBlock
        _code.handleElse()
      }
    }

    def range(start: StanValue[StanInt], end: StanValue[StanInt]): StanValueRange = StanValueRange(start, end)

    def loop(cond: StanValue[StanInt])(body: => Unit): Unit = {
      _code.enter()
      body
      _code.leave(children => ast.StanWhileLoop(cond, StanBlock(children)))
    }

    def break: Unit = {
      _code.append(StanBreakStatement())
    }

    def continue: Unit = {
      _code.append(StanContinueStatement())
    }

    private[ScalaStan] def emitTopLevelLocals(writer: PrintWriter): Unit = {
      // Values have to be declared before code.  Since we treat transformations
      // differently, we need to make a special pass to combine the top-level locals.
      _code.results.children.foreach { child =>
        if (child.isInstanceOf[StanInlineDeclaration]) {
          child.emit(writer, 1)
        }
      }
    }

    private[ScalaStan] def emitCode(writer: PrintWriter): Unit = {
      _code.results.children.foreach { child =>
        if (!child.isInstanceOf[StanInlineDeclaration]) {
          child.emit(writer, 1)
        }
      }
    }
  }

  abstract class Function[RETURN_TYPE <: StanType](returnType: RETURN_TYPE = StanVoid()) extends StanCode with StanFunction {

    private[scalastan] val result = StanLocalDeclaration[RETURN_TYPE](returnType)
    val name: String = result.emit

    private val inputs = new ArrayBuffer[StanLocalDeclaration[_ <: StanType]]()

    def input[T <: StanType](typeConstructor: T): StanLocalDeclaration[T] = {
      val decl = StanLocalDeclaration[T](typeConstructor)
      inputs += decl
      decl
    }

    def output(value: StanValue[RETURN_TYPE]): Unit = {
      _code.append(StanReturnStatement(value))
    }

    def apply(args: StanValue[_ <: StanType]*)(implicit code: CodeBuilder): StanCall[RETURN_TYPE] = {
      val node = StanCall[RETURN_TYPE](returnType, this, args)
      if (returnType == StanVoid()) {
        code.append(StanValueStatement(node))
      }
      node
    }

    private[scalastan] def export(builder: CodeBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanFunctionDeclaration = StanFunctionDeclaration(
      result,
      inputs,
      _code.results
    )
  }

  abstract class TransformBase[T <: StanType, D <: StanDeclaration[T]] extends StanCode with NameLookup {
    val result: D
    protected def _userName: Option[String] = NameLookup.lookupName(this)(ss)
    protected val _ss: ScalaStan = ss
  }

  abstract class TransformedData[T <: StanType](typeConstructor: T) extends TransformBase[T, StanLocalDeclaration[T]] {
    lazy val result: StanLocalDeclaration[T] = StanLocalDeclaration[T](
      typeConstructor, () => _userName, derivedFromData = true
    )

    private[scalastan] lazy val generate: StanTransformedData = StanTransformedData(result, _code.results)
  }

  abstract class TransformedParameter[T <: StanType](
    typeConstructor: T
  ) extends TransformBase[T, StanParameterDeclaration[T]] {
    lazy val result: StanParameterDeclaration[T] =
      StanParameterDeclaration[T](typeConstructor, () => _userName, transformed = true)
    protected implicit val _parameterTransform: InParameterTransform = InParameterTransform

    private[scalastan] lazy val generate: StanTransformedParameter = StanTransformedParameter(result, _code.results)
  }

  abstract class GeneratedQuantity[T <: StanType](typeConstructor: T) extends TransformBase[T, StanParameterDeclaration[T]] {
    lazy val result: StanParameterDeclaration[T] =
      StanParameterDeclaration[T](typeConstructor, () => _userName, transformed = true)
    protected implicit val _generatedQuantity: InGeneratedQuantityBlock = InGeneratedQuantityBlock

    private[scalastan] lazy val generate: StanGeneratedQuantity = StanGeneratedQuantity(result, _code.results)
  }

  /** Trait providing the Stan Model DSL. */
  trait Model extends StanCode {

    // Log probability function.
    final protected def target: StanTargetValue = StanTargetValue()

    def emit(writer: PrintWriter): Unit = TransformedModel(this).emit(writer)

    private[scalastan] def program: StanProgram = _code.program

    final def emit(ps: PrintStream): Unit = {
      val pw = new PrintWriter(ps)
      emit(pw)
      pw.flush()
    }

    private[scalastan] def getCode: String = {
      val writer = new StringWriter()
      emit(new PrintWriter(writer))
      writer.close()
      writer.toString
    }

    private[scalastan] def getBasePath: Path = {
      Option(System.getenv("HOME")) match {
        case Some(home) => Paths.get(home).resolve(".scalastan")
        case None       => Paths.get("/tmp").resolve("scalastan")
      }
    }

    private[scalastan] def cleanOldModels(base: Path, hash: String): Unit = {
      if (base.toFile.exists) {
        val dirs = base.toFile.listFiles.filter { f =>
          f.isDirectory && f.getName != hash
        }.sortBy(_.lastModified).dropRight(maxCacheSize - 1)
        dirs.foreach { dir =>
          println(s"removing old cache directory ${dir.getAbsolutePath}")
          try {
            dir.listFiles.foreach(_.delete())
            dir.delete()
          } catch {
            case ex: Exception =>
              println(s"unable to remove cache directory: $ex")
          }
        }
      }
    }

    private[scalastan] def generate: File = {
      val str = getCode
      println("code:")
      println(str)

      val hash = SHA.hash(str)
      val base = getBasePath
      cleanOldModels(base, hash)
      val dir = base.resolve(hash).toFile
      println(s"writing code to $dir")

      if (!dir.exists || !dir.listFiles().exists(f => f.getName == stanFileName && f.canExecute)) {
        Files.createDirectories(dir.toPath)
        val codeFile = new File(s"${dir.getPath}/$stanFileName")
        val codeWriter = new PrintWriter(codeFile)
        codeWriter.print(str)
        codeWriter.close()
      }
      dir
    }

    def transform(t: StanTransform): Model = TransformedModel(this).transform(t)

    def compile[M <: CompiledModel](implicit runner: StanRunner[M]): CompiledModel =
      TransformedModel(this).compile(runner)
  }

  case class TransformedModel private (
    model: Model,
    transforms: Seq[StanTransform] = Seq(new LoopChecker)
  ) extends Model {
    override final def transform(t: StanTransform): TransformedModel = TransformedModel(model, transforms :+ t)

    override private[scalastan] final def program: StanProgram =
      transforms.foldLeft(model.program) { (prev, t) => t.run(prev) }

    override final def emit(writer: PrintWriter): Unit = program.emit(writer)

    override final def compile[M <: CompiledModel](implicit runner: StanRunner[M]): CompiledModel =
      runner.compile(ss, model)
  }

  private case class BlackBoxModel private (
    private val model: String
  ) extends Model {

    override def emit(pw: PrintWriter): Unit = {
      pw.write(model)
    }

    override private[scalastan] def generate: File = {
      val hash = SHA.hash(model)
      val base = getBasePath
      cleanOldModels(base, hash)
      val dir = base.resolve(hash).toFile
      println(s"writing code to $dir")

      if (!dir.exists || !dir.listFiles().exists(f => f.getName == stanFileName && f.canExecute)) {
        Files.createDirectories(dir.toPath)
        val codeFile = new File(s"${dir.getPath}/$stanFileName")
        val codeWriter = new PrintWriter(codeFile)
        codeWriter.write(model)
        codeWriter.close()
      }
      dir
    }
  }

  object Model {
    /** Create a model from Stan code. */
    def loadFromString(model: String): Model = BlackBoxModel(model)

    /** Create a model from a Stan file. */
    def loadFromFile(path: String): Model = BlackBoxModel(scala.io.Source.fromFile(path).getLines.mkString("\n"))
  }
}
