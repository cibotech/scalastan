package com.cibo.scalastan

import java.io.{PrintStream, PrintWriter}

import scala.language.implicitConversions

import com.cibo.scalastan.ast._
import com.cibo.scalastan.run.StanCompiler
import com.cibo.scalastan.transform.{LoopChecker, StanTransform}
import scala.collection.mutable.ArrayBuffer

trait StanModel extends StanCodeBlock with StanContext { self =>

  // Maximum number of models to cache.
  val maxCacheSize: Int = 100

  // Generated quantities aren't referenced from the model, so we need some way to cause them
  // to be generated.  To deal with this, generated quantities are created inside the model
  // so that we can track them and generate them when the model is generated.
  private val generatedQuantities: ArrayBuffer[GeneratedQuantity[_]] = new ArrayBuffer[GeneratedQuantity[_]]()

  // Code loaded from a file or string.
  private var rawCode: Option[String] = None

  def data[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): StanDataDeclaration[T] = {
    StanDataDeclaration[T](typeConstructor, fixName(name.value))
  }

  def parameter[T <: StanType](
    typeConstructor: T
  )(
    implicit ev: T#ELEMENT_TYPE =:= StanReal,
    name: sourcecode.Name
  ): StanParameterDeclaration[T] = {
    StanParameterDeclaration[T](typeConstructor, fixName(name.value))
  }

  def int(
    lower: StanValue[StanInt] = StanUnknownInt,
    upper: StanValue[StanInt] = StanUnknownInt
  ): StanInt = StanInt(StanUnknown.boundOpt(lower), StanUnknown.boundOpt(upper))

  def categorical(): StanCategorical = StanCategorical()

  def real(
    lower: StanValue[StanReal] = StanUnknownReal,
    upper: StanValue[StanReal] = StanUnknownReal
  ): StanReal = StanReal(StanUnknown.boundOpt(lower), StanUnknown.boundOpt(upper))

  def vector(
    dim: StanValue[StanInt] = StanUnknownInt,
    lower: StanValue[StanReal] = StanUnknownReal,
    upper: StanValue[StanReal] = StanUnknownReal
  ): StanVector = StanVector(dim, StanUnknown.boundOpt(lower), StanUnknown.boundOpt(upper))

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
    lower: StanValue[StanReal] = StanUnknownReal,
    upper: StanValue[StanReal] = StanUnknownReal
  ): StanRowVector = StanRowVector(dim, StanUnknown.boundOpt(lower), StanUnknown.boundOpt(upper))

  def matrix(
    rows: StanValue[StanInt],
    cols: StanValue[StanInt],
    lower: StanValue[StanReal] = StanUnknownReal,
    upper: StanValue[StanReal] = StanUnknownReal
  ): StanMatrix = StanMatrix(rows, cols, StanUnknown.boundOpt(lower), StanUnknown.boundOpt(upper))

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

  def choleskyFactorCov(
    rows: StanValue[StanInt],
    cols: StanValue[StanInt]
  ): StanMatrix = StanMatrix(rows, cols, constraint = MatrixConstraint.CholeskyFactorCov)

  abstract class Function[RETURN_TYPE <: StanType](
    returnType: RETURN_TYPE = StanVoid()
  )(
    implicit sourceName: sourcecode.Enclosing
  ) extends StanTransformBase[RETURN_TYPE, StanLocalDeclaration[RETURN_TYPE]] with StanFunction {

    implicit val _context: StanContext = self._context

    val name: String = fixEnclosingName(sourceName.value)

    lazy val result: StanLocalDeclaration[RETURN_TYPE] =
      StanLocalDeclaration[RETURN_TYPE](returnType, name, owner = Some(this))

    private val inputs = new ArrayBuffer[StanLocalDeclaration[_ <: StanType]]()

    def input[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): StanLocalDeclaration[T] = {
      val decl = StanLocalDeclaration[T](typeConstructor, fixName(name.value))
      inputs += decl
      decl
    }

    def output(value: StanValue[RETURN_TYPE]): Unit = {
      _code.append(StanReturnStatement(value))
    }

    def apply(args: StanValue[_ <: StanType]*)(implicit code: StanProgramBuilder): StanCall[RETURN_TYPE] = {
      val node = StanCall[RETURN_TYPE](returnType, this, args)
      if (returnType == StanVoid()) {
        code.append(StanValueStatement(node))
      }
      node
    }

    def export(builder: StanProgramBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanFunctionDeclaration = StanFunctionDeclaration(
      result,
      inputs,
      _code.results
    )
  }

  abstract class TransformedData[T <: StanType](typeConstructor: T)(
    implicit sourceName: sourcecode.Enclosing
  ) extends StanTransformBase[T, StanLocalDeclaration[T]] {

    implicit val _context: StanContext = self._context

    val name: String = fixEnclosingName(sourceName.value)

    protected implicit val _rngAvailable: RngAvailable = RngAvailable

    lazy val result: StanLocalDeclaration[T] = StanLocalDeclaration[T](
      typeConstructor, name, derivedFromData = true, owner = Some(this)
    )

    def export(builder: StanProgramBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanTransformedData = StanTransformedData(result, _code.results)
  }

  abstract class TransformedParameter[T <: StanType](
    typeConstructor: T
  )(
    implicit sourceName: sourcecode.Enclosing
  ) extends StanTransformBase[T, StanParameterDeclaration[T]] {

    implicit val _context: StanContext = self._context

    val name: String = fixEnclosingName(sourceName.value)

    lazy val result: StanParameterDeclaration[T] =
      StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))
    protected implicit val _parameterTransform: InParameterTransform = InParameterTransform

    def export(builder: StanProgramBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanTransformedParameter = StanTransformedParameter(result, _code.results)
  }

  abstract class GeneratedQuantity[T <: StanType](
    typeConstructor: T
  )(
    implicit sourceName: sourcecode.Enclosing
  ) extends StanTransformBase[T, StanParameterDeclaration[T]] {

    implicit val _context: StanContext = self._context

    val name: String = fixEnclosingName(sourceName.value)

    lazy val result: StanParameterDeclaration[T] = {
      if (!generatedQuantities.exists(_.name == name)) {
        generatedQuantities += this
      }
      StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))
    }
    StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))

    protected implicit val _rngAvailable: RngAvailable = RngAvailable

    def export(builder: StanProgramBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanGeneratedQuantity = StanGeneratedQuantity(result, _code.results)
  }

  // Log probability function.
  final protected def target: StanTargetValue = StanTargetValue()

  def emit(writer: PrintWriter): Unit = {
    rawCode match {
      case Some(raw) => writer.print(raw)
      case None      => TransformedModel(this).emit(writer)
    }
  }

  private[scalastan] def program: StanProgram = {
    generatedQuantities.foreach(_code.append)
    _code.program
  }

  final def emit(ps: PrintStream): Unit = {
    val pw = new PrintWriter(ps)
    emit(pw)
    pw.flush()
  }

  case class TransformedModel private (
    model: StanModel,
    transforms: Seq[StanTransform[_]] = Seq(new LoopChecker)
  ) extends StanModel {
    override final def transform(t: StanTransform[_]): TransformedModel = TransformedModel(model, transforms :+ t)

    override private[scalastan] final def program: StanProgram = {
      transforms.foldLeft(model.program) { (prev, t) => t.run(prev) }
    }

    override final def emit(writer: PrintWriter): Unit = {
      model.rawCode match {
        case Some(raw) => model.emit(writer)
        case None      => program.emit(writer)
      }
    }

    override final def compile(implicit compiler: StanCompiler): CompiledModel = compiler.compile(this)
  }

  def transform(t: StanTransform[_]): StanModel = TransformedModel(this).transform(t)

  def compile(implicit compiler: StanCompiler): CompiledModel = TransformedModel(this).compile(compiler)

  /** Create a model from Stan code. */
  def loadFromString(code: String): Unit = {
    require(rawCode.isEmpty, "Code loaded multiple times")
    rawCode = Some(code)
  }

  /** Create a model from a Stan file. */
  def loadFromFile(path: String): Unit = loadFromString(scala.io.Source.fromFile(path).getLines.mkString("\n"))

  implicit def dataTransform2Value[T <: StanType](
    transform: StanModel#TransformedData[T]
  ): StanLocalDeclaration[T] = transform.result

  implicit def paramTransform2Value[T <: StanType](
    transform: StanModel#TransformedParameter[T]
  ): StanParameterDeclaration[T] = transform.result

  implicit def generatedQuantity2Value[T <: StanType](
    quantity: StanModel#GeneratedQuantity[T]
  ): StanParameterDeclaration[T] = quantity.result
}

object StanModel {
  implicit def compile(model: StanModel)(implicit compiler: StanCompiler): CompiledModel = model.compile(compiler)
}
