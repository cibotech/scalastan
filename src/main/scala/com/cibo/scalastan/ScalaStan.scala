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

import com.cibo.scalastan.ast._
import com.cibo.scalastan.run.StanCompiler
import com.cibo.scalastan.transform.{LoopChecker, StanTransform}
import com.typesafe.scalalogging.LazyLogging

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

trait ScalaStan extends Implicits with LazyLogging { ss =>

  type ParameterDeclaration[T <: StanType] = StanParameterDeclaration[T]
  type DataDeclaration[T <: StanType] = StanDataDeclaration[T]

  object stan extends StanFunctions with StanDistributions

  // Maximum number of models to cache.
  val maxCacheSize: Int = 100

  protected implicit val _scalaStan: ScalaStan = this

  private var idCounter: Int = 0

  private def nextId: Int = {
    synchronized {
      idCounter += 1
      idCounter
    }
  }

  def newName: String = s"ss_v$nextId"

  private def fixName(name: String): String = {
    val pattern = raw"[A-Za-z][A-Za-z0-9_]*".r
    name match {
      case pattern(_*) if !name.startsWith("ss_v") => name
      case _                                       => newName
    }
  }

  private def fixEnclosingName(name: String): String = {
    // These names have the form of:
    //    "com.cibo.scalastan.GeneratedQuantitySpec#$anon#model $anon#t $anon" or
    //    "com.cibo.scalastan.GeneratedQuantitySpec#$anon#t $anon"
    // where "t" is the name of interest.
    val parts = name.split(' ')
    if (parts.length > 1) {
      fixName(parts.dropRight(1).last.split('#').last.split('.').last)
    } else {
      newName
    }
  }

  def data[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): DataDeclaration[T] = {
    StanDataDeclaration[T](typeConstructor, fixName(name.value))
  }

  def parameter[T <: StanType](
    typeConstructor: T
  )(
    implicit ev: T#ELEMENT_TYPE =:= StanReal,
    name: sourcecode.Name
  ): ParameterDeclaration[T] = {
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

  trait StanCode {

    implicit val _code: CodeBuilder = new CodeBuilder

    def local[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): StanLocalDeclaration[T] = {
      if (typeConstructor.lower.isDefined || typeConstructor.upper.isDefined) {
        throw new IllegalStateException("local variables may not have constraints")
      }

      val decl = StanLocalDeclaration[T](typeConstructor, fixName(name.value))
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

  abstract class TransformBase[T <: StanType, D <: StanDeclaration[T]] extends StanCode {
    private[scalastan] val result: D
    private[scalastan] def export(builder: CodeBuilder): Unit
    val name: String
  }

  abstract class Function[RETURN_TYPE <: StanType](
    returnType: RETURN_TYPE = StanVoid()
  )(
    implicit sourceName: sourcecode.Enclosing
  ) extends TransformBase[RETURN_TYPE, StanLocalDeclaration[RETURN_TYPE]] with StanFunction {

    val name: String = fixEnclosingName(sourceName.value)

    private[scalastan] lazy val result = StanLocalDeclaration[RETURN_TYPE](
      returnType, name, owner = Some(this))

    private val inputs = new ArrayBuffer[StanLocalDeclaration[_ <: StanType]]()

    def input[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): StanLocalDeclaration[T] = {
      val decl = StanLocalDeclaration[T](typeConstructor, fixName(name.value))
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

  abstract class TransformedData[T <: StanType](typeConstructor: T)(
    implicit sourceName: sourcecode.Enclosing
  ) extends TransformBase[T, StanLocalDeclaration[T]] {

    val name: String = fixEnclosingName(sourceName.value)

    protected implicit val _rngAvailable: RngAvailable = RngAvailable

    lazy val result: StanLocalDeclaration[T] = StanLocalDeclaration[T](
      typeConstructor, name, derivedFromData = true, owner = Some(this)
    )

    private[scalastan] def export(builder: CodeBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanTransformedData = StanTransformedData(result, _code.results)
  }

  abstract class TransformedParameter[T <: StanType](
    typeConstructor: T
  )(
    implicit sourceName: sourcecode.Enclosing
  ) extends TransformBase[T, StanParameterDeclaration[T]] {

    val name: String = fixEnclosingName(sourceName.value)

    lazy val result: StanParameterDeclaration[T] =
      StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))
    protected implicit val _parameterTransform: InParameterTransform = InParameterTransform

    private[scalastan] def export(builder: CodeBuilder): Unit = builder.append(this)

    private[scalastan] lazy val generate: StanTransformedParameter = StanTransformedParameter(result, _code.results)
  }

  /** Trait providing the Stan Model DSL. */
  trait Model extends StanCode {

    // Generated quantities aren't referenced from the model, so we need some way to cause them
    // to be generated.  To deal with this, generated quantities are created inside the model
    // so that we can track them and generate them when the model is generated.
    private val generatedQuantities: ArrayBuffer[GeneratedQuantity[_]] = new ArrayBuffer[GeneratedQuantity[_]]()

    abstract class GeneratedQuantity[T <: StanType](
      typeConstructor: T
    )(
      implicit sourceName: sourcecode.Enclosing
    ) extends TransformBase[T, StanParameterDeclaration[T]] {
      val name: String = fixEnclosingName(sourceName.value)

      lazy val result: StanParameterDeclaration[T] = {
        if (!generatedQuantities.exists(_.name == name)) {
          generatedQuantities += this
        }
        StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))
      }
      StanParameterDeclaration[T](typeConstructor, name, owner = Some(this))

      protected implicit val _rngAvailable: RngAvailable = RngAvailable

      private[scalastan] def export(builder: CodeBuilder): Unit = builder.append(this)

      private[scalastan] lazy val generate: StanGeneratedQuantity = StanGeneratedQuantity(result, _code.results)
    }

    // Log probability function.
    final protected def target: StanTargetValue = StanTargetValue()

    def emit(writer: PrintWriter): Unit = TransformedModel(this).emit(writer)

    private[scalastan] def program: StanProgram = {
      generatedQuantities.foreach(_code.append)
      _code.program
    }

    final def emit(ps: PrintStream): Unit = {
      val pw = new PrintWriter(ps)
      emit(pw)
      pw.flush()
    }

    def transform(t: StanTransform[_]): Model = TransformedModel(this).transform(t)

    def compile(implicit compiler: StanCompiler): CompiledModel =
      TransformedModel(this).compile(compiler)
  }

  implicit def compile(model: Model)(implicit compiler: StanCompiler): CompiledModel = model.compile

  implicit def dataTransform2Value[T <: StanType](transform: TransformedData[T]): StanLocalDeclaration[T] = {
    transform.result
  }

  implicit def paramTransform2Value[T <: StanType](transform: TransformedParameter[T]): ParameterDeclaration[T] = {
    transform.result
  }

  implicit def generatedQuantity2Value[T <: StanType](quantity: Model#GeneratedQuantity[T]): ParameterDeclaration[T] = {
    quantity.result
  }

  case class TransformedModel private (
    model: Model,
    transforms: Seq[StanTransform[_]] = Seq(new LoopChecker)
  ) extends Model {
    override final def transform(t: StanTransform[_]): TransformedModel = TransformedModel(model, transforms :+ t)

    override private[scalastan] final def program: StanProgram = {
      transforms.foldLeft(model.program) { (prev, t) => t.run(prev) }
    }

    override final def emit(writer: PrintWriter): Unit = program.emit(writer)

    override final def compile(implicit compiler: StanCompiler): CompiledModel = compiler.compile(ss, this)
  }

  private case class BlackBoxModel private (
    private val model: String
  ) extends Model {

    override def emit(pw: PrintWriter): Unit = pw.write(model)

    override final def compile(implicit compiler: StanCompiler): CompiledModel = compiler.compile(ss, this)
  }

  object Model {
    /** Create a model from Stan code. */
    def loadFromString(model: String): Model = BlackBoxModel(model)

    /** Create a model from a Stan file. */
    def loadFromFile(path: String): Model = BlackBoxModel(scala.io.Source.fromFile(path).getLines.mkString("\n"))
  }
}
