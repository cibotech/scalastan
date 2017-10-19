package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

sealed abstract class StanValue[T <: StanType] extends Implicits {
  def foreach(f: StanValue[T] => Unit)(implicit ev: TemporaryValue[T], code: ArrayBuffer[StanValue[_]]): Unit = {
    val temp = ev.create()
    val decl = StanDeclaration[T, LocalDeclarationType](temp)
    code += ForLoop(decl, this)
    f(decl)
    code += LeaveScope
  }

  def emit: String

  val terminator: String = ";"

  def unary_-(): StanValue[T] = UnaryOperator("-", this)

  // Logical functions.
  def ===(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("==", this, right)
  def =/=(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("!=", this, right)
  def <(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("<", this, right)
  def <=(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("<=", this, right)
  def >(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator(">", this, right)
  def >=(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator(">=", this, right)

  // Boolean operators.
  def unary_!()(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = UnaryOperator("!", this)
  def ||(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("||", this, right)
  def &&(right: StanValue[T])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = BinaryOperator("&&", this, right)

  def +[R <: StanType](right: Int)(implicit ev: AdditionAllowed[R, T, StanInt]): StanValue[R] =
    BinaryOperator[R, T, StanInt]("+", this, right)
  def +[R <: StanType](right: Double)(implicit ev: AdditionAllowed[R, T, StanReal]): StanValue[R] =
    BinaryOperator[R, T, StanReal]("+", this, right)
  def +[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("+", this, right)

  def -[R <: StanType](right: Int)(implicit ev: AdditionAllowed[R, T, StanInt]): StanValue[R] =
    BinaryOperator[R, T, StanInt]("-", this, right)
  def -[R <: StanType](right: Double)(implicit ev: AdditionAllowed[R, T, StanReal]): StanValue[R] =
    BinaryOperator[R, T, StanReal]("-", this, right)
  def -[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("-", this, right)

  def *[R <: StanType](right: Int)(implicit ev: MultiplicationAllowed[R, T, StanInt]): StanValue[R] =
    BinaryOperator[R, T, StanInt]("*", this, right)
  def *[R <: StanType](right: Double)(implicit ev: MultiplicationAllowed[R, T, StanReal]): StanValue[R] =
    BinaryOperator[R, T, StanReal]("*", this, right)
  def *[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: MultiplicationAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("*", this, right)

  def /[B <: StanScalarType](right: StanValue[B]): StanValue[T] = BinaryOperator("/", this, right)

  def %(right: StanValue[T])(implicit ev: ModulusAllowed[T]): StanValue[T] = BinaryOperator("%", this, right)

  def ^(right: StanValue[T]): StanValue[T] = BinaryOperator("^", this, right)

  // Element-wise operators.
  def :*(right: StanValue[T]): StanValue[T]= BinaryOperator(".*", this, right)
  /*
  def :*[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit e: ElementwiseMultiplicationAllowed[R, T, B]): StanValue[R]= BinaryOperator(".*", this, right)
  */
  /*
  def :/[B <: StanType, R <: StanType](
    right: StanValue[T]
  )(implicit e: ElementwiseMultiplicationAllowed[R, T, B]): StanValue[R] = BinaryOperator("./", this, right)
  */
  def :/(right: StanValue[T]): StanValue[T]= BinaryOperator("./", this, right)

  def +=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B], code: ArrayBuffer[StanValue[_]]
  ): StanValue[T] = {
    this := this + right
  }

  def -=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B], code: ArrayBuffer[StanValue[_]]
  ): StanValue[T] = {
    this := this - right
  }

  def *=[B <: StanType](right: StanValue[B])(
    implicit ev: MultiplicationAllowed[T, T, B], code: ArrayBuffer[StanValue[_]]
  ): StanValue[T] = {
    this := this * right
  }

  def /=[B <: StanScalarType](right: StanValue[B])(
    implicit code: ArrayBuffer[StanValue[_]]
  ): StanValue[T] = {
    this := this / right
  }

  def ~(dist: StanValue[T])(implicit code: ArrayBuffer[StanValue[_]]): StanValue[T] = {
    val op = BinaryOperator[T, T, T]("~", this, dist, parens = false)
    code += op
    op
  }

  def t[R <: StanType](implicit e: TranposeAllowed[T, R]): StanValue[R] = TransposeOperator(this)

  def :=(right: StanValue[T])(implicit code: ArrayBuffer[StanValue[_]]): StanValue[T] = {
    val op = BinaryOperator[T, T, T]("=", this, right, parens = false)
    code += op
    op
  }

  def apply[N <: StanType](index: StanValue[StanInt])(implicit ev: N =:= T#NEXT_TYPE): StanValue[N] = {
    IndexOperator(this, index)
  }

  def apply[N <: StanType](
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  )(implicit ev: N =:= T#NEXT_TYPE#NEXT_TYPE): StanValue[N] = {
    IndexOperator(this, index1, index2)
  }
}

abstract class EnterScope extends StanValue[StanInt] {
  override val terminator: String = ""
}

case class ForLoop[T <: StanType](
  decl: StanDeclaration[T, LocalDeclarationType],
  range: StanValue[T]
) extends EnterScope {
  def emit: String = s"for(${decl.emit} in ${range.emit}) {"
}

case class IfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"if(${cond.emit}) {"
}

case class ElseIfStatement[T <: StanType](
  cond: StanValue[T]
) extends EnterScope {
  def emit: String = s"else if(${cond.emit}) {"
}

case object ElseStatement extends EnterScope {
  def emit: String = s"else {"
}

case object LeaveScope extends StanValue[StanInt] {
  def emit: String = "}"
  override val terminator: String = ""
}

case class FunctionNode[T <: StanType](
  name: String,
  args: Seq[StanValue[_]]
) extends StanValue[T] {
  def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name($argStr)"
  }
}

case class ReturnNode[T <: StanType](
  result: StanValue[T]
) extends StanValue[T] {
  def emit: String = s"return ${result.emit}"
}

case class ImplicitConversion[FROM <: StanType, TO <: StanType](
  value: StanValue[FROM]
) extends StanValue[TO] {
  def emit: String = value.emit
}

case class UnaryOperator[T <: StanType, R <: StanType](
  symbol: String,
  right: StanValue[T]
) extends StanValue[R] {
  def emit: String = s"$symbol (${right.emit})"
}

case class BinaryOperator[T <: StanType, L <: StanType, R <: StanType](
  symbol: String,
  left: StanValue[L],
  right: StanValue[R],
  parens: Boolean = true
) extends StanValue[T] {
  def emit: String =
    if (parens) {
      s"(${left.emit}) $symbol (${right.emit})"
    } else {
      s"${left.emit} $symbol ${right.emit}"
    }
}

case class IndexOperator[T <: StanType, N <: StanType](
  value: StanValue[T],
  indices: StanValue[StanInt]*
) extends StanValue[N] {
  def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class TransposeOperator[T <: StanType, R <: StanType](value: StanValue[T]) extends StanValue[R] {
  def emit: String = s"(${value.emit})'"
}

case class ValueRange(start: StanValue[StanInt], end: StanValue[StanInt]) extends StanValue[StanInt] {
  def emit: String = s"${start.emit}:${end.emit}"
}

sealed trait DeclarationType
final class DataDeclarationType extends DeclarationType
final class ParameterDeclarationType extends DeclarationType
final class LocalDeclarationType extends DeclarationType

case class StanDeclaration[T <: StanType, D <: DeclarationType] private[scalastan] (
  typeConstructor: T
) extends StanValue[T] {
  private val name: String = StanDeclaration.getName
  def emit: String = name
  def emitDeclaration: String = typeConstructor.emitDeclaration(name)
}

object StanDeclaration {
  private var counter: Int = 0

  private def getName: String = {
    counter += 1
    s"v$counter"
  }
}

case class StanInlineDeclaration[T <: StanType](
  decl: StanDeclaration[T, LocalDeclarationType]
) extends StanValue[T] {
  def emit: String = decl.emitDeclaration
}

case class StanConstant[T <: StanType](value: T#SCALA_TYPE) extends StanValue[T] {
  def emit: String = value.toString
}
