package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

// Base class for value types.
abstract class StanValue[T <: StanType] extends StanNode with Implicits {

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
    implicit ev: AdditionAllowed[T, T, B], code: ArrayBuffer[StanNode]
  ): Unit = {
    this := this + right
  }

  def -=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B], code: ArrayBuffer[StanNode]
  ): Unit = {
    this := this - right
  }

  def *=[B <: StanType](right: StanValue[B])(
    implicit ev: MultiplicationAllowed[T, T, B], code: ArrayBuffer[StanNode]
  ): Unit = {
    this := this * right
  }

  def /=[B <: StanScalarType](right: StanValue[B])(
    implicit code: ArrayBuffer[StanNode]
  ): Unit = {
    this := this / right
  }

  def ~(dist: StanDistribution[T])(implicit code: ArrayBuffer[StanNode]): Unit = {
    code += SampleNode[T](this, dist)
  }

  def t[R <: StanType](implicit e: TranposeAllowed[T, R]): StanValue[R] = TransposeOperator(this)

  def :=(right: StanValue[T])(implicit code: ArrayBuffer[StanNode]): Unit = {
    code += BinaryOperator[T, T, T]("=", this, right, parens = false)
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

  def apply[N <: StanType](
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  )(implicit ev: N =:= T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE): StanValue[N] = {
    IndexOperator(this, index1, index2, index3)
  }

  def apply[N <: StanType](
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  )(implicit ev: N =:= T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE): StanValue[N] = {
    IndexOperator(this, index1, index2, index3, index4)
  }
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

case class DistributionFunctionNode[T <: StanType](
  name: String,
  y: StanValue[T],
  sep: String,
  args: Seq[StanValue[_]]
) extends StanValue[T] {
  def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name(${y.emit} $sep $argStr)"
  }
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

case class StanConstant[T <: StanType](value: T#SCALA_TYPE) extends StanValue[T] {
  def emit: String = value.toString
}
