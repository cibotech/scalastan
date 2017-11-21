/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer
import scala.language.existentials

// Base class for value types.
abstract class StanValue[T <: StanType] extends StanNode with Implicits {

  // The declaration type used to declare this value.
  // This is used to determine if assignment is allowed.
  type DECL_TYPE <: StanDeclaration[_]

  def unary_-(): StanValue[T] = UnaryOperator("-", this)

  // Logical functions.
  def ===[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("==", this, right)
  def =/=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("!=", this, right)
  def <[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("<", this, right)
  def <=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("<=", this, right)
  def >[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator(">", this, right)
  def >=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator(">=", this, right)

  // Boolean operators.
  def unary_!()(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = UnaryOperator("!", this)
  def ||[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("||", this, right)
  def &&[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    BinaryOperator("&&", this, right)

  def +[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("+", this, right)

  def -[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("-", this, right)

  def *[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: MultiplicationAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("*", this, right)

  def /[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: DivisionAllowed[R, T, B]
  ): StanValue[R] = BinaryOperator("/", this, right)

  def \[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: LeftDivisionAllowed[R, T, B]): StanValue[R] =
    BinaryOperator("\\", this, right)

  def %(right: StanValue[T])(implicit ev: ModulusAllowed[T]): StanValue[T] =
    BinaryOperator("%", this, right)

  def ^[R <: StanScalarType](
    right: StanValue[R]
  )(
    implicit ev: IsScalarType[R]
  ): StanValue[T] = BinaryOperator("^", this, right)

  // Element-wise operators.
  def :*(right: StanValue[T])(implicit ev: IsCompoundType[T]): StanValue[T] =
    BinaryOperator(".*", this, right)
  def :/[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: ElementWiseDivisionAllowed[R, T, B]): StanValue[R]= BinaryOperator("./", this, right)

  def ~(dist: StanDistribution[T])(
    implicit code: ArrayBuffer[StanNode],
    ev: Is0or1Dimensional[T]
  ): Unit = {
    code += SampleNode[T](this, dist)
  }

  def t[R <: StanType](implicit e: TransposeAllowed[T, R]): StanValue[R] = TransposeOperator(this)
}

trait ReadOnlyIndex[T <: StanType] { self: StanValue[T] =>
  def apply(index: StanValue[StanInt]): IndexOperator[T, T#NEXT_TYPE] = IndexOperator(this, index)

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  ): IndexOperator[T, T#NEXT_TYPE#NEXT_TYPE] = {
    IndexOperator(this, index1, index2)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  ): IndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    IndexOperator(this, index1, index2, index3)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  ): IndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    IndexOperator(this, index1, index2, index3, index4)
  }

  def apply(slice: ValueRange): SliceOperator[T] = SliceOperator(this, slice)
}

trait Assignable[T <: StanType] { self: StanValue[T] =>
  def :=(right: StanValue[T])(
    implicit code: ArrayBuffer[StanNode], ev: AssignmentAllowed[DECL_TYPE]
  ): Unit = {
    code += BinaryOperator[T, T, T]("=", this, right, parens = false)
  }

  def apply(
    index: StanValue[StanInt]
  ): IndexOperatorWithAssignment[T, T#NEXT_TYPE, DECL_TYPE] = {
    IndexOperatorWithAssignment(this, index)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  ): IndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    IndexOperatorWithAssignment(this, index1, index2)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  ): IndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    IndexOperatorWithAssignment(this, index1, index2, index3)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  ): IndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    IndexOperatorWithAssignment(this, index1, index2, index3, index4)
  }

  def apply(slice: ValueRange): SliceOperatorWithAssignment[T, DECL_TYPE] = SliceOperatorWithAssignment(this, slice)
}

trait Incrementable[T <: StanType] { self: StanValue[T] =>
  def +=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B],
    code: ArrayBuffer[StanNode]
  ): Unit = {
    code += BinaryOperator("+=", this, right, parens = false)
  }
}

trait Updatable[T <: StanType] extends Incrementable[T] { self: StanValue[T] =>
  def -=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B], code: ArrayBuffer[StanNode]
  ): Unit = {
    code += BinaryOperator("-=", this, right, parens = false)
  }

  def *=[B <: StanType](right: StanValue[B])(
    implicit ev: MultiplicationAllowed[T, T, B], code: ArrayBuffer[StanNode]
  ): Unit = {
    code += BinaryOperator("*=", this, right, parens = false)
  }

  def /=[B <: StanScalarType](right: StanValue[B])(
    implicit code: ArrayBuffer[StanNode]
  ): Unit = {
    code += BinaryOperator("/=", this, right, parens = false)
  }
}

case class FunctionNode[T <: StanType] private[scalastan] (
  private val name: String,
  private val args: StanValue[_]*
) extends StanValue[T] with ReadOnlyIndex[T] {
  def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name($argStr)"
  }
}

case class TargetFunction private[scalastan] () extends StanValue[StanReal] {
  private[scalastan] def emit: String = "target()"
}

case class TargetValue private[scalastan] () extends StanValue[StanReal] with Incrementable[StanReal] {
  private[scalastan] def emit: String = "target"
  def apply(): TargetFunction = TargetFunction()
}

case class DistributionFunctionNode[T <: StanType, R <: StanType] private[scalastan] (
  private val name: String,
  private val y: StanValue[T],
  private val sep: String,
  private val args: Seq[StanValue[_]]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name(${y.emit} $sep $argStr)"
  }
}

case class UnaryOperator[T <: StanType, R <: StanType] private[scalastan] (
  private val symbol: String,
  private val right: StanValue[T]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def emit: String = s"$symbol(${right.emit})"
}

case class BinaryOperator[T <: StanType, L <: StanType, R <: StanType] private[scalastan] (
  private val symbol: String,
  private val left: StanValue[L],
  private val right: StanValue[R],
  private val parens: Boolean = true
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def emit: String =
    if (parens) {
      s"(${left.emit}) $symbol (${right.emit})"
    } else {
      s"${left.emit} $symbol ${right.emit}"
    }
}

case class IndexOperator[T <: StanType, N <: StanType] private[scalastan] (
  private val value: StanValue[T],
  private val indices: StanValue[StanInt]*
) extends StanValue[N] with ReadOnlyIndex[N] {
  private[scalastan] def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class SliceOperator[T <: StanType] private[scalastan] (
  private val value: StanValue[T],
  private val slice: ValueRange
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def emit: String = s"${value.emit}[${slice.start.emit}:${slice.end.emit}]"
}

case class IndexOperatorWithAssignment[T <: StanType, N <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  private val value: StanValue[T],
  private val indices: StanValue[StanInt]*
) extends StanValue[N] with Assignable[N] {
  type DECL_TYPE = D
  private[scalastan] def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class SliceOperatorWithAssignment[T <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  private val value: StanValue[T],
  private val slice: ValueRange
) extends StanValue[T] with Assignable[T] {
  type DECL_TYPE = D
  private[scalastan] def emit: String = s"${value.emit}[${slice.start.emit}:${slice.end.emit}]"
}

case class TransposeOperator[T <: StanType, R <: StanType] private[scalastan] (
  private val value: StanValue[T]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def emit: String = s"(${value.emit})'"
}

case class StanConstant[T <: StanType] private[scalastan] (
  private val value: T#SCALA_TYPE
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def emit: String = value.toString
}

case class StanStringLiteral private[scalastan] (
  private val value: String
) extends StanValue[StanString] {
  private[scalastan] def emit: String = s""""$value""""
}

case class LiteralNode private[scalastan] (
  private val value: String
) extends StanValue[StanVoid] {
  private[scalastan] def emit: String = value.toString
}
