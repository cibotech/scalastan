/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.ast

import com.cibo.scalastan._

import scala.language.existentials

// Base class for value types.
abstract class StanValue[T <: StanType] extends StanNode with Implicits {

  // The declaration type used to declare this value.
  // This is used to determine if assignment is allowed.
  private[scalastan] type DECL_TYPE <: StanDeclaration[_]

  private[scalastan] def inputs: Seq[StanDeclaration[_]]
  private[scalastan] def outputs: Seq[StanDeclaration[_]]

  // Check if this value is derived from data only.
  private[scalastan] def isDerivedFromData: Boolean

  // Emit the Stan representation of this value.
  private[scalastan] def emit: String

  def unary_-(): StanValue[T] = StanUnaryOperator("-", this)

  // Logical functions.
  def ===[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("==", this, right)
  def =/=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("!=", this, right)
  def <[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("<", this, right)
  def <=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("<=", this, right)
  def >[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(">", this, right)
  def >=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(">=", this, right)

  // Boolean operators.
  def unary_!()(implicit ev: LogicalAllowed[T]): StanValue[StanInt] = StanUnaryOperator("!", this)
  def ||[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("||", this, right)
  def &&[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator("&&", this, right)

  def +[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator("+", this, right)

  def -[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator("-", this, right)

  def *[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: MultiplicationAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator("*", this, right)

  def /[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: DivisionAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator("/", this, right)

  def \[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: LeftDivisionAllowed[R, T, B]): StanValue[R] =
    StanBinaryOperator("\\", this, right)

  def %(right: StanValue[T])(implicit ev: ModulusAllowed[T]): StanValue[T] =
    StanBinaryOperator("%", this, right)

  def ^[R <: StanScalarType](
    right: StanValue[R]
  )(
    implicit ev: IsScalarType[R]
  ): StanValue[T] = StanBinaryOperator("^", this, right)

  // Element-wise operators.
  def *:*(right: StanValue[T])(implicit ev: IsCompoundType[T]): StanValue[T] =
    StanBinaryOperator(".*", this, right)
  def /:/[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: ElementWiseDivisionAllowed[R, T, B]): StanValue[R]= StanBinaryOperator("./", this, right)

  def ~(dist: StanDistribution[T])(
    implicit code: CodeBuilder,
    ev: Is0or1Dimensional[T]
  ): Unit = {
    code.append(StanSampleStatement[T](this, dist))
  }

  def t[R <: StanType](implicit e: TransposeAllowed[T, R]): StanValue[R] = StanTranspose(this)
}

trait ReadOnlyIndex[T <: StanType] { self: StanValue[T] =>
  def apply[I <: StanType, N <: StanType](
    index: StanValue[I]
  )(
    implicit ev: IndexAllowed[T, I, N]
  ): StanIndexOperator[T, N] = StanIndexOperator(this, index)

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE] = {
    StanIndexOperator(this, index1, index2)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    StanIndexOperator(this, index1, index2, index3)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE] = {
    StanIndexOperator(this, index1, index2, index3, index4)
  }

  def apply(slice: StanValueRange): StanSliceOperator[T] = StanSliceOperator(this, slice)
}

trait Assignable[T <: StanType] { self: StanValue[T] =>

  private[scalastan] val value: StanValue[_]

  def :=[R <: StanType](right: StanValue[R])(
    implicit code: CodeBuilder,
    ev1: AssignmentAllowed[DECL_TYPE],
    ev2: CanConvert[R, T]
  ): Unit = {
    code.append(StanAssignment(this, right))
  }

  def apply[I <: StanType, N <: StanType](
    index: StanValue[I]
  )(
    implicit ev: IndexAllowed[T, I, N]
  ): StanIndexOperatorWithAssignment[T, N, DECL_TYPE] =
    StanIndexOperatorWithAssignment(this, index)

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  ): StanIndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperatorWithAssignment(this, index1, index2)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  ): StanIndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperatorWithAssignment(this, index1, index2, index3)
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  ): StanIndexOperatorWithAssignment[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperatorWithAssignment(this, index1, index2, index3, index4)
  }

  def apply(slice: StanValueRange): StanSliceOperatorWithAssignment[T, DECL_TYPE] = StanSliceOperatorWithAssignment(this, slice)
}

trait Incrementable[T <: StanType] { self: StanValue[T] =>
  def +=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B],
    code: CodeBuilder
  ): Unit = {
    code.append(StanAssignment(this, right, StanAssignment.Add))
  }
}

trait Updatable[T <: StanType] extends Incrementable[T] { self: StanValue[T] =>
  def -=[B <: StanType](right: StanValue[B])(
    implicit ev: AdditionAllowed[T, T, B], code: CodeBuilder
  ): Unit = {
    code.append(StanAssignment(this, right, StanAssignment.Subtract))
  }

  def *=[B <: StanType](right: StanValue[B])(
    implicit ev: MultiplicationAllowed[T, T, B], code: CodeBuilder
  ): Unit = {
    code.append(StanAssignment(this, right, StanAssignment.Multiply))
  }

  def /=[B <: StanScalarType](right: StanValue[B])(
    implicit code: CodeBuilder
  ): Unit = {
    code.append(StanAssignment(this, right, StanAssignment.Divide))
  }
}

case class StanCall[T <: StanType] private[scalastan] (
  private val name: String,
  private val args: StanValue[_]*
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = args.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = args.flatMap(_.outputs)
  private[scalastan] def isDerivedFromData: Boolean = args.forall(_.isDerivedFromData)
  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name($argStr)"
  }
}

case class StanGetTarget private[scalastan] () extends StanValue[StanReal] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def emit: String = "target()"
}

case class StanTargetValue private[scalastan] () extends StanValue[StanReal] with Incrementable[StanReal] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def emit: String = "target"
  def apply(): StanGetTarget = StanGetTarget()
}

case class StanDistributionNode[T <: StanType, R <: StanType] private[scalastan] (
  private val name: String,
  private val y: StanValue[T],
  private val sep: String,
  private val args: Seq[StanValue[_]]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = y.inputs ++ args.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name(${y.emit} $sep $argStr)"
  }
}

case class StanUnaryOperator[T <: StanType, R <: StanType] private[scalastan] (
  private val symbol: String,
  private val right: StanValue[T]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = right.outputs
  private[scalastan] def isDerivedFromData: Boolean = right.isDerivedFromData
  private[scalastan] def emit: String = s"$symbol(${right.emit})"
}

case class StanBinaryOperator[T <: StanType, L <: StanType, R <: StanType] private[scalastan] (
  private val symbol: String,
  private val left: StanValue[L],
  private val right: StanValue[R],
  private val parens: Boolean = true
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = left.inputs ++ right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = left.outputs ++ right.outputs
  private[scalastan] def isDerivedFromData: Boolean = left.isDerivedFromData && right.isDerivedFromData
  private[scalastan] def emit: String =
    if (parens) {
      s"(${left.emit}) $symbol (${right.emit})"
    } else {
      s"${left.emit} $symbol ${right.emit}"
    }
}

case class StanIndexOperator[T <: StanType, N <: StanType] private[scalastan] (
  private val value: StanValue[T],
  private val indices: StanValue[_]*
) extends StanValue[N] with ReadOnlyIndex[N] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = value.inputs ++ indices.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = value.outputs ++ indices.flatMap(_.outputs)
  private[scalastan] def isDerivedFromData: Boolean = value.isDerivedFromData && indices.forall(_.isDerivedFromData)
  private[scalastan] def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class StanSliceOperator[T <: StanType] private[scalastan] (
  private val value: StanValue[T],
  private val slice: StanValueRange
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = value.inputs ++ slice.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = value.outputs ++ slice.outputs
  private[scalastan] def isDerivedFromData: Boolean =
    value.isDerivedFromData && slice.start.isDerivedFromData && slice.end.isDerivedFromData
  private[scalastan] def emit: String = s"${value.emit}[${slice.start.emit}:${slice.end.emit}]"
}

case class StanIndexOperatorWithAssignment[T <: StanType, N <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  private[scalastan] val value: StanValue[_ <: StanType],
  private[scalastan] val indices: StanValue[_]*
) extends StanValue[N] with Assignable[N] {
  private[scalastan] type DECL_TYPE = D
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = value.inputs ++ indices.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = value.outputs ++ indices.flatMap(_.outputs)
  private[scalastan] def isDerivedFromData: Boolean = value.isDerivedFromData && indices.forall(_.isDerivedFromData)
  private[scalastan] def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class StanSliceOperatorWithAssignment[T <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  private[scalastan] val value: StanValue[_ <: StanType],
  private[scalastan] val slice: StanValueRange
) extends StanValue[T] with Assignable[T] {
  private[scalastan] type DECL_TYPE = D
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = value.inputs ++ slice.start.inputs ++ slice.end.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = value.outputs ++ slice.start.outputs ++ slice.end.outputs
  private[scalastan] def isDerivedFromData: Boolean =
    value.isDerivedFromData && slice.start.isDerivedFromData && slice.end.isDerivedFromData
  private[scalastan] def emit: String = s"${value.emit}[${slice.start.emit}:${slice.end.emit}]"
}

case class StanTranspose[T <: StanType, R <: StanType] private[scalastan] (
  private val value: StanValue[T]
) extends StanValue[R] with ReadOnlyIndex[R] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = value.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = value.outputs
  private[scalastan] def isDerivedFromData: Boolean = value.isDerivedFromData
  private[scalastan] def emit: String = s"(${value.emit})'"
}

case class StanConstant[T <: StanType] private[scalastan] (
  private val value: T#SCALA_TYPE
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def emit: String = value.toString
}

case class StanArrayLiteral[T <: StanType] private[scalastan] (
  private val values: Seq[StanValue[T#NEXT_TYPE]]
) extends StanValue[T] with ReadOnlyIndex[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = values.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = values.flatMap(_.outputs)
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def emit: String = values.map(_.emit).mkString("{", ",", "}")
}

case class StanStringLiteral private[scalastan] (
  private val value: String
) extends StanValue[StanString] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def emit: String = s""""$value""""
}

case class StanLiteral private[scalastan] (
  private val value: String
) extends StanValue[StanVoid] {
  private[scalastan] def inputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def emit: String = value.toString
}
