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

  private[scalastan] val returnType: T

  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]]
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]]

  private[scalastan] def export(builder: CodeBuilder): Unit

  // Check if this value is derived from data only.
  private[scalastan] def isDerivedFromData: Boolean

  // Emit the Stan representation of this value.
  private[scalastan] def emit: String

  def unary_-(): StanValue[T] = StanUnaryOperator(returnType, StanUnaryOperator.Negate, this)

  // Logical functions.
  def ===[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.EqualTo, StanInt(), this, right)
  def =/=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.NotEqualTo, StanInt(), this, right)
  def <[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.LessThan, StanInt(), this, right)
  def <=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.LessThanOrEqualTo, StanInt(), this, right)
  def >[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.GreaterThan, StanInt(), this, right)
  def >=[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.GreaterThanOrEqualTo, StanInt(), this, right)

  // Boolean operators.
  def unary_!()(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanUnaryOperator(StanInt(), StanUnaryOperator.LogicalNot, this)
  def ||[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.LogicalOr, StanInt(), this, right)
  def &&[R <: StanScalarType](right: StanValue[R])(implicit ev: LogicalAllowed[T]): StanValue[StanInt] =
    StanBinaryOperator(StanBinaryOperator.LogicalAnd, StanInt(), this, right)

  def +[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] =
    StanBinaryOperator(StanBinaryOperator.Add, ev.newType(returnType, right.returnType), this, right)

  def -[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: AdditionAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator(StanBinaryOperator.Subtract, ev.newType(returnType, right.returnType), this, right)

  def *[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: MultiplicationAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator(StanBinaryOperator.Multiply, ev.newType(returnType, right.returnType), this, right)

  def /[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(
    implicit ev: DivisionAllowed[R, T, B]
  ): StanValue[R] = StanBinaryOperator(StanBinaryOperator.Divide, ev.newType(returnType, right.returnType), this, right)

  def \[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: LeftDivisionAllowed[R, T, B]): StanValue[R] =
    StanBinaryOperator(StanBinaryOperator.LeftDivide, ev.newType(returnType, right.returnType), this, right)

  def %(right: StanValue[T])(implicit ev: ModulusAllowed[T]): StanValue[T] =
    StanBinaryOperator(StanBinaryOperator.Modulus, ev.newType(returnType, right.returnType), this, right)

  def ^[R <: StanScalarType](
    right: StanValue[R]
  )(
    implicit ev: IsScalarType[R]
  ): StanValue[StanReal] = StanBinaryOperator(StanBinaryOperator.Power, StanReal(), this, right)

  // Element-wise operators.
  def *:*(right: StanValue[T])(implicit ev: IsCompoundType[T]): StanValue[T] =
    StanBinaryOperator(StanBinaryOperator.ElementWiseMultiply, returnType, this, right)
  def /:/[B <: StanType, R <: StanType](
    right: StanValue[B]
  )(implicit ev: ElementWiseDivisionAllowed[R, T, B]): StanValue[R] =
    StanBinaryOperator(StanBinaryOperator.ElementWiseDivide, ev.newType(returnType, right.returnType), this, right)

  def ~[R <: StanType](dist: StanDistribution[T, R])(
    implicit code: CodeBuilder,
    ev: SampleAllowed[T, R]
  ): Unit = {
    code.append(StanSampleStatement[T, R](this, dist))
  }

  def t[R <: StanType](implicit e: TransposeAllowed[T, R]): StanValue[R] = StanTranspose(e.newType(returnType), this)

  def :=[R <: StanType](right: StanValue[R])(
    implicit code: CodeBuilder,
    ev1: AssignmentAllowed[DECL_TYPE],
    ev2: CanConvert[R, T]
  ): StanAssignment = {
    val assignment = StanAssignment(this, right)
    code.append(assignment)
    assignment
  }

  def apply[I <: StanType, N <: StanType](
    index: StanValue[I]
  )(
    implicit ev: IndexAllowed[T, I, N]
  ): StanIndexOperator[T, N, DECL_TYPE] =
    StanIndexOperator(ev.nextType(returnType, index.returnType), this, Seq(index))

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperator(returnType.next.next.asInstanceOf[T#NEXT_TYPE#NEXT_TYPE], this, Seq(index1, index2))
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperator(
      returnType.next.next.next.asInstanceOf[T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE], this, Seq(index1, index2, index3))
  }

  def apply(
    index1: StanValue[StanInt],
    index2: StanValue[StanInt],
    index3: StanValue[StanInt],
    index4: StanValue[StanInt]
  ): StanIndexOperator[T, T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE, DECL_TYPE] = {
    StanIndexOperator(
      returnType.next.next.next.next.asInstanceOf[T#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE#NEXT_TYPE],
      this, Seq(index1, index2, index3, index4))
  }

  def apply(slice: StanValueRange): StanSliceOperator[T, DECL_TYPE] = StanSliceOperator(this, slice)
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

trait StanFunction {
  def name: String
  private[scalastan] def export(builder: CodeBuilder): Unit
}

case class BuiltinFunction(name: String) extends StanFunction {
  private[scalastan] def export(bulder: CodeBuilder): Unit = ()
}

case class StanCall[T <: StanType] private[scalastan] (
  returnType: T,
  function: StanFunction,
  args: Seq[StanValue[_ <: StanType]],
  id: Int
) extends StanValue[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = args.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = args.forall(_.isDerivedFromData)
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    args.foreach(_.export(builder))
    function.export(builder)
  }
  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"${function.name}($argStr)"
  }
}

object StanCall {
  def apply[T <: StanType](
    returnType: T,
    function: StanFunction,
    args: Seq[StanValue[_ <: StanType]]
  ): StanCall[T] = new StanCall(returnType, function, args, StanNode.getNextId)

  def apply[T <: StanType](
    returnType: T,
    name: String,
    args: Seq[StanValue[_ <: StanType]] = Seq.empty
  ): StanCall[T] = apply(returnType, BuiltinFunction(name), args)
}

case class StanGetTarget private[scalastan] (
  id: Int = StanNode.getNextId
) extends StanValue[StanReal] {
  private[scalastan] val returnType: StanReal = StanReal()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = "target()"
}

case class StanTargetValue private[scalastan] (
  id: Int = StanNode.getNextId
) extends StanValue[StanReal] with Incrementable[StanReal] {
  val returnType: StanReal = StanReal()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = "target"
  def apply(): StanGetTarget = StanGetTarget()
}

case class StanDistributionNode[T <: StanType] private[scalastan] (
  name: String,
  y: StanValue[T],
  sep: String,
  args: Seq[StanValue[_ <: StanType]],
  id: Int = StanNode.getNextId
) extends StanValue[StanReal] {
  val returnType: StanReal = StanReal()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = y.inputs ++ args.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = false
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    args.foreach(_.export(builder))
    y.export(builder)
  }
  private[scalastan] def emit: String = {
    val argStr = args.map(_.emit).mkString(",")
    s"$name(${y.emit} $sep $argStr)"
  }
}

case class StanUnaryOperator[T <: StanType, R <: StanType] private[scalastan] (
  returnType: R,
  op: StanUnaryOperator.Operator,
  right: StanValue[T],
  id: Int = StanNode.getNextId
) extends StanValue[R] {
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = right.isDerivedFromData
  private[scalastan] def export(builder: CodeBuilder): Unit = right.export(builder)
  private[scalastan] def emit: String = s"${op.name}(${right.emit})"
}

object StanUnaryOperator {
  sealed abstract class Operator(val name: String)
  case object Negate extends Operator("-")
  case object LogicalNot extends Operator("!")
}

case class StanBinaryOperator[T <: StanType, L <: StanType, R <: StanType] private[scalastan] (
  op: StanBinaryOperator.Operator,
  returnType: T,
  left: StanValue[L],
  right: StanValue[R],
  parens: Boolean = true,
  id: Int = StanNode.getNextId
) extends StanValue[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = left.inputs ++ right.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = left.isDerivedFromData && right.isDerivedFromData
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    left.export(builder)
    right.export(builder)
  }
  private[scalastan] def emit: String =
    if (parens) {
      s"(${left.emit}) ${op.name} (${right.emit})"
    } else {
      s"${left.emit} ${op.name} ${right.emit}"
    }
}

object StanBinaryOperator {
  sealed abstract class Operator(val name: String)
  case object Add extends Operator("+")
  case object Subtract extends Operator("-")
  case object Multiply extends Operator("*")
  case object Divide extends Operator("/")
  case object LeftDivide extends Operator("\\")
  case object Modulus extends Operator("%")
  case object Power extends Operator("^")
  case object ElementWiseMultiply extends Operator(".*")
  case object ElementWiseDivide extends Operator("./")
  case object EqualTo extends Operator("==")
  case object NotEqualTo extends Operator("!=")
  case object LessThan extends Operator("<")
  case object LessThanOrEqualTo extends Operator("<=")
  case object GreaterThan extends Operator(">")
  case object GreaterThanOrEqualTo extends Operator(">=")
  case object LogicalOr extends Operator("||")
  case object LogicalAnd extends Operator("&&")
}

case class StanIndexOperator[T <: StanType, N <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  returnType: N,
  value: StanValue[_ <: StanType],
  indices: Seq[StanValue[_ <: StanType]],
  id: Int = StanNode.getNextId
) extends StanValue[N] {
  private[scalastan] type DECL_TYPE = D
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = value.inputs ++ indices.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = value.isDerivedFromData && indices.forall(_.isDerivedFromData)
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    value.export(builder)
    indices.foreach(_.export(builder))
  }
  private[scalastan] def emit: String = value.emit + indices.map(_.emit).mkString("[", ",", "]")
}

case class StanSliceOperator[T <: StanType, D <: StanDeclaration[_]] private[scalastan] (
  value: StanValue[T],
  slice: StanValueRange,
  id: Int = StanNode.getNextId
) extends StanValue[T] {
  private[scalastan] type DECL_TYPE = D
  private[scalastan] val returnType: T = value.returnType.asInstanceOf[T]
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = value.inputs ++ slice.start.inputs ++ slice.end.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean =
    value.isDerivedFromData && slice.start.isDerivedFromData && slice.end.isDerivedFromData
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    value.export(builder)
    slice.export(builder)
  }
  private[scalastan] def emit: String = s"${value.emit}[${slice.start.emit}:${slice.end.emit}]"
}

case class StanTranspose[T <: StanType, R <: StanType] private[scalastan] (
  returnType: R,
  value: StanValue[T],
  id: Int = StanNode.getNextId
) extends StanValue[R] {
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = value.inputs
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = value.isDerivedFromData
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    value.export(builder)
  }
  private[scalastan] def emit: String = s"(${value.emit})'"
}

case class StanConstant[T <: StanType] private[scalastan] (
  returnType: T,
  value: T#SCALA_TYPE,
  id: Int = StanNode.getNextId
) extends StanValue[T] {
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = value.toString
}

case class StanArrayLiteral[N <: StanType, T <: StanArray[N]] private[scalastan] (
  values: Seq[StanValue[N]],
  id: Int = StanNode.getNextId
) extends StanValue[T] {
  val returnType: T = StanArray(StanConstant[StanInt](StanInt(), values.length), values.head.returnType).asInstanceOf[T]
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = values.flatMap(_.inputs)
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def export(builder: CodeBuilder): Unit = {
    values.foreach(_.export(builder))
  }
  private[scalastan] def emit: String = values.map(_.emit).mkString("{", ",", "}")
}

case class StanStringLiteral private[scalastan] (
  value: String,
  id: Int = StanNode.getNextId
) extends StanValue[StanString] {
  val returnType: StanString = StanString()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = s""""$value""""
}

case class StanLiteral private[scalastan] (
  value: String,
  id: Int = StanNode.getNextId
) extends StanValue[StanVoid] {
  private[scalastan] val returnType: StanVoid = StanVoid()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = value.toString
}

case class StanUnknownDim(
  id: Int = StanNode.getNextId
) extends StanValue[StanInt] {
  val returnType: StanInt = StanInt()
  private[scalastan] def inputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def outputs: Seq[StanDeclaration[_ <: StanType]] = Seq.empty
  private[scalastan] def isDerivedFromData: Boolean = true
  private[scalastan] def export(builder: CodeBuilder): Unit = ()
  private[scalastan] def emit: String = ""
}
