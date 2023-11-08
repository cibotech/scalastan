/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.ast.{StanBinaryOperator, StanCall, StanValue}
import com.cibo.scalastan._

case class StrengthReduction() extends StanTransform[Unit] {

  def initialState: Unit = ()

  private def isScalar[T <: StanType](exp: StanValue[T]): Boolean = exp.returnType match {
    case _: StanInt  => true
    case _: StanReal => true
    case _           => false
  }

  private def scalarBinaryOperator[T <: StanType, L <: StanType, R <: StanType](
    op: StanBinaryOperator.Operator,
    exp: StanValue[T]
  ): Option[(StanValue[L], StanValue[R])] = exp match {
    case b: StanBinaryOperator[_, L, R] @unchecked if b.op == op && isScalar(b.left) && isScalar(b.right) =>
      Some((b.left, b.right))
    case _ => None
  }

  private def scalarMultiply[T <: StanType](exp: StanValue[T]): Option[(StanValue[T], StanValue[T])] = {
    scalarBinaryOperator(StanBinaryOperator.Multiply, exp)
  }

  private def expScalar[T <: StanType](exp: StanValue[T]): Option[StanValue[_ <: StanType]] = exp match {
    case call: StanCall[T] if call.function.name == "exp" && isScalar(call.args.head) => Some(call.args.head)
    case _                                                                            => None
  }

  private def expComposite[T <: StanType](exp: StanValue[T]): Option[StanValue[_ <: StanType]] = exp match {
    case call: StanCall[T] if call.function.name == "exp" && !isScalar(call.args.head) => Some(call.args.head)
    case _                                                                             => None
  }

  private def sumExpScalars[T <: StanType](
    exp: StanValue[T]
  ): Option[(StanValue[_ <: StanType], StanValue[_ <: StanType])] = exp match {
    case op: StanBinaryOperator[_, _, _] if op.op == StanBinaryOperator.Add =>
      expScalar(op.left).flatMap { left =>
        expScalar(op.right).map { right =>
          (left, right)
        }
      }
    case _ => None
  }

  private def sumExpSeq[T <: StanType](exp: StanValue[T]): Option[StanValue[_ <: StanType]] = exp match {
    case call: StanCall[_] if call.function.name == "sum" =>
      require(call.args.length == 1, s"expected 1 argument for sum, got ${call.args.length}")
      expComposite(call.args.head)
    case _ => None
  }

  // Convert: log(exp(a) + exp(b)) -> log_sum_exp(a, b)
  // and log(sum(exp(x))) -> log_sum_exp(x)
  private def reduceLog[T <: StanType](call: StanCall[T])(implicit context: StanContext): State[StanValue[T]] = {
    require(call.args.length == 1, s"expected 1 argument for log, got ${call.args.length}")
    for {
      newArg <- handleExpression(call.args.head)
    } yield {
      sumExpScalars(newArg).map { case (left, right) =>
        StanCall(call.returnType, "log_sum_exp", Seq(left, right))
      }.getOrElse {
        sumExpSeq(newArg).map { arg =>
          StanCall(call.returnType, "log_sum_exp", Seq(arg))
        }.getOrElse {
          call.copy(args = Seq(newArg))
        }
      }
    }
  }

  override def handleCall[T <: StanType](
    call: StanCall[T]
  )(implicit context: StanContext): State[StanValue[T]] = call.function.name match {
    case "log" => reduceLog(call)
    case _     => super.handleCall(call)
  }

  // Convert (a * b + c) -> fma(a, b, c) and (a + b * c) -> fma(b, c, a)
  private def reduceAdd[T <: StanType, L <: StanType, R <: StanType](
    op: StanBinaryOperator[T, L, R]
  ): StanValue[T] = {
    val (addLeft, addRight) = (op.left, op.right)
    scalarMultiply(addLeft).map { case (multLeft, multRight) =>
      StanCall(op.returnType, "fma", Seq(multLeft, multRight, addRight))
    }.orElse {
      scalarMultiply(addRight).map { case (multLeft, multRight) =>
        StanCall(op.returnType, "fma", Seq(multLeft, multRight, addLeft))
      }
    }.getOrElse(op)
  }

  override def handleBinaryOperator[T <: StanType, L <: StanType, R <: StanType](
    op: StanBinaryOperator[T, L, R]
  )(implicit context: StanContext): State[StanValue[T]] = {
    for {
      left <- handleExpression(op.left)
      right <- handleExpression(op.right)
      newOp = op.copy(left = left, right = right)
    } yield op.op match {
      case StanBinaryOperator.Add => reduceAdd(newOp)
      case _                      => newOp
    }
  }

}
