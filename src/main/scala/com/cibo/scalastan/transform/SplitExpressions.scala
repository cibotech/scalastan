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

import scala.language.existentials
import com.cibo.scalastan.{StanContext, StanType}
import com.cibo.scalastan.ast._

// Break apart expressions into one operator per statement.
case class SplitExpressions() extends StanTransform[Unit] {

  def initialState: Unit = ()

  override def run(program: StanProgram)(implicit context: StanContext): StanProgram = {
    val updated = super.run(program)
    AstSimplifier().run(updated)
  }

  private def handleLHS[T <: StanType](
    expr: StanValue[T]
  )(implicit context: StanContext): (StanValue[T], Seq[StanStatement]) = {
    expr match {
      case i: StanIndexOperator[_, T, _] =>
        val newIndices = i.indices.map(x => splitExpression(x))
        val newIndex = StanIndexOperator(i.returnType, i.value, newIndices.map(_._1))
        (newIndex, newIndices.flatMap(_._2))
      case s: StanSliceOperator[T, _]    =>
        val newSlices = s.slices.map { slice =>
          val newStart = splitExpression(slice.start)
          val newEnd = splitExpression(slice.end)
          StanValueRange(newStart._1, newEnd._1) -> (newStart._2 ++ newEnd._2)
        }
        (StanSliceOperator(s.value, newSlices.map(_._1)), newSlices.flatMap(_._2))
      case x                             => (x, Seq.empty)
    }
  }

  override def handleAssignment(a: StanAssignment)(implicit context: StanContext): State[StanStatement] = {
    val (lhsValue, lhsStatements) = handleLHS(a.lhs)
    val (rhsValue, rhsStatements) = splitExpression(a.rhs)
    (lhsValue, rhsValue) match {
      case (ld: StanDeclaration[_], _) =>
        val newStatement = StanAssignment(lhsValue, rhsValue, a.op)
        State.pure(StanBlock(lhsStatements ++ rhsStatements :+ newStatement))
      case (_, st: StanDeclaration[_]) =>
        val newStatement = StanAssignment(lhsValue, rhsValue, a.op)
        State.pure(StanBlock(lhsStatements ++ rhsStatements :+ newStatement))
      case _ =>
        val temp = StanLocalDeclaration(rhsValue.returnType, context.newName, derivedFromData = rhsValue.isDerivedFromData)
        val decl = StanInlineDeclaration(temp)
        val load = StanAssignment(temp, rhsValue)
        val store = StanAssignment(lhsValue, temp)
        State.pure(StanBlock(lhsStatements ++ rhsStatements :+ decl :+ load :+ store))
    }
  }

  override def handleReturn(r: StanReturnStatement)(implicit context: StanContext): State[StanStatement] = {
    val (value, statements) = splitExpression(r.result)
    State.pure(StanBlock(statements :+ StanReturnStatement(value)))
  }

  override def handleSample[T <: StanType, R <: StanType](
    s: StanSampleStatement[T, R]
  )(implicit context: StanContext): State[StanStatement] = {
    val (lhsValue, lhsStatements) = handleLHS(s.left)
    val (rhsValue, rhsStatements) = handleDistribution(s.right)
    State.pure {
      StanBlock(
        lhsStatements ++ rhsStatements :+ StanSampleStatement(lhsValue, rhsValue)
      )
    }
  }

  override def handleIf(i: StanIfStatement)(implicit context: StanContext): State[StanStatement] = for {
    conds <- State.traverse(i.conds)(c => dispatch(c._2).map(x => c._1 -> x))
    newOtherwise <- dispatchOption(i.otherwise)
  } yield {
    val newConds = conds.map { case (cond, statement) =>
      val (newCond, newStatements) = splitExpression(cond)
      (newStatements, newCond -> statement)
    }
    val statements = newConds.flatMap(_._1)
    StanBlock(
      statements :+ i.copy(
        conds = newConds.map(_._2),
        otherwise = newOtherwise
      )
    )
  }

  override def handleFor(f: StanForLoop)(implicit context: StanContext): State[StanStatement] = {
    val (start, startStatements) = splitExpression(f.range.start)
    val (end, endStatements) = splitExpression(f.range.end)
    dispatch(f.body).map { newBody =>
      StanBlock(
        startStatements ++ endStatements :+ f.copy(
          range = f.range.copy(start = start, end = end),
          body = newBody
        )
      )
    }
  }

  override def handleWhile(w: StanWhileLoop)(implicit context: StanContext): State[StanStatement] = {
    val (cond, condStatements) = splitExpression(w.cond)
    dispatch(w.body).map { newBody =>
      StanBlock(
        condStatements :+ w.copy(cond = cond, body = newBody)
      )
    }
  }

  private def handleDistribution[T <: StanType, R <: StanType](
    dist: StanDistribution[T, R]
  )(implicit context: StanContext): (StanDistribution[T, R], Seq[StanStatement]) = {
    val newArgs = dist.args.map(a => splitExpression(a))
    val newDist: StanDistribution[T, R] = dist match {
      case c: StanContinuousDistribution[T, R]          => c.copy(args = newArgs.map(_._1))
      case dc: StanDiscreteDistributionWithCdf[T, R]    => dc.copy(args = newArgs.map(_._1))
      case dn: StanDiscreteDistributionWithoutCdf[T, R] => dn.copy(args = newArgs.map(_._1))
    }
    (newDist, newArgs.flatMap(_._2))
  }

  override def handleExpression[T <: StanType](
    expr: StanValue[T]
  )(implicit context: StanContext): State[StanValue[T]] = {
    // If we get here, we missed the implementation of something.
    throw new NotImplementedError("handleExpression should not be called")
  }

  def splitExpression[T <: StanType](
    expr: StanValue[T]
  )(implicit context: StanContext): (StanValue[T], Seq[StanStatement]) = expr match {
    case i: StanIndexOperator[_, T, _] =>
      val temp = StanLocalDeclaration(i.returnType, context.newName, derivedFromData = i.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val newIndices = i.indices.map(x => splitExpression(x))
      val newIndex = StanIndexOperator(i.returnType, i.value, newIndices.map(_._1))
      val statement = StanAssignment(temp, newIndex)
      (temp, newIndices.flatMap(_._2) :+ decl :+ statement)
    case s: StanSliceOperator[T, _] =>
      val temp = StanLocalDeclaration(s.returnType, context.newName, derivedFromData = s.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val newSlices = s.slices.map { slice =>
        val newStart = splitExpression(slice.start)
        val newEnd = splitExpression(slice.end)
        StanValueRange(newStart._1, newEnd._1) -> (newStart._2 ++ newEnd._2)
      }
      val newValue = StanSliceOperator(s.value, newSlices.map(_._1))
      val statement = StanAssignment(temp, newValue)
      (newValue, newSlices.flatMap(_._2) :+ decl :+ statement)
    case op: StanBinaryOperator[T, _, _]             =>
      val temp = StanLocalDeclaration(op.returnType, context.newName, derivedFromData = op.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val (leftValue, leftStatements) = splitExpression(op.left)
      val (rightValue, rightStatements) = splitExpression(op.right)
      val statement = StanAssignment(temp, op.copy(left = leftValue, right = rightValue))
      val statements = leftStatements ++ rightStatements :+ decl :+ statement
      (temp, statements)
    case op: StanUnaryOperator[_, T] =>
      val temp = StanLocalDeclaration(op.returnType, context.newName, derivedFromData = op.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val (rightValue, rightStatements) = splitExpression(op.right)
      val statement = StanAssignment(temp, op.copy(right = rightValue))
      (temp, rightStatements :+ decl :+ statement)
    case _ => (expr, Seq.empty)
  }

}
