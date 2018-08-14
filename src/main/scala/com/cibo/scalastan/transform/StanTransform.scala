/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, StanInt, StanType}
import com.cibo.scalastan.ast._

abstract class StanTransform[STATE](implicit ss: ScalaStan) {

  case class State[+T](run: STATE => (T, STATE)) {
    def flatMap[B](f: T => State[B]): State[B] = State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

    def map[B](f: T => B): State[B] = State { s =>
      val (a, s2) = run(s)
      f(a) -> s2
    }
  }

  object State {
    def pure[T](t: T): State[T] = State(s => (t, s))

    def sequence[T](vs: Seq[T])(f: T => State[T]): State[Seq[T]] = State { state =>
      vs.foldLeft(Vector.empty[T] -> state) { case ((lst, oldState), v) =>
        val (nv, newState) = f(v).run(oldState)
        (lst :+ nv) -> newState
      }
    }

    def get: State[STATE] = State { s => (s, s) }

    def put(v: STATE): State[STATE] = State { s => (v, v) }

    def update(f: STATE => STATE): State[STATE] = State { s => (s, f(s)) }
  }

  def initialState: STATE

  def run(program: StanProgram): StanProgram = handleProgram(program).run(initialState)._1

  def handleProgram(program: StanProgram): State[StanProgram] = {
    for {
      newFuncs <- State.sequence(program.functions)(handleFunction)
      newTransData <- State.sequence(program.transformedData)(handleTransformedData)
      newTransParams <- State.sequence(program.transformedParameters)(handleTransformedParameter)
      newGenQuants <- State.sequence(program.generatedQuantities)(handleGeneratedQuantity)
      newModel <- handleModel(program.model)
    } yield program.copy(
      functions = newFuncs,
      transformedData = newTransData,
      transformedParameters = newTransParams,
      generatedQuantities = newGenQuants,
      model = newModel
    )
  }

  def handleFunction(function: StanFunctionDeclaration): State[StanFunctionDeclaration] = {
    handleRoot(function.code).map(newCode => function.copy(code = newCode))
  }

  def handleTransformedData(transform: StanTransformedData): State[StanTransformedData] = {
    handleRoot(transform.code).map(newCode => transform.copy(code = newCode))
  }

  def handleTransformedParameter(
    transform: StanTransformedParameter
  ): State[StanTransformedParameter] = handleRoot(transform.code).map(newCode => transform.copy(code = newCode))

  def handleGeneratedQuantity(g: StanGeneratedQuantity): State[StanGeneratedQuantity] = {
    handleRoot(g.code).map(newCode => g.copy(code = newCode))
  }

  def handleModel(statement: StanStatement): State[StanStatement] = handleRoot(statement)

  def handleRoot(statement: StanStatement): State[StanStatement] = dispatch(statement)

  def dispatch(statement: StanStatement): State[StanStatement] = statement match {
    case t: StanBlock                 => handleBlock(t)
    case v: StanValueStatement        => handleValue(v)
    case a: StanAssignment            => handleAssignment(a)
    case f: StanForLoop               => handleFor(f)
    case w: StanWhileLoop             => handleWhile(w)
    case i: StanIfStatement           => handleIf(i)
    case b: StanBreakStatement        => handleBreak(b)
    case c: StanContinueStatement     => handleContinue(c)
    case s: StanSampleStatement[_, _] => handleSample(s)
    case r: StanReturnStatement       => handleReturn(r)
    case d: StanInlineDeclaration     => handleDecl(d)
  }

  def dispatchOption(statementOpt: Option[StanStatement]): State[Option[StanStatement]] = {
    statementOpt match {
      case Some(statement) => dispatch(statement).map(x => Some(x))
      case None            => State.pure(None)
    }
  }

  def handleRange(v: StanValueRange): State[StanValueRange] = {
    for {
      newStart <- handleExpression(v.start)
      newEnd <- handleExpression(v.end)
    } yield v.copy(start = newStart, end = newEnd)
  }

  // Process the LHS of an assignment or sample.
  private def handleLHS[T <: StanType](v: StanValue[T]): State[StanValue[T]] = v match {
    case i: StanIndexOperator[_, T, _] =>
      for {
        newValue <- handleExpression(i.value)
        newIndices <- State.sequence(i.indices)(handleExpression(_))
      } yield i.copy(value = newValue, indices = newIndices)
    case s: StanSliceOperator[T, _]    =>
      for {
        newValue <- handleExpression(s.value)
        newSlice <- handleRange(s.slice)
      } yield s.copy(value = newValue, slice = newSlice)
    case _                             => State.pure(v)
  }

  def handleBlock(b: StanBlock): State[StanStatement] = {
    State.sequence(b.children)(dispatch).map { newChildren =>
      b.copy(children = newChildren)
    }
  }

  def handleValue(v: StanValueStatement): State[StanStatement] = State.pure(v)

  def handleAssignment(a: StanAssignment): State[StanStatement] = {
    for {
      newLhs <- handleExpression(a.lhs)
      newRhs <- handleExpression(a.rhs)
    } yield a.copy(lhs = newLhs, rhs = newRhs)
  }

  def handleFor(f: StanForLoop): State[StanStatement] = {
    for {
      newRange <- handleRange(f.range)
      newBody <- dispatch(f.body)
    } yield f.copy(range = newRange, body = newBody)
  }

  def handleWhile(w: StanWhileLoop): State[StanStatement] = {
    for {
      newCond <- handleExpression(w.cond)
      newBody <- dispatch(w.body)
    } yield w.copy(cond = newCond, body = newBody)
  }

  def handleIf(i: StanIfStatement): State[StanStatement] = {
    for {
      newConds <- State.sequence(i.conds)(c => dispatch(c._2).map(x => c._1 -> x))
      newOtherwise <- dispatchOption(i.otherwise)
    } yield i.copy(conds = newConds, otherwise = newOtherwise)
  }

  def handleBreak(b: StanBreakStatement): State[StanStatement] = State.pure(b)

  def handleContinue(c: StanContinueStatement): State[StanStatement] = State.pure(c)

  def handleSample[T <: StanType, R <: StanType](s: StanSampleStatement[T, R]): State[StanStatement] = {
    for {
      newLhs <- handleLHS(s.left)
      newRhs <- handleDistribution(s.right)
    } yield s.copy(left = newLhs, right = newRhs)
  }

  def handleReturn(r: StanReturnStatement): State[StanStatement] = {
    handleExpression(r.result).map(newResult => r.copy(result = newResult))
  }

  def handleDecl(d: StanInlineDeclaration): State[StanStatement] = State.pure(d)

  private def handleDistribution[T <: StanType, R <: StanType](
    dist: StanDistribution[T, R]
  ): State[StanDistribution[T, R]] = {
    State.sequence(dist.args)(handleExpression(_)).map { newArgs =>
      dist match {
        case c: StanContinuousDistribution[T, R]          => c.copy(args = newArgs)
        case dc: StanDiscreteDistributionWithCdf[T, R]    => dc.copy(args = newArgs)
        case dn: StanDiscreteDistributionWithoutCdf[T, R] => dn.copy(args = newArgs)
      }
    }
  }

  def handleExpression[T <: StanType](expr: StanValue[T]): State[StanValue[T]] = expr match {
    case call: StanCall[T] => handleCall(call)
    case gt: StanGetTarget => handleGetTarget(gt)
    case tv: StanTargetValue => handleTargetValue(tv)
    case dn: StanDistributionNode[_] => handleDistributionNode(dn)
    case un: StanUnaryOperator[_, T] => handleUnaryOperator(un)
    case bn: StanBinaryOperator[T, _, _] => handleBinaryOperator(bn)
    case in: StanIndexOperator[_, T, _] => handleIndexOperator(in)
    case sl: StanSliceOperator[T, _] => handleSliceOperator(sl)
    case tr: StanTranspose[_, T] => handleTranspose(tr)
    case vr: StanDeclaration[T] => handleVariable(vr)
    case cn: StanConstant[T] => handleConstant(cn)
    case ar: StanArrayLiteral[_, _] => handleArray(ar)
    case st: StanStringLiteral => handleString(st)
    case lt: StanLiteral => handleLiteral(lt)
    case ud: StanUnknownDim => handleUnknownDim(ud)
  }

  def handleCall[T <: StanType](call: StanCall[T]): State[StanValue[T]] = {
    State.sequence(call.args)(handleExpression(_)).map(newArgs => call.copy(args = newArgs))
  }

  def handleGetTarget[T <: StanType](gt: StanGetTarget): State[StanValue[T]] =
    State.pure(gt.asInstanceOf[StanValue[T]])

  def handleTargetValue[T <: StanType](tv: StanTargetValue): State[StanValue[T]] =
    State.pure(tv.asInstanceOf[StanValue[T]])

  def handleDistributionNode[T <: StanType](d: StanDistributionNode[_ <: StanType]): State[StanValue[T]] = {
    for {
      newY <- handleExpression(d.y)
      newArgs <- State.sequence(d.args)(handleExpression(_))
    } yield d.copy(y = newY, args = newArgs).asInstanceOf[StanValue[T]]
  }

  def handleUnaryOperator[T <: StanType, R <: StanType](op: StanUnaryOperator[T, R]): State[StanValue[R]] = {
    handleExpression(op.right).map(newRight => op.copy(right = newRight))
  }

  def handleBinaryOperator[T <: StanType, L <: StanType, R <: StanType](
    op: StanBinaryOperator[T, L, R]
  ): State[StanValue[T]] = {
    for {
      newLeft <- handleExpression(op.left)
      newRight <- handleExpression(op.right)
    } yield op.copy(left = newLeft, right = newRight)
  }

  def handleIndexOperator[T <: StanType, N <: StanType, D <: StanDeclaration[_]](
    op: StanIndexOperator[T, N, D]
  ): State[StanValue[N]] = {
    for {
      newValue <- handleExpression(op.value)
      newIndices <- State.sequence(op.indices)(handleExpression(_))
    } yield op.copy(value = newValue, indices = newIndices)
  }

  def handleSliceOperator[T <: StanType, D <: StanDeclaration[_]](
    op: StanSliceOperator[T, D]
  ): State[StanValue[T]] = {
    for {
      newValue <- handleExpression(op.value)
      newSlice <- handleRange(op.slice)
    } yield op.copy(value = newValue, slice = newSlice)
  }

  def handleTranspose[T <: StanType, R <: StanType](tr: StanTranspose[T, R]): State[StanValue[R]] = {
    handleExpression(tr.value).map(newValue => tr.copy(value = newValue))
  }

  def handleConstant[T <: StanType](cn: StanConstant[T]): State[StanValue[T]] = State.pure(cn)

  def handleArray[R <: StanType, N <: StanType](ar: StanArrayLiteral[N, _]): State[StanValue[R]] = {
    State.sequence(ar.values)(handleExpression).map { newValues =>
      ar.copy(values = newValues).asInstanceOf[StanValue[R]]
    }
  }

  def handleString[T <: StanType](st: StanStringLiteral): State[StanValue[T]] =
    State.pure(st.asInstanceOf[StanValue[T]])

  def handleLiteral[T <: StanType](l: StanLiteral): State[StanValue[T]] = State.pure(l.asInstanceOf[StanValue[T]])

  def handleUnknownDim[T <: StanType](ud: StanUnknownDim): State[StanValue[T]] =
    State.pure(ud.asInstanceOf[StanValue[T]])

  def handleVariable[T <: StanType](decl: StanDeclaration[T]): State[StanValue[T]] = State.pure(decl)
}
