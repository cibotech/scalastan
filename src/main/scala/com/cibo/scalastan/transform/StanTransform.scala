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

import com.cibo.scalastan.{ScalaStan, StanType}
import com.cibo.scalastan.ast._

abstract class StanTransform(implicit ss: ScalaStan) {

  def run(program: StanProgram): StanProgram = handleProgram(program)

  protected def handleProgram(program: StanProgram): StanProgram = program.copy(
    functions = program.functions.map(handleFunction),
    transformedData = program.transformedData.map(handleTransformedData),
    transformedParameters = program.transformedParameters.map(handleTransformedParameter),
    generatedQuantities = program.generatedQuantities.map(handleGeneratedQuantity),
    model = handleModel(program.model)
  )

  protected def handleFunction(function: StanFunctionDeclaration): StanFunctionDeclaration = {
    function.copy(code = handleRoot(function.code))
  }

  protected def handleTransformedData(transform: StanTransformedData): StanTransformedData = {
    transform.copy(code = handleRoot(transform.code))
  }

  protected def handleTransformedParameter(
    transform: StanTransformedParameter
  ): StanTransformedParameter = transform.copy(code = handleRoot(transform.code))

  protected def handleGeneratedQuantity(g: StanGeneratedQuantity): StanGeneratedQuantity = {
    g.copy(code = handleRoot(g.code))
  }

  protected def handleModel(statement: StanStatement): StanStatement = handleRoot(statement)

  protected def handleRoot(statement: StanStatement): StanStatement = dispatch(statement)

  protected def dispatch(statement: StanStatement): StanStatement = statement match {
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

  protected def handleRange(v: StanValueRange): StanValueRange =
    v.copy(start = handleExpression(v.start), end = handleExpression(v.end))

  // Process the LHS of an assignment or sample.
  private def handleLHS[T <: StanType](v: StanValue[T]): StanValue[T] = v match {
    case i: StanIndexOperator[_, T, _] =>
      i.copy(
        value = handleExpression(i.value),
        indices = i.indices.map(x => handleExpression(x))
      )
    case s: StanSliceOperator[T, _]    =>
      s.copy(value = handleExpression(s.value), slice = handleRange(s.slice))
    case _                             => v
  }

  protected def handleBlock(b: StanBlock): StanStatement = b.copy(children = b.children.map(dispatch))

  protected def handleValue(v: StanValueStatement): StanStatement = v

  protected def handleAssignment(a: StanAssignment): StanStatement = a.copy(
    lhs = handleExpression(a.lhs),
    rhs = handleExpression(a.rhs)
  )

  protected def handleFor(f: StanForLoop): StanStatement =
    f.copy(range = handleRange(f.range), body = dispatch(f.body))

  protected def handleWhile(w: StanWhileLoop): StanStatement =
    w.copy(cond = handleExpression(w.cond), body = dispatch(w.body))

  protected def handleIf(i: StanIfStatement): StanStatement = i.copy(
    conds = i.conds.map(cond => (cond._1, dispatch(cond._2))),
    otherwise = i.otherwise.map(o => dispatch(o))
  )

  protected def handleBreak(b: StanBreakStatement): StanStatement = b

  protected def handleContinue(c: StanContinueStatement): StanStatement = c

  protected def handleSample[T <: StanType, R <: StanType](s: StanSampleStatement[T, R]): StanStatement = {
    val lhs = handleLHS(s.left)
    val rhs = handleDistribution(s.right)
    s.copy(left = lhs, right = rhs)
  }

  protected def handleReturn(r: StanReturnStatement): StanStatement = r.copy(result = handleExpression(r.result))

  protected def handleDecl(d: StanInlineDeclaration): StanStatement = d

  private def handleDistribution[T <: StanType, R <: StanType](dist: StanDistribution[T, R]): StanDistribution[T, R] = {
    val newArgs = dist.args.map(a => handleExpression(a))
    dist match {
      case c: StanContinuousDistribution[T, R]          => c.copy(args = newArgs)
      case dc: StanDiscreteDistributionWithCdf[T, R]    => dc.copy(args = newArgs)
      case dn: StanDiscreteDistributionWithoutCdf[T, R] => dn.copy(args = newArgs)
    }
  }

  protected def handleExpression[T <: StanType](expr: StanValue[T]): StanValue[T] = expr match {
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

  protected def handleCall[T <: StanType](call: StanCall[T]): StanValue[T] = call.copy(
    args = call.args.map(handleExpression(_))
  )

  protected def handleGetTarget[T <: StanType](gt: StanGetTarget): StanValue[T] = gt.asInstanceOf[StanValue[T]]

  protected def handleTargetValue[T <: StanType](tv: StanTargetValue): StanValue[T] = tv.asInstanceOf[StanValue[T]]

  protected def handleDistributionNode[T <: StanType](d: StanDistributionNode[_ <: StanType]): StanValue[T] = d.copy(
    y = handleExpression(d.y),
    args = d.args.map(handleExpression(_))
  ).asInstanceOf[StanValue[T]]

  protected def handleUnaryOperator[T <: StanType, R <: StanType](op: StanUnaryOperator[T, R]): StanValue[R] = {
    op.copy(right = handleExpression(op.right))
  }

  protected def handleBinaryOperator[T <: StanType, L <: StanType, R <: StanType](
    op: StanBinaryOperator[T, L, R]
  ): StanValue[T] = op.copy(
    left = handleExpression(op.left),
    right = handleExpression(op.right)
  )

  protected def handleIndexOperator[T <: StanType, N <: StanType, D <: StanDeclaration[_]](
    op: StanIndexOperator[T, N, D]
  ): StanValue[N] = op.copy(
    value = handleExpression(op.value),
    indices = op.indices.map(handleExpression(_))
  )

  protected def handleSliceOperator[T <: StanType, D <: StanDeclaration[_]](
    op: StanSliceOperator[T, D]
  ): StanValue[T] = op.copy(
    value = handleExpression(op.value),
    slice = handleRange(op.slice)
  )

  protected def handleTranspose[T <: StanType, R <: StanType](tr: StanTranspose[T, R]): StanValue[R] = tr.copy(
    value = handleExpression(tr.value)
  )

  protected def handleConstant[T <: StanType](cn: StanConstant[T]): StanValue[T] = cn

  protected def handleArray[R <: StanType, N <: StanType](ar: StanArrayLiteral[N, _]): StanValue[R] = ar.copy(
    values = ar.values.map(handleExpression)
  ).asInstanceOf[StanValue[R]]

  protected def handleString[T <: StanType](st: StanStringLiteral): StanValue[T] = st.asInstanceOf[StanValue[T]]

  protected def handleLiteral[T <: StanType](l: StanLiteral): StanValue[T] = l.asInstanceOf[StanValue[T]]

  protected def handleUnknownDim[T <: StanType](ud: StanUnknownDim): StanValue[T] = ud.asInstanceOf[StanValue[T]]

  protected def handleVariable[T <: StanType](decl: StanDeclaration[T]): StanValue[T] = decl
}
