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

import com.cibo.scalastan.ast._

trait StanTransform[STATE] {

  protected val initialState: STATE

  final def run(program: StanProgram): StanProgram = handleProgram(program, initialState)

  protected def handleProgram(program: StanProgram, state: STATE): StanProgram = StanProgram(
    data = program.data,
    parameters = program.parameters,
    functions = program.functions.map(f => handleFunction(f, state)),
    transformedData = program.transformedData.map(t => handleTransformedData(t, state)),
    transformedParameters = program.transformedParameters.map(t => handleTransformedParameter(t, state)),
    generatedQuantities = program.generatedQuantities.map(g => handleGeneratedQuantity(g, state)),
    model = handleModel(program.model, state)
  )

  protected def handleFunction(function: StanFunctionDeclaration, state: STATE): StanFunctionDeclaration = {
    StanFunctionDeclaration(
      returnValue = function.returnValue,
      inputs = function.inputs,
      code = dispatch(function.code, state)
    )
  }

  protected def handleTransformedData(transform: StanTransformedData, state: STATE): StanTransformedData = {
    StanTransformedData(
      result = transform.result,
      code = dispatch(transform.code, state)
    )
  }

  protected def handleTransformedParameter(
    transform: StanTransformedParameter,
    state: STATE
  ): StanTransformedParameter = StanTransformedParameter(
    result = transform.result,
    code = dispatch(transform.code, state)
  )

  protected def handleGeneratedQuantity(g: StanGeneratedQuantity, state: STATE): StanGeneratedQuantity = {
    StanGeneratedQuantity(
      result = g.result,
      code = dispatch(g.code, state)
    )
  }

  protected def handleModel(statement: StanStatement, state: STATE): StanStatement = dispatch(statement, state)

  protected def dispatch(statement: StanStatement, state: STATE): StanStatement = statement match {
    case t: StanBlock              => handleBlock(t, state)
    case v: StanValueStatement     => handleValue(v, state)
    case a: StanAssignment         => handleAssignment(a, state)
    case f: StanForLoop            => handleFor(f, state)
    case w: StanWhileLoop          => handleWhile(w, state)
    case i: StanIfStatement        => handleIf(i, state)
    case b: StanBreakStatement     => handleBreak(b, state)
    case c: StanContinueStatement  => handleContinue(c, state)
    case s: StanSampleStatement[_] => handleSample(s, state)
    case r: StanReturnStatement    => handleReturn(r, state)
    case d: StanInlineDeclaration  => handleDecl(d, state)
  }

  protected def handleBlock(b: StanBlock, state: STATE): StanStatement =
    StanBlock(b.children.map(c => dispatch(c, state)))

  protected def handleValue(v: StanValueStatement, state: STATE): StanStatement = v

  protected def handleAssignment(a: StanAssignment, state: STATE): StanStatement = a

  protected def handleFor(f: StanForLoop, state: STATE): StanStatement =
    StanForLoop(f.decl, f.range, dispatch(f.body, state))

  protected def handleWhile(w: StanWhileLoop, state: STATE): StanStatement =
    StanWhileLoop(w.cond, dispatch(w.body, state))

  protected def handleIf(i: StanIfStatement, state: STATE): StanStatement = StanIfStatement(
    i.conds.map(cond => (cond._1, dispatch(cond._2, state))),
    i.otherwise.map(o => dispatch(o, state))
  )

  protected def handleBreak(b: StanBreakStatement, state: STATE): StanStatement = b

  protected def handleContinue(c: StanContinueStatement, state: STATE): StanStatement = c

  protected def handleSample(s: StanSampleStatement[_], state: STATE): StanStatement = s

  protected def handleReturn(r: StanReturnStatement, state: STATE): StanStatement = r

  protected def handleDecl(d: StanInlineDeclaration, state: STATE): StanStatement = d
}
