/*
 * Copyright (c) 2017 - 2020 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.PrintWriter

import com.cibo.scalastan.ast._

trait StanCodeBlock extends Implicits {

  implicit val _context: StanContext
  implicit val _code: StanProgramBuilder = new StanProgramBuilder

  object stan extends StanFunctions with StanDistributions

  private def insertLocal[T <: StanType](
    typeConstructor: T,
    valueOpt: Option[StanValue[T]],
    name: sourcecode.Name
  ): StanLocalDeclaration[T] = {
    if (typeConstructor.lower.isDefined || typeConstructor.upper.isDefined) {
      throw new IllegalStateException("local variables may not have constraints")
    }

    val decl = StanLocalDeclaration[T](typeConstructor, _context.fixName(name.value))
    _code.insert(StanInlineDeclaration(decl, valueOpt))
    decl
  }

  def local[T <: StanType](typeConstructor: T)(implicit name: sourcecode.Name): StanLocalDeclaration[T] = {
    insertLocal(typeConstructor, None, name)
  }

  def local[T <: StanType](
    typeConstructor: T,
    value: StanValue[T]
  )(implicit name: sourcecode.Name): StanLocalDeclaration[T] = {
    insertLocal(typeConstructor, Some(value), name)
  }

  case class when(cond: StanValue[StanInt])(block: => Unit) {
    _code.enter()
    block
    _code.leave(code => ast.StanIfStatement(Seq((cond, StanBlock(code))), None))

    def when(cond: StanValue[StanInt])(otherBlock: => Unit): when = {
      _code.enter()
      otherBlock
      _code.handleElseIf(cond)
      this
    }

    def otherwise(otherBlock: => Unit): Unit = {
      _code.enter()
      otherBlock
      _code.handleElse()
    }
  }

  def when[T <: StanType](cond: StanValue[StanInt], ifTrue: StanValue[T], ifFalse: StanValue[T]): StanValue[T] = {
    StanTernaryOperator(cond, ifTrue, ifFalse)
  }

  def range(start: StanValue[StanInt], end: StanValue[StanInt]): StanValueRange = StanValueRange(start, end)

  def loop(cond: StanValue[StanInt])(body: => Unit): Unit = {
    _code.enter()
    body
    _code.leave(children => ast.StanWhileLoop(cond, StanBlock(children)))
  }

  def break: Unit = {
    _code.append(StanBreakStatement())
  }

  def continue: Unit = {
    _code.append(StanContinueStatement())
  }

  private[scalastan] def emitTopLevelLocals(writer: PrintWriter): Unit = {
    // Values have to be declared before code.  Since we treat transformations
    // differently, we need to make a special pass to combine the top-level locals.
    _code.results.children.foreach { child =>
      if (child.isInstanceOf[StanInlineDeclaration]) {
        child.emit(writer, 1)
      }
    }
  }

  private[scalastan] def emitCode(writer: PrintWriter): Unit = {
    _code.results.children.foreach { child =>
      if (!child.isInstanceOf[StanInlineDeclaration]) {
        child.emit(writer, 1)
      }
    }
  }
}
