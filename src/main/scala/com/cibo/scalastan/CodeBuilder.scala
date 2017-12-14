package com.cibo.scalastan

import com.cibo.scalastan.ast.{StanBlock, StanIfStatement, StanStatement, StanValue}

import scala.collection.mutable.ArrayBuffer

protected class CodeBuilder {

  // Stack of statements.
  // New statements are appended to the last buffer.
  // Entering a new scope extends the number of buffers.
  private val stack = new ArrayBuffer[ArrayBuffer[StanStatement]]()

  // Create the top-level scope.
  stack += ArrayBuffer()

  // Enter a new scope.
  def enter(): Unit = {
    stack += ArrayBuffer()
  }

  // Leave the current scope (collapsing the statements collected using the specified function).
  def leave(f: Seq[StanStatement] => StanStatement): Unit = {
    require(stack.size > 1)
    val result = f(stack.remove(stack.size - 1))
    stack.last += result
  }

  // Special handling for "else if" statements.
  def handleElseIf(cond: StanValue[StanInt]): Unit = {
    val inside = stack.remove(stack.size - 1)
    val ifStatement = stack.last.remove(stack.last.size - 1).asInstanceOf[StanIfStatement]
    stack.last += ifStatement.copy(ifStatement.conds :+ (cond, StanBlock(inside)))
  }

  // Special handling for "else" statements.
  def handleElse(): Unit = {
    val inside = stack.remove(stack.size - 1)
    val ifStatement = stack.last.remove(stack.last.size - 1).asInstanceOf[StanIfStatement]
    stack.last += ifStatement.copy(otherwise = Some(StanBlock(inside)))
  }

  def insert(s: StanStatement): Unit = {
    require(stack.nonEmpty)
    stack.last.insert(0, s)
  }

  def append(s: StanStatement): Unit = {
    require(stack.nonEmpty)
    stack.last += s
  }

  lazy val results: StanBlock = {
    require(stack.size == 1)
    StanBlock(stack.last)
  }
}
