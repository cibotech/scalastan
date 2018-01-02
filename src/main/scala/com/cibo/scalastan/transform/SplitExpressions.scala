package com.cibo.scalastan.transform

import com.cibo.scalastan.StanType
import com.cibo.scalastan.ast._

// Break apart expressions into one operator per statement.
/*
object SplitExpressions extends StanTransform {

  class LocalDecls {
    val decls: scala.collection.mutable.Seq[StanLocalDeclaration[_]] = scala.collection.mutable.Seq.empty
  }

  type STATE = LocalDecls

  protected val initialState = new LocalDecls

  override protected def handleReturn(r: StanReturnStatement, state: STATE): StanStatement = {
    StanReturnStatement(splitExpression(r.result))
  }

  private def getType(expr: StanValue[_]): StanType = expr match {
    case a: StanArrayLiteral[_] => getType(a.values.head).apply()  //TODO: what if values is empty?
    case c: StanCall[_] =>
  }

  private def splitExpression[T <: StanType](expr: StanValue[T]): StanValue[T] = expr match {
    case op: StanBinaryOperator[T, _, _] if op.inputs.size > 2 =>
  }

}
*/
