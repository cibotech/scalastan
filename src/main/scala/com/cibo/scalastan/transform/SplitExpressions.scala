package com.cibo.scalastan.transform

import scala.language.existentials

import com.cibo.scalastan.{ScalaStan, StanType}
import com.cibo.scalastan.ast._

// Break apart expressions into one operator per statement.
case class SplitExpressions()(implicit val ss: ScalaStan) extends StanTransform {

  private var decls: Seq[StanLocalDeclaration[_]] = Seq.empty

  override def run(program: StanProgram): StanProgram = {
    val updated = super.run(program)
    AstSimplifier().run(updated)
  }

  private def handleLHS[T <: StanType](expr: StanValue[T]): (StanValue[T], Seq[StanStatement]) = {
    expr match {
      case i: StanIndexOperator[_, T, _] =>
        val newIndices = i.indices.map(x => splitExpression(x))
        val newIndex = StanIndexOperator(i.returnType, i.value, newIndices.map(_._1))
        (newIndex, newIndices.flatMap(_._2))
      case s: StanSliceOperator[T, _]    =>
        val newStart = splitExpression(s.slice.start)
        val newEnd = splitExpression(s.slice.end)
        val newSlice = StanSliceOperator(s.value, StanValueRange(newStart._1, newEnd._1))
        (newSlice, newStart._2 ++ newEnd._2)
      case x                             => (x, Seq.empty)
    }
  }

  override protected def handleAssignment(a: StanAssignment): StanStatement = {
    val (lhsValue, lhsStatements) = handleLHS(a.lhs)
    val (rhsValue, rhsStatements) = splitExpression(a.rhs)
    (lhsValue, rhsValue) match {
      case (ld: StanDeclaration[_], _) =>
        val newStatement = StanAssignment(lhsValue, rhsValue, a.op)
        StanBlock(lhsStatements ++ rhsStatements :+ newStatement)
      case (_, st: StanDeclaration[_]) =>
        val newStatement = StanAssignment(lhsValue, rhsValue, a.op)
        StanBlock(lhsStatements ++ rhsStatements :+ newStatement)
      case _ =>
        val temp = StanLocalDeclaration(rhsValue.returnType, derivedFromData = rhsValue.isDerivedFromData)
        val decl = StanInlineDeclaration(temp)
        val load = StanAssignment(temp, rhsValue)
        val store = StanAssignment(lhsValue, temp)
        StanBlock(lhsStatements ++ rhsStatements :+ decl :+ load :+ store)
    }
  }

  override protected def handleReturn(r: StanReturnStatement): StanStatement = {
    val (value, statements) = splitExpression(r.result)
    StanBlock(statements :+ StanReturnStatement(value))
  }

  override protected def handleSample[T <: StanType](s: StanSampleStatement[T]): StanStatement = {
    val (lhsValue, lhsStatements) = handleLHS(s.left)
    val (rhsValue, rhsStatements) = handleDistribution(s.right)
    StanBlock(
      lhsStatements ++ rhsStatements :+ StanSampleStatement(lhsValue, rhsValue)
    )
  }

  override protected def handleIf(i: StanIfStatement): StanStatement = {
    val newConds = i.conds.map { case (cond, statement) =>
      val (newCond, newStatements) = splitExpression(cond)
      (newStatements, newCond -> dispatch(statement))
    }
    val statements = newConds.flatMap(_._1)
    StanBlock(
      statements :+ i.copy(
        conds = newConds.map(_._2),
        otherwise = i.otherwise.map(dispatch)
      )
    )
  }

  override protected def handleFor(f: StanForLoop): StanStatement = {
    val (start, startStatements) = splitExpression(f.range.start)
    val (end, endStatements) = splitExpression(f.range.end)
    StanBlock(
      startStatements ++ endStatements :+ f.copy(
        range = f.range.copy(start = start, end = end),
        body = dispatch(f.body)
      )
    )
  }

  override protected def handleWhile(w: StanWhileLoop): StanStatement = {
    val (cond, condStatements) = splitExpression(w.cond)
    StanBlock(
      condStatements :+ w.copy(cond = cond, body = dispatch(w.body))
    )
  }

  private def handleDistribution[T <: StanType](
    dist: StanDistribution[T]
  ): (StanDistribution[T], Seq[StanStatement]) = {
    val newArgs = dist.args.map(a => splitExpression(a))
    val newDist: StanDistribution[T] = dist match {
      case c: StanContinuousDistribution[T, _]          => c.copy(args = newArgs.map(_._1))
      case dc: StanDiscreteDistributionWithCdf[T, _]    => dc.copy(args = newArgs.map(_._1))
      case dn: StanDiscreteDistributionWithoutCdf[T, _] => dn.copy(args = newArgs.map(_._1))
    }
    (newDist, newArgs.flatMap(_._2))
  }

  protected override def handleExpression[T <: StanType](expr: StanValue[T]): StanValue[T] = {
    // If we get here, we missed the implementation of something.
    throw new NotImplementedError("handleExpression should not be called")
  }

  private def splitExpression[T <: StanType](expr: StanValue[T]): (StanValue[T], Seq[StanStatement]) = expr match {
    case i: StanIndexOperator[_, T, _] =>
      val temp = StanLocalDeclaration(i.returnType, derivedFromData = i.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val newIndices = i.indices.map(x => splitExpression(x))
      val newIndex = StanIndexOperator(i.returnType, i.value, newIndices.map(_._1))
      val statement = StanAssignment(temp, newIndex)
      (temp, newIndices.flatMap(_._2) :+ decl :+ statement)
    case s: StanSliceOperator[T, _] =>
      val temp = StanLocalDeclaration(s.returnType, derivedFromData = s.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val newStart = splitExpression(s.slice.start)
      val newEnd = splitExpression(s.slice.end)
      val newSlice = StanSliceOperator(s.value, StanValueRange(newStart._1, newEnd._1))
      val statement = StanAssignment(temp, newSlice)
      (newSlice, newStart._2 ++ newEnd._2 :+ decl :+ statement)
    case op: StanBinaryOperator[T, _, _]             =>
      val temp = StanLocalDeclaration(op.returnType, derivedFromData = op.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val (leftValue, leftStatements) = splitExpression(op.left)
      val (rightValue, rightStatements) = splitExpression(op.right)
      val statement = StanAssignment(temp, op.copy(left = leftValue, right = rightValue))
      val statements = leftStatements ++ rightStatements :+ decl :+ statement
      (temp, statements)
    case op: StanUnaryOperator[_, T] =>
      val temp = StanLocalDeclaration(op.returnType, derivedFromData = op.isDerivedFromData)
      val decl = StanInlineDeclaration(temp)
      val (rightValue, rightStatements) = splitExpression(op.right)
      val statement = StanAssignment(temp, op.copy(right = rightValue))
      (temp, rightStatements :+ decl :+ statement)
    case _ => (expr, Seq.empty)
  }

}
