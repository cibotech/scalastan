package com.cibo.scalastan.transform

import com.cibo.scalastan.analysis.AvailableExpressions
import com.cibo.scalastan.{ScalaStan, StanType}
import com.cibo.scalastan.ast._

// Common subexpression elimination.
case class CSE()(implicit val ss: ScalaStan) extends StanTransform {

  private var root: Option[StanStatement] = None
  private var mapping: Map[Int, StanStatement] = Map.empty
  private var eliminated: Set[Int] = Set.empty

  private def commutative(op: StanBinaryOperator.Operator): Boolean = op match {
    case StanBinaryOperator.NotEqualTo => true
    case StanBinaryOperator.EqualTo    => true
    case StanBinaryOperator.Add        => true
    case StanBinaryOperator.Multiply   => true
    case StanBinaryOperator.LogicalAnd => true
    case StanBinaryOperator.LogicalOr  => true
    case _                             => false
  }

  private def sameValue[A <: StanType, B <: StanType](a: StanValue[A], b: StanValue[B]): Boolean = (a, b) match {
    case (b1: StanBinaryOperator[_, _, _], b2: StanBinaryOperator[_, _, _]) if b1.op == b2.op                       =>
      (sameValue(b1.left, b2.left) && sameValue(b1.right, b2.right)) ||
        (commutative(b1.op) && sameValue(b1.left, b2.right) && sameValue(b1.right, b2.left))
    case (u1: StanUnaryOperator[_, _], u2: StanUnaryOperator[_, _]) if u1.op == u2.op                               =>
      sameValue(u1.right, u2.right)
    case (i1: StanIndexOperator[_, _, _], i2: StanIndexOperator[_, _, _]) if i1.indices.length == i2.indices.length =>
      sameValue(i1.value, i2.value) && i1.indices.zip(i2.indices).forall(i => sameValue(i._1, i._2))
    case (s1: StanSliceOperator[_, _], s2: StanSliceOperator[_, _])                                                 =>
      sameValue(s1.value, s2.value) && sameValue(s1.slice.start, s2.slice.start) && sameValue(s1.slice.end, s2.slice.end)
    case (d1: StanDeclaration[_], d2: StanDeclaration[_])                                                           =>
      d1.name == d2.name
    case (c1: StanConstant[_], c2: StanConstant[_])                                                                 =>
      c1.value == c2.value
    case _                                                                                                          =>
      false
  }

  // Return true if the two statements are both compute the same expression.
  private def sameAssignment(a: StanStatement, b: StanStatement): Boolean = (a, b) match {
    case (a1: StanAssignment, a2: StanAssignment) if a1.op == StanAssignment.Assign && a2.op == a1.op =>
      sameValue(a1.rhs, a2.rhs)
    case _ => false
  }

  override protected def handleRoot(statement: StanStatement): StanStatement = {
    root = Some(statement)
    mapping = StanProgram.getStatements(statement).map(s => s.id -> s).toMap
    eliminated = Set.empty
    dispatch(statement)
  }

  override protected def handleAssignment(a: StanAssignment): StanStatement = {
    val ae: Set[Int] = AvailableExpressions(root.get).solve.lookup(a) -- eliminated
    ae.map(mapping.apply).find(s => sameAssignment(a, s)) match {
      case Some(other) =>
        // An equivalent available expression was found, use its value instead of our RHS.
        val oa = other.asInstanceOf[StanAssignment]
        eliminated += a.id
        StanAssignment(a.lhs, oa.lhs, a.op)
      case None =>
        // No equivalent available assignment found.
        a
    }
  }
}
