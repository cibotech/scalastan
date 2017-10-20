package com.cibo.scalastan

import scala.annotation.implicitNotFound

sealed trait TypeCheck

@implicitNotFound("multiplication not allowed for ${R} = ${A} * ${B}")
sealed class MultiplicationAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

object MultiplicationAllowed {
  implicit def scalarMultiplication[T <: StanScalarType] = new MultiplicationAllowed[T, T, T]
  implicit def scalarVectorMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, S, V]
  implicit def vectorScalarMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, V, S]
}

@implicitNotFound("elementwise multiplication not allowed for ${R} = ${A} * ${B}")
sealed class ElementwiseMultiplicationAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

object ElementwiseMultiplicationAllowed {
  implicit def elementwiseMultiplication[T <: StanCompoundType] = new MultiplicationAllowed[T, T, T]
  implicit def scalarVectorEMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, S, V]
  implicit def vectorScalarEMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, V, S]
}

@implicitNotFound("addition not allowed for ${R} = ${A} + ${B}")
sealed class AdditionAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

object AdditionAllowed {
  implicit val riAddition = new AdditionAllowed[StanReal, StanReal, StanInt]
  implicit val irAddition = new AdditionAllowed[StanReal, StanInt, StanReal]
  implicit def sameTypeAddition[T <: StanType] = new AdditionAllowed[T, T, T]
  implicit def scalarVectorAddtion[V <: StanCompoundType, S <: StanScalarType] = new AdditionAllowed[V, S, V]
  implicit def vectorScalarAddtion[V <: StanCompoundType, S <: StanScalarType] = new AdditionAllowed[V, V, S]
}

@implicitNotFound("modulus not allowed for ${T} (only int)")
sealed class ModulusAllowed[T <: StanType] extends TypeCheck

object ModulusAllowed {
  implicit def intModulus[T <: StanInt] = new ModulusAllowed[T]
}

@implicitNotFound("logical not allowed for type ${T}")
sealed class LogicalAllowed[T <: StanType] extends TypeCheck

object LogicalAllowed {
  implicit def intLogical[T <: StanInt] = new LogicalAllowed[T]
  implicit def realLogical[T <: StanReal] = new LogicalAllowed[T]
}

@implicitNotFound("distnace not allowed between types ${A} and ${B}")
sealed class DistanceAllowed[A <: StanType, B <: StanType] extends TypeCheck

object DistanceAllowed {
  implicit val vvDistance = new DistanceAllowed[StanVector, StanVector]
  implicit val vrDistance = new DistanceAllowed[StanVector, StanRowVector]
  implicit val rvDistance = new DistanceAllowed[StanRowVector, StanVector]
  implicit val rrDistance = new DistanceAllowed[StanRowVector, StanRowVector]
}

@implicitNotFound("transpose not allowed for ${R} = ${T}.t")
sealed class TranposeAllowed[T <: StanType, R <: StanType] extends TypeCheck

object TransposeAllowed {
  implicit val matrixTranspose = new TranposeAllowed[StanMatrix, StanMatrix]
  implicit val vectorTranspose = new TranposeAllowed[StanRowVector, StanVector]
  implicit val rowVectorTranspose = new TranposeAllowed[StanVector, StanRowVector]
}

@implicitNotFound("function only allowed in a GeneratedQuantity")
sealed trait InGeneratedQuantityBlock extends TypeCheck

object InGeneratedQuantityBlock extends InGeneratedQuantityBlock
