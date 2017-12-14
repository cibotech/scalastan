/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.cibo.scalastan.ast.{StanLocalDeclaration, StanNode, StanParameterDeclaration}

import scala.annotation.implicitNotFound

protected sealed trait TypeCheck

@implicitNotFound("${T} not a array/vector/matrix type")
protected sealed class IsCompoundType[T <: StanType]

protected object IsCompoundType {
  implicit def isCompound[T <: StanCompoundType] = new IsCompoundType[T]
}

@implicitNotFound("${T} not an int/real type")
protected sealed class IsScalarType[T <: StanType]

protected object IsScalarType {
  implicit def IsScalarType[T <: StanScalarType] = new IsScalarType[T]
}

@implicitNotFound("multiplication not allowed for ${R} = ${A} * ${B}")
protected sealed class MultiplicationAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

protected object MultiplicationAllowed {
  implicit val riMultiplication = new MultiplicationAllowed[StanReal, StanReal, StanInt]
  implicit val irMultiplication = new MultiplicationAllowed[StanReal, StanInt, StanReal]
  implicit def scalarMultiplication[T <: StanScalarType] = new MultiplicationAllowed[T, T, T]
  implicit def scalarVectorMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, S, V]
  implicit def vectorScalarMultiplication[S <: StanScalarType, V <: StanCompoundType] = new MultiplicationAllowed[V, V, S]
  implicit val matVecMultiplication = new MultiplicationAllowed[StanVector, StanMatrix, StanVector]
  implicit val rvMatMultiplication = new MultiplicationAllowed[StanRowVector, StanRowVector, StanMatrix]
  implicit val rvVecMultiplication = new MultiplicationAllowed[StanReal, StanRowVector, StanVector]
  implicit val vecRvMultiplication = new MultiplicationAllowed[StanMatrix, StanVector, StanRowVector]
}

@implicitNotFound("division not allowed for ${R} = ${A} * ${B}")
protected sealed class DivisionAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

protected object DivisionAllowed {
  implicit val riDivision = new DivisionAllowed[StanReal, StanReal, StanInt]
  implicit val irDivision = new DivisionAllowed[StanReal, StanInt, StanReal]
  implicit def scalarDivision[T <: StanScalarType] = new DivisionAllowed[T, T, T]
  implicit def vecScalarDivision[V <: StanVectorLike, T <: StanScalarType] = new DivisionAllowed[V, V, T]
  implicit def matScalarDivision[T <: StanScalarType] = new DivisionAllowed[StanMatrix, StanMatrix, T]
  implicit val vecMatDivision = new DivisionAllowed[StanRowVector, StanRowVector, StanMatrix]
  implicit val matMatDivision = new DivisionAllowed[StanMatrix, StanMatrix, StanMatrix]
}

@implicitNotFound("left division not allowed for ${R} = ${A} * ${B}")
protected sealed class LeftDivisionAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

protected object LeftDivisionAllowed {
  implicit val matVecDivision = new LeftDivisionAllowed[StanVector, StanMatrix, StanVector]
  implicit val matMatDivision = new LeftDivisionAllowed[StanMatrix, StanMatrix, StanMatrix]
}

@implicitNotFound("element-wise division not allowed for ${R} = ${A} :/ ${B}")
protected sealed class ElementWiseDivisionAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

protected object ElementWiseDivisionAllowed {
  implicit def sameTypeDivision[T <: StanCompoundType] = new ElementWiseDivisionAllowed[T, T, T]
  implicit def csDivision[C <: StanCompoundType, S <: StanScalarType] = new ElementWiseDivisionAllowed[C, C, S]
  implicit def scDivision[C <: StanCompoundType, S <: StanScalarType] = new ElementWiseDivisionAllowed[C, S, C]
}

@implicitNotFound("addition not allowed for ${R} = ${A} + ${B}")
protected sealed class AdditionAllowed[R <: StanType, A <: StanType, B <: StanType] extends TypeCheck

protected object AdditionAllowed {
  implicit val riAddition = new AdditionAllowed[StanReal, StanReal, StanInt]
  implicit val irAddition = new AdditionAllowed[StanReal, StanInt, StanReal]
  implicit def sameTypeAddition[T <: StanType] = new AdditionAllowed[T, T, T]
  implicit def scalarVectorAddtion[V <: StanCompoundType, S <: StanScalarType] = new AdditionAllowed[V, S, V]
  implicit def vectorScalarAddtion[V <: StanCompoundType, S <: StanScalarType] = new AdditionAllowed[V, V, S]
}

@implicitNotFound("modulus not allowed for ${T} (only int)")
protected sealed class ModulusAllowed[T <: StanType] extends TypeCheck

protected object ModulusAllowed {
  implicit def intModulus[T <: StanInt] = new ModulusAllowed[T]
}

@implicitNotFound("logical not allowed for type ${T}")
protected sealed class LogicalAllowed[T <: StanType] extends TypeCheck

protected object LogicalAllowed {
  implicit def intLogical[T <: StanInt] = new LogicalAllowed[T]
  implicit def realLogical[T <: StanReal] = new LogicalAllowed[T]
}

@implicitNotFound("distance not allowed between types ${A} and ${B}")
protected sealed class DistanceAllowed[A <: StanType, B <: StanType] extends TypeCheck

protected object DistanceAllowed {
  implicit val vvDistance = new DistanceAllowed[StanVector, StanVector]
  implicit val vrDistance = new DistanceAllowed[StanVector, StanRowVector]
  implicit val rvDistance = new DistanceAllowed[StanRowVector, StanVector]
  implicit val rrDistance = new DistanceAllowed[StanRowVector, StanRowVector]
}

@implicitNotFound("transpose not allowed for ${R} = ${T}.t")
protected sealed class TransposeAllowed[T <: StanType, R <: StanType] extends TypeCheck

protected object TransposeAllowed {
  implicit val matrixTranspose = new TransposeAllowed[StanMatrix, StanMatrix]
  implicit val vectorTranspose = new TransposeAllowed[StanRowVector, StanVector]
  implicit val rowVectorTranspose = new TransposeAllowed[StanVector, StanRowVector]
}

@implicitNotFound("only allowed in a GeneratedQuantity")
protected sealed trait InGeneratedQuantityBlock extends TypeCheck

protected object InGeneratedQuantityBlock extends InGeneratedQuantityBlock

@implicitNotFound("only allowed in a ParameterTransform")
protected sealed trait InParameterTransform extends TypeCheck

protected object InParameterTransform extends InParameterTransform

@implicitNotFound("assignment not allowed")
protected sealed class AssignmentAllowed[N <: StanNode] extends TypeCheck

protected object AssignmentAllowed {
  implicit def paramAssignment[T <: StanType](
    implicit ev: InParameterTransform
  ): AssignmentAllowed[StanParameterDeclaration[T]] = new AssignmentAllowed[StanParameterDeclaration[T]]

  implicit def localAssignment[T <: StanType]: AssignmentAllowed[StanLocalDeclaration[T]] =
    new AssignmentAllowed[StanLocalDeclaration[T]]

  implicit def generatedQuantityAssignment[T <: StanType](
    implicit ev: InGeneratedQuantityBlock
  ): AssignmentAllowed[StanParameterDeclaration[T]] = new AssignmentAllowed[StanParameterDeclaration[T]]
}

@implicitNotFound("implicit conversion from ${FROM} to ${TO} not allowed")
protected sealed class CanConvert[FROM <: StanType, TO <: StanType]

protected object CanConvert {
  implicit def compoundType[T <: StanCompoundType] = new CanConvert[T, T]
  implicit def scalar2real[T <: StanScalarType] = new CanConvert[T, StanReal]
  implicit val int2int = new CanConvert[StanInt, StanInt]
}

@implicitNotFound("continuous type required, got ${T}")
protected sealed class ContinuousType[T <: StanType] extends TypeCheck

protected object ContinuousType {
  implicit def hasRealElement[T <: StanType](implicit ev: T#ELEMENT_TYPE =:= StanReal) = new ContinuousType[T]

  // Because Stan auto-converts ints to reals, we allow bare ints to be treated as continuous.
  implicit val canConvertToReal = new ContinuousType[StanInt]
}

@implicitNotFound("discrete type required, got ${T}")
protected sealed class DiscreteType[T <: StanType] extends TypeCheck

protected object DiscreteType {
  implicit def hasIntElement[T <: StanType](implicit ev: T#ELEMENT_TYPE =:= StanInt) = new DiscreteType[T]
}

@implicitNotFound("${T} not a vector, row vector, or array")
protected sealed class IsVectorLikeOrArray[T <: StanType] extends TypeCheck

protected object IsVectorLikeOrArray {
  implicit val isVector = new IsVectorLikeOrArray[StanVector]
  implicit val isRowVector = new IsVectorLikeOrArray[StanRowVector]
  implicit def isArray[T <: StanType] = new IsVectorLikeOrArray[StanArray[T]]
}

@implicitNotFound("${T} not a vector or matrix")
protected sealed class IsVectorOrMatrix[T <: StanType] extends TypeCheck

protected object IsVectorOrMatrix {
  implicit val isVector = new IsVectorOrMatrix[StanVector]
  implicit val isMatrix = new IsVectorOrMatrix[StanMatrix]
}

@implicitNotFound("${T} not a row vector or matrix")
protected sealed class IsRowVectorOrMatrix[T <: StanType] extends TypeCheck

protected object IsRowVectorOrMatrix {
  implicit val isRowVector = new IsRowVectorOrMatrix[StanRowVector]
  implicit val isMatrix = new IsRowVectorOrMatrix[StanMatrix]
}

@implicitNotFound("${T} not a vector, row vector, or matrix")
protected sealed class IsVectorLikeOrMatrix[T <: StanType] extends TypeCheck

protected object IsVectorLikeOrMatrix {
  implicit val isVector = new IsVectorLikeOrMatrix[StanVector]
  implicit val isRowVector = new IsVectorLikeOrMatrix[StanRowVector]
  implicit val isMatrix = new IsVectorLikeOrMatrix[StanMatrix]
}

@implicitNotFound("${T} not a vector, row vector, or an array of vector or row vector")
protected sealed class IsVectorLikeOrArrayVectorLike[T <: StanType] extends TypeCheck

protected object IsVectorLikeOrArrayVectorLike {
  implicit def isVectorLike[T <: StanVectorLike] = new IsVectorLikeOrArrayVectorLike[T]
  implicit def isArray[T <: StanVectorLike] = new IsVectorLikeOrArrayVectorLike[StanArray[T]]
}

@implicitNotFound("${T} must be 0 or 1 dimensional")
protected sealed class Is0or1Dimensional[T <: StanType] extends TypeCheck

protected object Is0or1Dimensional {
  implicit def isScalar[T <: StanScalarType] = new Is0or1Dimensional[T]
  implicit def isVectorLike[T <: StanVectorLike] = new Is0or1Dimensional[T]
  implicit val isIntArray = new Is0or1Dimensional[StanArray[StanInt]]
  implicit val isRealArray = new Is0or1Dimensional[StanArray[StanReal]]
  implicit val isCatArray = new Is0or1Dimensional[StanArray[StanCategorical]]
}

@implicitNotFound("${T} must be 1 or 2 dimensional")
protected sealed class Is1or2Dimensional[T <: StanType] extends TypeCheck

protected object Is1or2Dimensional {
  implicit def isVectorLike[T <: StanVectorLike] = new Is1or2Dimensional[T]
  implicit def isMatrix[T <: StanMatrix] = new Is1or2Dimensional[T]
  implicit def isArray0or1[T <: StanType: Is0or1Dimensional] = new Is1or2Dimensional[StanArray[T]]
}

@implicitNotFound("toMatrix not supported for type ${T}")
protected sealed class ToMatrixAllowed[T <: StanType] extends TypeCheck

protected object ToMatrixAllowed {
  implicit val isVector = new ToMatrixAllowed[StanVector]
  implicit val isRowVector = new ToMatrixAllowed[StanRowVector]
  implicit val isMatrix = new ToMatrixAllowed[StanMatrix]
  implicit val isIntArrayArray = new ToMatrixAllowed[StanArray[StanArray[StanInt]]]
  implicit val isRealArrayArray = new ToMatrixAllowed[StanArray[StanArray[StanReal]]]
}

@implicitNotFound("toVector not supported for type ${T}")
protected sealed class ToVectorAllowed[T <: StanType] extends TypeCheck

protected object ToVectorAllowed {
  implicit val isVector = new ToVectorAllowed[StanVector]
  implicit val isRowVector = new ToVectorAllowed[StanRowVector]
  implicit val isMatrix = new ToVectorAllowed[StanMatrix]
  implicit val isIntArray= new ToVectorAllowed[StanArray[StanInt]]
  implicit val isRealArray= new ToVectorAllowed[StanArray[StanReal]]
}

@implicitNotFound("appendCol not allowed for ${R} = appendCol(${X}, ${Y})")
protected sealed class AppendColAllowed[X <: StanType, Y <: StanType, R <: StanType] extends TypeCheck

protected object AppendColAllowed {
  implicit val appendColMM = new AppendColAllowed[StanMatrix, StanMatrix, StanMatrix]
  implicit val appendColMV = new AppendColAllowed[StanMatrix, StanVector, StanMatrix]
  implicit val appendColVM = new AppendColAllowed[StanVector, StanMatrix, StanMatrix]
  implicit val appendColVV = new AppendColAllowed[StanVector, StanVector, StanMatrix]
  implicit val appendColRR = new AppendColAllowed[StanRowVector, StanRowVector, StanRowVector]
  implicit val appendColDR = new AppendColAllowed[StanReal, StanRowVector, StanRowVector]
  implicit val appendColRD = new AppendColAllowed[StanRowVector, StanReal, StanRowVector]
}

@implicitNotFound("appendRow not allowed for ${R} = appendCol(${X}, ${Y})")
protected sealed class AppendRowAllowed[X <: StanType, Y <: StanType, R <: StanType] extends TypeCheck

protected object AppendRowAllowed {
  implicit val appendRowMM = new AppendRowAllowed[StanMatrix, StanMatrix, StanMatrix]
  implicit val appendRowMR = new AppendRowAllowed[StanMatrix, StanRowVector, StanMatrix]
  implicit val appendRowRM = new AppendRowAllowed[StanRowVector, StanMatrix, StanMatrix]
  implicit val appendRowRR = new AppendRowAllowed[StanRowVector, StanRowVector, StanMatrix]
  implicit def appendRowVV = new AppendRowAllowed[StanVector, StanVector, StanVector]
  implicit val appendRowDV = new AppendRowAllowed[StanReal, StanVector, StanVector]
  implicit val appendRowVD = new AppendRowAllowed[StanVector, StanReal, StanVector]
}

@implicitNotFound("invalid vectorization: ${T}")
protected sealed class Vectorized1[T <: StanType] extends TypeCheck

protected object Vectorized1 {
  implicit def validVectorization1[T <: StanType: Is0or1Dimensional] = new Vectorized1[T]
}

@implicitNotFound("invalid vectorization")
protected sealed class Vectorized2[A <: StanType, B <: StanType]

protected object Vectorized2 {
  implicit def validVectorization2[
    A <: StanType: Is0or1Dimensional,
    B <: StanType: Is0or1Dimensional
  ] = new Vectorized2[A, B]
}

@implicitNotFound("invalid vectorization")
protected sealed class Vectorized3[A <: StanType, B <: StanType, C <: StanType]

protected object Vectorized3 {
  implicit def validVectorization3[
    A <: StanType: Is0or1Dimensional,
    B <: StanType: Is0or1Dimensional,
    C <: StanType: Is0or1Dimensional
  ] = new Vectorized3[A, B, C]
}

@implicitNotFound("invalid vectorization")
protected sealed class Vectorized4[A <: StanType, B <: StanType, C <: StanType, D <: StanType]

protected object Vectorized4 {
  implicit def validVectorizationScalar[
    A <: StanType: Is0or1Dimensional,
    B <: StanType: Is0or1Dimensional,
    C <: StanType: Is0or1Dimensional,
    D <: StanType: Is0or1Dimensional
  ] = new Vectorized4[A, B, C, D]
}

@implicitNotFound("index not allowed")
protected sealed class IndexAllowed[T <: StanType, I <: StanType, N <: StanType]

protected object IndexAllowed {
  implicit def dereference[T <: StanType, I <: StanInt] = new IndexAllowed[T, I, T#NEXT_TYPE]
  implicit def combine1[T <: StanType, I <: StanArray[StanInt]] = new IndexAllowed[T, I, T]
}
