/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.cibo.scalastan.ast._

import scala.collection.mutable.ArrayBuffer

protected trait StanFunctions {

  // Reject (5.10).
  def reject(args: StanValue[_ <: StanType]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall(StanVoid(), "reject", args)))
  }

  // Print (38.1).
  def print(args: StanValue[_ <: StanType]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall(StanVoid(), "print", args)))
  }

  // Integer functions (39.2).
  def int_step[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(),"int_step", Seq(x))

  // Bound functions (39.3).
  def min(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "min", Seq(x, y))
  def max(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "max", Seq(x, y))

  // Mathematical constants (40.2).
  def pi: StanValue[StanReal] = StanCall(StanReal(), "pi")
  def e: StanValue[StanReal] = StanCall(StanReal(), "e")
  def sqrt2: StanValue[StanReal] = StanCall(StanReal(), "sqrt2")
  def log2: StanValue[StanReal] = StanCall(StanReal(), "log2")
  def log10: StanValue[StanReal] = StanCall(StanReal(), "log10")

  // Special values (40.3)
  def not_a_number: StanValue[StanReal] = StanCall(StanReal(), "not_a_number")
  def positive_infinity: StanValue[StanReal] = StanCall(StanReal(), "positive_infinity")
  def negative_infinity: StanValue[StanReal] = StanCall(StanReal(), "negative_infinity")
  def machine_precision: StanValue[StanReal] = StanCall(StanReal(), "machine_precision")

  // Step-like functions (40.7)
  def step[T <: StanScalarType](x: StanValue[T]): StanValue[StanReal] = StanCall(StanReal(), "step", Seq(x))
  def is_inf[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "is_inf", Seq(x))
  def is_nan[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "is_nan", Seq(x))
  def fabs[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "fabs", Seq(x))
  def abs(x: StanValue[StanInt]): StanValue[StanInt] = StanCall(x.returnType, "abs", Seq(x))
  def fdim[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fdim", Seq(x, y))
  def fmin[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmin", Seq(x, y))
  def fmax[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmax", Seq(x, y))
  def fmod[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmod", Seq(x, y))
  def floor[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "floor", Seq(x))
  def ceil[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "ceil", Seq(x))
  def round[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "round", Seq(x))
  def trunc[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "trunc", Seq(x))

  // Power and Logarithm functions (40.8).
  def sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "sqrt", Seq(x))
  def cbrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "cbrt", Seq(x))
  def square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "square", Seq(x))
  def exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "exp", Seq(x))
  def exp2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "exp2", Seq(x))
  def log[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log", Seq(x))
  def log2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log2", Seq(x))
  def log10[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log10", Seq(x))
  def pow[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "pow", Seq(x, y))
  def inv[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv", Seq(x))
  def inv_sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_sqrt", Seq(x))
  def inv_square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_square", Seq(x))

  // Trigonometric functions (40.9).
  def hypot[T <: StanScalarType](x: StanValue[T], y: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "hypot", Seq(x, y))
  def cos[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "cos", Seq(x))
  def sin[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "sin", Seq(x))
  def tan[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "tan", Seq(x))
  def acos[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "acos", Seq(x))
  def asin[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "asin", Seq(x))
  def atan[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "atan", Seq(x))
  def atan2[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "atan2", Seq(x, y))

  // Hyperbolic Trigonometric functions (40.10).
  def cosh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "cosh", Seq(x))
  def sinh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "sinh", Seq(x))
  def tanh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "tanh", Seq(x))
  def acosh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "acosh", Seq(x))
  def asinh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "asinh", Seq(x))
  def atanh[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "atanh", Seq(x))

  // Link functions (40.11).
  def logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "logit", Seq(x))
  def inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_logit", Seq(x))
  def inv_cloglog[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_cloglog", Seq(x))

  // Probability related functions (40.12).
  def erf[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "erf", Seq(x))
  def erfc[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "erfc", Seq(x))
  def phi[T <: StanType](x: StanValue[T]): StanValue[T#REAL_TYPE] = StanCall(x.returnType.realType, "Phi", Seq(x))
  def inv_Phi[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_Phi", Seq(x))
  def Phi_approx[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "Phi_approx", Seq(x))
  def binary_log_loss[T <: StanScalarType](x: StanValue[StanInt], y: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "binary_log_loss", Seq(x, y))
  def owens_t[H <: StanScalarType, A <: StanScalarType](
    h: StanValue[H], a: StanValue[A]
  ): StanValue[StanReal] = StanCall(StanReal(), "owens_t", Seq(h, a))

  // Combinatorial functions (40.13).
  def inc_beta[A <: StanScalarType, B <: StanScalarType, X <: StanScalarType](
    alpha: StanValue[A], beta: StanValue[B], x: StanValue[X]
  ): StanValue[StanReal] = StanCall(StanReal(), "inc_beta", Seq(alpha, beta, x))
  def lbeta[A <: StanScalarType, B <: StanScalarType](
    alpha: StanValue[A], beta: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "lbeta", Seq(alpha, beta))
  def tgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "tgamma", Seq(x))
  def lgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "lgamma", Seq(x))
  def digamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "digamma", Seq(x))
  def trigamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "trigamma", Seq(x))
  def lmgamma[T <: StanScalarType](n: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "lmgamma", Seq(n, x))
  def gamma_p[A <: StanScalarType, Z <: StanScalarType](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "gamma_p", Seq(a, z))
  def gamma_q[A <: StanScalarType, Z <: StanScalarType](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "gamma_q", Seq(a, z))
  def binomial_coefficient_log[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "binomial_coefficient_log", Seq(x, y))
  def choose(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "choose", Seq(x, y))
  def bessel_first_kind[T <: StanScalarType](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "bessel_first_kind", Seq(v, x))
  def bessel_second_kind[T <: StanScalarType](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "bessel_second_kind", Seq(v, x))
  def modified_bessel_first_kind[T <: StanScalarType](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "modified_bessel_first_kind", Seq(v, z))
  def modified_bessel_second_kind[T <: StanScalarType](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "modified_bessel_second_kind", Seq(v, z))
  def falling_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "falling_factorial", Seq(x, n))
  def lchoose[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "lchoose", Seq(x, y))
  def log_falling_factorial[T <: StanScalarType](x: StanValue[T], n: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "log_falling_factorial", Seq(x, n))
  def rising_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "rising_factorial", Seq(x, n))
  def log_rising_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_rising_factorial", Seq(x, n))

  // Composed functions (40.14).
  def expm1[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "expm1", Seq(x))
  def fma[X <: StanScalarType, Y <: StanScalarType, Z <: StanScalarType](
    x: StanValue[X], y: StanValue[Y], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "fma", Seq(x, y, z))
  def lmultiply[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "lmultiply", Seq(x, y))
  def log1p[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1p", Seq(x))
  def log1m[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m", Seq(x))
  def log1p_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1p_exp", Seq(x))
  def log1m_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m_exp", Seq(x))
  def log_diff_exp[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_diff_exp", Seq(x, y))
  def log_mix[T <: StanScalarType, A <: StanScalarType, B <: StanScalarType](
    theta: StanValue[T], lp1: StanValue[A], lp2: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_mix", Seq(theta, lp1, lp2))
  def log_sum_exp[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_sum_exp", Seq(x, y))
  def log_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log_inv_logit", Seq(x))
  def log1m_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m_inv_logit", Seq(x))

  // Array reductions (41.1).
  def min[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "min", Seq(x))
  def max[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "max", Seq(x))
  def sum[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "sum", Seq(x))
  def prod[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "prod", Seq(x))
  def log_sum_exp[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "log_sum_exp", Seq(x))
  def mean[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "mean", Seq(x))
  def variance[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "variance", Seq(x))
  def sd[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "sd", Seq(x))
  def distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall(StanReal(), "distance", Seq(x, y))
  def squared_distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall(StanReal(), "squared_distance", Seq(x, y))

  // Array size and dimension function (41.2).
  def dims[T <: StanType](x: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(x.returnType.emitDims.size, StanInt()), "dims", Seq(x))
  def size[T <: StanArray[_]](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "size", Seq(x))

  // Array broadcasting (41.3).
  def rep_array[T <: StanType](x: StanValue[T], n: StanValue[StanInt]): StanValue[StanArray[T]]
    = StanCall(StanArray(n, x.returnType), "rep_array", Seq(x, n))
  def rep_array[T <: StanType](
    x: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[T]]] = StanCall(StanArray(m, StanArray(n, x.returnType)), "rep_array", Seq(x, m, n))
  def rep_array[T <: StanType](
    x: StanValue[T],
    k: StanValue[StanInt],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[StanArray[T]]]] =
    StanCall(StanArray(k, StanArray(m, StanArray(n, x.returnType))), "rep_array", Seq(x, k, m, n))

  def rep_vector(x: StanValue[StanReal], m: StanValue[StanInt]): StanValue[StanVector] =
    StanCall(StanVector(m), "rep_vector", Seq(x, m))
  def rep_row_vector(x: StanValue[StanReal], n: StanValue[StanInt]): StanValue[StanRowVector] =
    StanCall(StanRowVector(n), "rep_row_vector", Seq(x, n))
  def rep_matrix(x: StanValue[StanReal], m: StanValue[StanInt], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall(StanMatrix(m, n), "rep_matrix", Seq(x, m, n))
  def rep_matrix[T <: StanVectorLike](v: StanValue[T], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall(StanMatrix(n, 0), "rep_matrix", Seq(v, n))

  // Sorting functions (41.4, 42.14).
  def sort_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] =
    StanCall(v.returnType, "sort_asc", Seq(v))
  def sort_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] =
    StanCall(v.returnType, "sort_desc", Seq(v))
  def sort_indices_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(implicitly[IsVectorLikeOrArray[T]].dim(v.returnType), StanInt()), "sort_indices_asc", Seq(v))
  def sort_indices_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(implicitly[IsVectorLikeOrArray[T]].dim(v.returnType), StanInt()), "sort_indices_desc", Seq(v))
  def rank[T <: StanScalarType: IsVectorLikeOrArray](v: StanValue[T], s: StanValue[StanInt]): StanValue[StanInt] =
    StanCall(StanInt(), "rank", Seq(v, s))

  // Integer-valued matrix size functions (42.1).
  def numElements[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "num_elements", Seq(x))
  def rows[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "rows", Seq(x))
  def cols[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "cols", Seq(x))

  // Dot products and specialized products (42.5).
  def dot_product[A <: StanVectorLike, B <: StanVectorLike](
    x: StanValue[A],
    y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "dot_product", Seq(x, y))
  def columns_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanRowVector] = {
    val dim: StanValue[StanInt] = x.returnType match {
      case r: StanRowVector => r.dim
      case m: StanMatrix    => m.rows
      case _                => 1
    }
    StanCall(StanRowVector(dim), "columns_dot_product", Seq(x, y))
  }
  def rows_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanVector] = {
    val dim: StanValue[StanInt] = x.returnType match {
      case v: StanVector => v.dim
      case m: StanMatrix => m.cols
      case _             => 1
    }
    StanCall(StanVector(dim), "rows_dot_product", Seq(x, y))
  }
  def dot_self[T <: StanVectorLike](x: StanValue[T]): StanValue[StanReal] = StanCall(StanReal(),"dot_self", Seq(x))
  def columns_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanRowVector] = {
    val dim: StanValue[StanInt] = x.returnType match {
      case r: StanRowVector => r.dim
      case m: StanMatrix    => m.rows
      case _                => 1
    }
    StanCall(StanRowVector(dim), "columns_dot_self", Seq(x))
  }
  def rows_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanVector] = {
    val dim: StanValue[StanInt] = x.returnType match {
      case v: StanVector => v.dim
      case m: StanMatrix => m.cols
      case _             => 1
    }
    StanCall(StanVector(dim), "rows_dot_self", Seq(x))
  }
  def tcrossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(x.returnType, "tcrossprod", Seq(x))
  def crossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(x.returnType, "crossprod", Seq(x))
  def quad_form[B <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[B]
  )(implicit ev: QuadFormAllowed[B, R]): StanValue[R] = StanCall(ev.newType(b.returnType), "quad_form", Seq(a, b))
  def quad_form_diag[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "quad_form_diag", Seq(m, v))
  def quad_form_sym[T <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  )(implicit ev: QuadFormAllowed[T, R]): StanValue[R] = StanCall(ev.newType(b.returnType), "quad_form_sym", Seq(a, b))
  def trace_quad_form(
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall(StanReal(), "trace_quad_form", Seq(a, b))
  def trace_gen_quad_form(
    d: StanValue[StanMatrix],
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall(StanReal(), "trace_gen_quad_form", Seq(d, a, b))
  def multiply_lower_tri_self_transpose(
    x: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall(x.returnType, "multiply_lower_tri_self_transpose", Seq(x))
  def diag_pre_multiply[T <: StanVectorLike](
    v: StanValue[T],
    m: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "diag_pre_multiply", Seq(v, m))
  def diag_post_multiply[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "diag_post_multiply", Seq(m, v))

  // Diagonal Matrix Functions (42.8).
  def diagonal(x: StanValue[StanMatrix]): StanValue[StanVector] =
    StanCall(StanVector(x.returnType.rows), "diagonal", Seq(x))
  def diag_matrix(x: StanValue[StanVector]): StanValue[StanMatrix] =
    StanCall(StanMatrix(x.returnType.dim, x.returnType.dim), "diag_matrix", Seq(x))

  // Slicing and blocking functions (42.9).
  def col(x: StanValue[StanMatrix], n: StanValue[StanInt]): StanValue[StanVector] =
    StanCall(StanVector(x.returnType.rows), "col", Seq(x, n))
  def row(x: StanValue[StanMatrix], m: StanValue[StanInt]): StanValue[StanRowVector] =
    StanCall(StanRowVector(x.returnType.cols), "row", Seq(x, m))
  def block(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(nRows, nCols), "block", Seq(x, i, j, nRows, nCols))
  def sub_col(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt]
  ): StanValue[StanVector] = StanCall(StanVector(nRows), "sub_col", Seq(x, i, j, nRows))
  def sub_row(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanRowVector] = StanCall(StanRowVector(nCols), "sub_row", Seq(x, i, j, nCols))
  def head[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "head", Seq(v, n))
  def tail[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "tail", Seq(v, n))
  def segment[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    i: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "segment", Seq(v, i, n))  // TODO: update bounds on returnType

  // Matrix concatenation (42.10).
  def append_col[X <: StanType, Y <: StanType, R <: StanType](
    x: StanValue[X],
    y: StanValue[Y]
  )(
    implicit ev: AppendColAllowed[X, Y, R]
  ): StanValue[R] = StanCall(ev.newType(x.returnType, y.returnType), "append_col", Seq(x, y))
  def append_row[X <: StanType, Y <: StanType, R <: StanType](
    x: StanValue[X],
    y: StanValue[Y]
  )(
    implicit ev: AppendRowAllowed[X, Y, R]
  ): StanValue[R] = StanCall(ev.newType(x.returnType, y.returnType), "append_row", Seq(x, y))

  // Special matrix functions (42.11).
  def softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall(x.returnType, "softmax", Seq(x))
  def log_softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall(x.returnType, "log_softmax", Seq(x))
  def cumulative_sum[T <: StanType: IsVectorLikeOrArray: ContinuousType](
    x: StanValue[T]
  ): StanValue[T] = StanCall(x.returnType, "cumulative_sum", Seq(x))

  // Covariance functions (42.12).
  def cov_exp_quad[T <: StanType: IsVectorLikeOrArray: ContinuousType, A <: StanScalarType, R <: StanScalarType](
    x: StanValue[T],
    alpha: StanValue[A],
    rho: StanValue[R]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(0, 0), "cov_exp_quad", Seq(x, alpha, rho))

  // Linear Algebra Functions and Solvers (42.13).
  def mdivide_left_tri_low[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall(b.returnType, "mdivide_left_tri_low", Seq(a, b))
  def mdivide_right_tri_low[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall(b.returnType, "mdivide_right_tri_low", Seq(a, b))
  def mdivide_left_spd[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall(b.returnType, "mdivide_left_spd", Seq(a, b))
  def mdivide_right_spd[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[T] = StanCall(a.returnType, "mdivide_right_spd", Seq(a, b))
  def matrix_exp(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "matrix_exp", Seq(a))
  def trace(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "trace", Seq(a))
  def determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "determinant", Seq(a))
  def log_determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "log_determinant", Seq(a))
  def inverse(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "inverse", Seq(a))
  def inverse_spd(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "inverse_spd", Seq(a))
  def eigenvalues_sym(a: StanValue[StanMatrix]): StanValue[StanVector] =
    StanCall(StanVector(a.returnType.rows), "eigenvalues_sym", Seq(a))
  def eigenvectors_sym(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "eigenvectors_sym", Seq(a))
  def qr_Q(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "qr_Q", Seq(a))
  def qr_R(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "qr_R", Seq(a))
  def cholesky_decompose(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "cholesky_decompose", Seq(a))
  def singular_values(a: StanValue[StanMatrix]): StanValue[StanVector] =
    StanCall(StanVector(a.returnType.rows), "singular_values", Seq(a))

  // Sparse Matrix Operations (43).
  def csr_extract_w(a: StanValue[StanMatrix]): StanValue[StanVector] =
    StanCall(StanVector(a.returnType.rows), "csr_extract_w", Seq(a))
  def csr_extract_v(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(a.returnType.rows, StanInt()), "csr_extract_v", Seq(a))
  def csr_extract_u(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(a.returnType.rows, StanInt()), "csr_extract_u", Seq(a))
  def csr_to_dense_matrix(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "csr_to_dense_matrix", Seq(m, n, w, v, u))
  def csr_matrix_times_vector(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]],
    b: StanValue[StanVector]
  ): StanValue[StanVector] = StanCall(b.returnType, "csr_matrix_times_vector", Seq(m, n, w, v, u, b))

  // Mixed operations (44).
  def to_matrix[T <: StanType: ToMatrixAllowed](v: StanValue[T]): StanValue[StanMatrix] =
    StanCall(StanMatrix(0, 0), "to_matrix", Seq(v))
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "to_matrix", Seq(v, m, n))
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    colMajor: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "to_matrix", Seq(v, m, n, colMajor))
  def to_vector[T <: StanType](
    v: StanValue[T]
  )(implicit ev: ToVectorAllowed[T]): StanValue[StanVector] =
    StanCall(StanVector(ev.dim(v.returnType)), "to_vector", Seq(v))
  def to_row_vector[T <: StanType](
    v: StanValue[T]
  )(implicit ev: ToVectorAllowed[T]): StanValue[StanRowVector] =
    StanCall(StanRowVector(ev.dim(v.returnType)), "to_row_vector", Seq(v))
  def to_array_2d(m: StanValue[StanMatrix]): StanValue[StanArray[StanArray[StanReal]]] =
    StanCall(StanArray(m.returnType.rows, StanArray(m.returnType.cols, StanReal())), "to_array_2d", Seq(m))
  def to_array_1d[T <: StanCompoundType](
    v: StanValue[T]
  ): StanValue[StanArray[v.returnType.ELEMENT_TYPE]] =
    StanCall[StanArray[v.returnType.ELEMENT_TYPE]](StanArray(StanUnknownDim(), v.returnType.element), "to_array_1d", Seq(v))

  // ODE Solvers (45).
  def integrate_ode_rk45[T <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  )(implicit code: CodeBuilder): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.export(code)
    StanCall(
      StanArray(initialState.returnType.dim, StanArray(times.returnType.dim, StanReal())),
      "integrate_ode_rk45",
      Seq(StanLiteral(ode.result.emit), initialState, initialTime, times, theta, xr, xi))
  }

  def integrate_ode_rk45[T <: StanScalarType, RT <: StanScalarType, AT <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]],
    relTol: StanValue[RT],
    absTol: StanValue[AT],
    maxNumSteps: StanValue[StanInt]
  )(implicit code: CodeBuilder): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.export(code)
    StanCall(
      StanArray(initialState.returnType.dim, StanArray(times.returnType.dim, StanReal())),
      "integrate_ode_rk45",
      Seq(
        StanLiteral(ode.result.emit),
        initialState,
        initialTime,
        times,
        theta,
        xr,
        xi,
        relTol,
        absTol,
        maxNumSteps
      )
    )
  }

  def integrate_ode_bdf[T <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  )(implicit code: CodeBuilder): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.export(code)
    StanCall(
      StanArray(initialState.returnType.dim, StanArray(times.returnType.dim, StanReal())),
      "integrate_ode_bdf",
      Seq(StanLiteral(ode.result.emit), initialState, initialTime, times, theta, xr, xi))
  }

  def integrate_ode_bdf[T <: StanScalarType, RT <: StanScalarType, AT <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]],
    relTol: StanValue[RT],
    absTol: StanValue[AT],
    maxNumSteps: StanValue[StanInt]
  )(implicit code: CodeBuilder): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.export(code)
    StanCall(
      StanArray(initialState.returnType.dim, StanArray(times.returnType.dim, StanReal())),
      "integrate_ode_bdf",
      Seq(
        StanLiteral(ode.result.emit),
        initialState,
        initialTime,
        times,
        theta,
        xr,
        xi,
        relTol,
        absTol,
        maxNumSteps
      )
    )
  }

}
