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

import com.cibo.scalastan.ast.{StanLiteral, StanCall, StanValue, StanValueStatement}

import scala.collection.mutable.ArrayBuffer

protected trait StanFunctions {

  // Reject (5.10).
  def reject(args: StanValue[_]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall(StanVoid(), "reject", args: _*)))
  }

  // Print (38.1).
  def print(args: StanValue[_]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall(StanVoid(), "print", args: _*)))
  }

  // Integer functions (39.2).
  def int_step[T <: StanScalarType[T]](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(),"int_step", x)

  // Bound functions (39.3).
  def min(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "min", x, y)
  def max(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "max", x, y)

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
  def step[T <: StanScalarType[T]](x: StanValue[T]): StanValue[StanReal] = StanCall(StanReal(), "step", x)
  def is_inf[T <: StanScalarType[T]](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "is_inf", x)
  def is_nan[T <: StanScalarType[T]](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "is_nan", x)
  def fabs[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "fabs", x)
  def abs(x: StanValue[StanInt]): StanValue[StanInt] = StanCall(x.returnType, "abs", x)
  def fdim[A <: StanScalarType[A], B <: StanScalarType[B]](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fdim", x, y)
  def fmin[A <: StanScalarType[A], B <: StanScalarType[B]](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmin", x, y)
  def fmax[A <: StanScalarType[A], B <: StanScalarType[B]](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmax", x, y)
  def fmod[A <: StanScalarType[A], B <: StanScalarType[B]](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "fmod", x, y)
  def floor[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "floor", x)
  def ceil[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "ceil", x)
  def round[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "round", x)
  def trunc[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "trunc", x)

  // Power and Logarithm functions (40.8).
  def sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "sqrt", x)
  def cbrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "cbrt", x)
  def square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "square", x)
  def exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "exp", x)
  def exp2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "exp2", x)
  def log[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log", x)
  def log2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log2", x)
  def log10[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log10", x)
  def pow[A <: StanScalarType[A], B <: StanScalarType[B]](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "pow", x, y)
  def inv[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv", x)
  def inv_sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_sqrt", x)
  def inv_square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_square", x)

  // Trigonometric functions (40.9).
  def hypot[T <: StanScalarType[T]](x: StanValue[T], y: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "hypot", x, y)
  def cos[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "cos", x)
  def sin[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "sin", x)
  def tan[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "tan", x)
  def acos[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "acos", x)
  def asin[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "asin", x)
  def atan[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "atan", x)
  def atan2[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "atan2", x, y)

  // Hyperbolic Trigonometric functions (40.10).
  def cosh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "cosh", x)
  def sinh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "sinh", x)
  def tanh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "tanh", x)
  def acosh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "acosh", x)
  def asinh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "asinh", x)
  def atanh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "atanh", x)

  // Link functions (40.11).
  def logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "logit", x)
  def inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_logit", x)
  def inv_cloglog[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_cloglog", x)

  // Probability related functions (40.12).
  def erf[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "erf", x)
  def erfc[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType.realType, "erfc", x)
  def phi[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "Phi", x)
  def inv_Phi[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "inv_Phi", x)
  def Phi_approx[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "Phi_approx", x)
  def binary_log_loss[T <: StanScalarType[T]](x: StanValue[StanInt], y: StanValue[T]): StanValue[StanReal] =
    StanCall(x.returnType, "binary_log_loss", x, y)
  def owens_t[H <: StanScalarType[H], A <: StanScalarType[A]](
    h: StanValue[H], a: StanValue[A]
  ): StanValue[StanReal] = StanCall(StanReal(), "owens_t", h, a)

  // Combinatorial functions (40.13).
  def inc_beta[A <: StanScalarType[A], B <: StanScalarType[B], X <: StanScalarType[X]](
    alpha: StanValue[A], beta: StanValue[B], x: StanValue[X]
  ): StanValue[StanReal] = StanCall(StanReal(), "inc_beta", alpha, beta, x)
  def lbeta[A <: StanScalarType[A], B <: StanScalarType[B]](
    alpha: StanValue[A], beta: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "lbeta", alpha, beta)
  def tgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "tgamma", x)
  def lgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "lgamma", x)
  def digamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "digamma", x)
  def trigamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "trigamma", x)
  def lmgamma[T <: StanScalarType[T]](n: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(x.returnType, "lmgamma", n, x)
  def gamma_p[A <: StanScalarType[A], Z <: StanScalarType[Z]](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "gamma_p", a, z)
  def gamma_q[A <: StanScalarType[A], Z <: StanScalarType[Z]](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "gamma_q", a, z)
  def binomial_coefficient_log[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "binomial_coefficient_log", x, y)
  def choose(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall(StanInt(), "choose", x, y)
  def bessel_first_kind[T <: StanScalarType[T]](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "bessel_first_kind", v, x)
  def bessel_second_kind[T <: StanScalarType[T]](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "bessel_second_kind", v, x)
  def modified_bessel_first_kind[T <: StanScalarType[T]](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "modified_bessel_first_kind", v, z)
  def modified_bessel_second_kind[T <: StanScalarType[T]](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "modified_bessel_second_kind", v, z)
  def falling_factorial[X <: StanScalarType[X], N <: StanScalarType[N]](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "falling_factorial", x, n)
  def lchoose[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "lchoose", x, y)
  def log_falling_factorial[T <: StanScalarType[T]](x: StanValue[T], n: StanValue[T]): StanValue[StanReal] =
    StanCall(StanReal(), "log_falling_factorial", x, n)
  def rising_factorial[X <: StanScalarType[X], N <: StanScalarType[N]](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "rising_factorial", x, n)
  def log_rising_factorial[X <: StanScalarType[X], N <: StanScalarType[N]](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_rising_factorial", x, n)

  // Composed functions (40.14).
  def expm1[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "expm1", x)
  def fma[X <: StanScalarType[X], Y <: StanScalarType[Y], Z <: StanScalarType[Z]](
    x: StanValue[X], y: StanValue[Y], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall(StanReal(), "fma", x, y, z)
  def lmultiply[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "lmultiply", x, y)
  def log1p[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1p", x)
  def log1m[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m", x)
  def log1p_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1p_exp", x)
  def log1m_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m_exp", x)
  def log_diff_exp[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_diff_exp", x, y)
  def log_mix[T <: StanScalarType[T], A <: StanScalarType[A], B <: StanScalarType[B]](
    theta: StanValue[T], lp1: StanValue[A], lp2: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_mix", theta, lp1, lp2)
  def log_sum_exp[X <: StanScalarType[X], Y <: StanScalarType[Y]](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall(StanReal(), "log_sum_exp", x, y)
  def log_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log_inv_logit", x)
  def log1m_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall(x.returnType, "log1m_inv_logit", x)

  // Array reductions (41.1).
  def min[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "min", x)
  def max[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "max", x)
  def sum[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "sum", x)
  def prod[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "prod", x)
  def log_sum_exp[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "log_sum_exp", x)
  def mean[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "mean", x)
  def variance[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] =
    StanCall(x.returnType.element, "variance", x)
  def sd[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall(x.returnType.element, "sd", x)
  def distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall(StanReal(), "distance", x, y)
  def squared_distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall(StanReal(), "squared_distance", x, y)

  // Array size and dimension function (41.2).
  def dims[T <: StanType](x: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(x.returnType.emitDims.size, StanInt()), "dims", x)
  def size[T <: StanArray[_]](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "size", x)

  // Array broadcasting (41.3).
  def rep_array[T <: StanType](x: StanValue[T], n: StanValue[StanInt]): StanValue[StanArray[T]]
    = StanCall(StanArray(n, x.returnType), "rep_array", x, n)
  def rep_array[T <: StanType](
    x: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[T]]] = StanCall(StanArray(m, StanArray(n, x.returnType)), "rep_array", x, m, n)
  def rep_array[T <: StanType](
    x: StanValue[T],
    k: StanValue[StanInt],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[StanArray[T]]]] =
    StanCall(StanArray(k, StanArray(m, StanArray(n, x.returnType))), "rep_array", x, k, m, n)

  def rep_vector(x: StanValue[StanReal], m: StanValue[StanInt]): StanValue[StanVector] =
    StanCall(StanVector(m), "rep_vector", x, m)
  def rep_row_vector(x: StanValue[StanReal], n: StanValue[StanInt]): StanValue[StanRowVector] =
    StanCall(StanRowVector(n), "rep_row_vector", x, n)
  def rep_matrix(x: StanValue[StanReal], m: StanValue[StanInt], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall(StanMatrix(m, n), "rep_matrix", x, m, n)
  def rep_matrix[T <: StanVectorLike](v: StanValue[T], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall(StanMatrix(n, 0), "rep_matrix", v, n)

  // Sorting functions (41.4, 42.14).
  def sort_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] =
    StanCall(v.returnType, "sort_asc", v)
  def sort_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] =
    StanCall(v.returnType, "sort_desc", v)
  def sort_indices_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(v.returnType, "sort_indices_asc", v)
  def sort_indices_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall(v.returnType, "sort_indices_desc", v)
  def rank[T <: StanScalarType[T]: IsVectorLikeOrArray](v: StanValue[T], s: StanValue[StanInt]): StanValue[StanInt] =
    StanCall(StanInt(), "rank", v, s)

  // Integer-valued matrix size functions (42.1).
  def numElements[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "num_elements", x)
  def rows[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "rows", x)
  def cols[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall(StanInt(), "cols", x)

  // Dot products and specialized products (42.5).
  def dot_product[A <: StanVectorLike, B <: StanVectorLike](
    x: StanValue[A],
    y: StanValue[B]
  ): StanValue[StanReal] = StanCall(StanReal(), "dot_product", x, y)
  def columns_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanRowVector] = StanCall(StanRowVector(0), "columns_dot_product", x, y)
  def rows_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanVector] = StanCall(StanVector(0), "rows_dot_product", x, y)
  def dot_self[T <: StanVectorLike](x: StanValue[T]): StanValue[StanReal] = StanCall(StanReal(), "dot_self", x)
  def columns_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanRowVector] = StanCall(StanRowVector(0), "columns_dot_self", x)
  def rows_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanVector] = StanCall(StanVector(0), "rows_dot_self", x)
  def tcrossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(x.returnType, "tcrossprod", x)
  def crossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(x.returnType, "crossprod", x)
  def quad_form[B <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[B]
  ): StanValue[R] = StanCall(if (b.returnType.isInstanceOf[StanMatrix]) b.returnType else StanReal(), "quad_form", a, b)
  def quad_form_diag[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "quad_form_diag", m, v)
  def quad_form_sym[T <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[R] = StanCall(if (b.returnType.isInstanceOf[StanMatrix]) b.returnType else StanReal(), "quad_form_sym", a, b)
  def trace_quad_form(
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall(StanReal(), "trace_quad_form", a, b)
  def trace_gen_quad_form(
    d: StanValue[StanMatrix],
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall(StanReal(), "trace_gen_quad_form", d, a, b)
  def multiply_lower_tri_self_transpose(
    x: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall(x.returnType, "multiply_lower_tri_self_transpose", x)
  def diag_pre_multiply[T <: StanVectorLike](
    v: StanValue[T],
    m: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "diag_pre_multiply", v, m)
  def diag_post_multiply[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall(m.returnType, "diag_post_multiply", m, v)

  // Diagonal Matrix Functions (42.8).
  def diagonal(x: StanValue[StanMatrix]): StanValue[StanVector] = StanCall(StanVector(0), "diagonal", x)
  def diag_matrix(x: StanValue[StanVector]): StanValue[StanMatrix] = StanCall(StanVector(0), "diag_matrix", x)

  // Slicing and blocking functions (42.9).
  def col(x: StanValue[StanMatrix], n: StanValue[StanInt]): StanValue[StanVector] =
    StanCall(StanVector(0), "col", x, n)
  def row(x: StanValue[StanMatrix], m: StanValue[StanInt]): StanValue[StanRowVector] =
    StanCall(StanVector(0), "row", x, m)
  def block(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(nRows, nCols), "block", x, i, j, nRows, nCols)
  def sub_col(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt]
  ): StanValue[StanVector] = StanCall(StanVector(nRows), "sub_col", x, i, j, nRows)
  def sub_row(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanRowVector] = StanCall(StanRowVector(nCols), "sub_row", x, i, j, nCols)
  def head[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "head", v, n)
  def tail[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "tail", v, n)
  def segment[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    i: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall(v.returnType, "segment", v, i, n)  // TODO: update bounds on type

  // Matrix concatenation (42.10).
  def append_col[X <: StanType, Y <: StanType, R <: StanType](
    x: StanValue[X],
    y: StanValue[Y]
  )(
    implicit ev: AppendColAllowed[X, Y, R]
  ): StanValue[R] = StanCall("append_col", x, y)
  def append_row[X <: StanType, Y <: StanType, R <: StanType](
    x: StanValue[X],
    y: StanValue[Y]
  )(
    implicit ev: AppendRowAllowed[X, Y, R]
  ): StanValue[R] = StanCall("append_row", x, y)

  // Special matrix functions (42.11).
  def softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall(x.returnType, "softmax", x)
  def log_softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall(x.returnType, "log_softmax", x)
  def cumulative_sum[T <: StanType: IsVectorLikeOrArray: ContinuousType](
    x: StanValue[T]
  ): StanValue[T] = StanCall(x.returnType, "cumulative_sum", x)

  // Covariance functions (42.12).
  def cov_exp_quad[T <: StanType: IsVectorLikeOrArray: ContinuousType, A <: StanScalarType[A], R <: StanScalarType[R]](
    x: StanValue[T],
    alpha: StanValue[A],
    rho: StanValue[R]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(0, 0), "cov_exp_quad", x, alpha, rho)

  // Linear Algebra Functions and Solvers (42.13).
  def mdivide_left_tri_low[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall(b.returnType, "mdivide_left_tri_low", a, b)
  def mdivide_right_tri_low[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[T] = StanCall(b.returnType, "mdivide_right_tri_low", a, b)
  def mdivide_left_spd[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall(b.returnType, "mdivide_left_spd", a, b)
  def mdivide_right_spd[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[T] = StanCall(a.returnType, "mdivide_right_spd", a, b)
  def matrix_exp(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "matrix_exp", a)
  def trace(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "trace", a)
  def determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "determinant", a)
  def log_determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall(StanReal(), "log_determinant", a)
  def inverse(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "inverse", a)
  def inverse_spd(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "inverse_spd", a)
  def eigenvalues_sym(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall(StanVector(0), "eigenvalues_sym", a)
  def eigenvectors_sym(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "eigenvectors_sym", a)
  def qr_Q(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "qr_Q", a)
  def qr_R(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "qr_R", a)
  def cholesky_decompose(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall(a.returnType, "cholesky_decompose", a)
  def singular_values(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall(a.returnType, "singular_values", a)

  // Sparse Matrix Operatiosn (43).
  def csr_extract_w(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall(StanVector(0), "csr_extract_w", a)
  def csr_extract_v(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(0, StanInt()), "csr_extract_v", a)
  def csr_extract_u(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] =
    StanCall(StanArray(0, StanInt()), "csr_extract_u", a)
  def csr_to_dense_matrix(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "csr_to_dense_matrix", m, n, w, v, u)
  def csr_matrix_times_vector(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]],
    b: StanValue[StanVector]
  ): StanValue[StanVector] = StanCall(StanMatrix(m, n), "csr_matrix_times_vector", m, n, w, v, u, b)

  // Mixed operations (44).
  def to_matrix[T <: StanType: ToMatrixAllowed](v: StanValue[T]): StanValue[StanMatrix] =
    StanCall(StanMatrix(0, 0), "to_matrix", v)
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "to_matrix", v, m, n)
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    colMajor: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall(StanMatrix(m, n), "to_matrix", v, m, n, colMajor)
  def to_vector[T <: StanType: ToVectorAllowed](
    v: StanValue[T]
  ): StanValue[StanVector] = StanCall(StanVector(0), "to_vector", v)
  def to_row_vector[T <: StanType: ToVectorAllowed](
    v: StanValue[T]
  ): StanValue[StanRowVector] = StanCall(StanRowVector(0), "to_row_vector", v)
  def to_array_2d(m: StanValue[StanMatrix]): StanValue[StanArray[StanArray[StanReal]]] =
    StanCall(StanArray(0, StanArray(0, StanReal())), "to_array_2d", m)
  def to_array_1d[T <: StanCompoundType, R <: StanScalarType[R]](
    v: StanValue[T]
  ): StanValue[StanArray[R]] = StanCall(StanArray(0, StanReal()), "to_array_1d", v)

  // ODE Solvers (45).
  def integrate_ode_rk45[T <: StanScalarType[T]](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(StanArray(0, StanArray(0, StanReal())), "integrate_ode_rk45", StanLiteral(ode.result.emit),
      initialState, initialTime, times, theta, xr, xi)
  }

  def integrate_ode_rk45[T <: StanScalarType[T], RT <: StanScalarType[RT], AT <: StanScalarType[AT]](
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
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(
      StanArray(0, StanArray(0, StanReal())),
      "integrate_ode_rk45",
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
  }

  def integrate_ode_bdf[T <: StanScalarType[T]](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(StanArray(0, StanArray(0, StanReal())), "integrate_ode_bdf", StanLiteral(ode.result.emit),
      initialState, initialTime, times, theta, xr, xi)
  }

  def integrate_ode_bdf[T <: StanScalarType[T], RT <: StanScalarType[RT], AT <: StanScalarType[AT]](
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
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(
      StanArray(0, StanArray(0, StanReal())),
      "integrate_ode_bdf",
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
  }

}
