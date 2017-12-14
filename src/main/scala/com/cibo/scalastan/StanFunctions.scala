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

import com.cibo.scalastan.ast.{StanLiteral, StanCall, StanValue, StanValueStatement}

import scala.collection.mutable.ArrayBuffer

protected trait StanFunctions {

  // Reject (5.10).
  def reject(args: StanValue[_]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall[StanVoid]("reject", args: _*)))
  }

  // Print (38.1).
  def print(args: StanValue[_]*)(implicit code: CodeBuilder): Unit = {
    code.append(StanValueStatement(StanCall[StanVoid]("print", args: _*)))
  }

  // Integer functions (39.2).
  def int_step[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall("int_step", x)

  // Bound functions (39.3).
  def min(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall("min", x, y)
  def max(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall("max", x, y)

  // Mathematical constants (40.2).
  def pi: StanValue[StanReal] = StanCall("pi")
  def e: StanValue[StanReal] = StanCall("e")
  def sqrt2: StanValue[StanReal] = StanCall("sqrt2")
  def log2: StanValue[StanReal] = StanCall("log2")
  def log10: StanValue[StanReal] = StanCall("log10")

  // Special values (40.3)
  def not_a_number: StanValue[StanReal] = StanCall("not_a_number")
  def positive_infinity: StanValue[StanReal] = StanCall("positive_infinity")
  def negative_infinity: StanValue[StanReal] = StanCall("negative_infinity")
  def machine_precision: StanValue[StanReal] = StanCall("machine_precision")

  // Step-like functions (40.7)
  def step[T <: StanScalarType](x: StanValue[T]): StanValue[StanReal] = StanCall("step", x)
  def is_inf[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall("is_inf", x)
  def is_nan[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = StanCall("is_nan", x)
  def fabs[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("fabs", x)
  def abs(x: StanValue[StanInt]): StanValue[StanInt] = StanCall("abs", x)
  def fdim[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall("fdim", x, y)
  def fmin[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall("fmin", x, y)
  def fmax[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall("fmax", x, y)
  def fmod[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall("fmod", x, y)
  def floor[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("floor", x)
  def ceil[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("ceil", x)
  def round[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("round", x)
  def trunc[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("trunc", x)

  // Power and Logarithm functions (40.8).
  def sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("sqrt", x)
  def cbrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("cbrt", x)
  def square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("square", x)
  def exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("exp", x)
  def exp2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("exp2", x)
  def log[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log", x)
  def log2[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log2", x)
  def log10[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log10", x)
  def pow[A <: StanScalarType, B <: StanScalarType](
    x: StanValue[A], y: StanValue[B]
  ): StanValue[StanReal] = StanCall("pow", x, y)
  def inv[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv", x)
  def inv_sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv_sqrt", x)
  def inv_square[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv_square", x)

  // Trigonometric functions (40.9).
  def hypot[T <: StanScalarType](x: StanValue[T], y: StanValue[T]): StanValue[StanReal] = StanCall("hypot", x, y)
  def cos[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("cos", x)
  def sin[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("sin", x)
  def tan[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("tan", x)
  def acos[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("acos", x)
  def asin[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("asin", x)
  def atan[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("atan", x)
  def atan2[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("atan2", x, y)

  // Hyperbolic Trigonometric functions (40.10).
  def cosh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("cosh", x)
  def sinh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("sinh", x)
  def tanh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("sinh", x)
  def acosh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("acosh", x)
  def asinh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("asinh", x)
  def atanh[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("atanh", x)

  // Link functions (40.11).
  def logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("logit", x)
  def inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv_logit", x)
  def inv_cloglog[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv_cloglog", x)

  // Probability related functions (40.12).
  def erf[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("erf", x)
  def erfc[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("erfc", x)
  def phi[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("Phi", x)
  def inv_Phi[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("inv_Phi", x)
  def Phi_approx[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("Phi_approx", x)
  def binary_log_loss[T <: StanScalarType](x: StanValue[StanInt], y: StanValue[T]): StanValue[StanReal] =
    StanCall("binary_log_loss", x, y)
  def owens_t[H <: StanScalarType, A <: StanScalarType](
    h: StanValue[H], a: StanValue[A]
  ): StanValue[StanReal] = StanCall("owens_t", h, a)

  // Combinatorial functions (40.13).
  def inc_beta[A <: StanScalarType, B <: StanScalarType, X <: StanScalarType](
    alpha: StanValue[A], beta: StanValue[B], x: StanValue[X]
  ): StanValue[StanReal] = StanCall("inc_beta", alpha, beta, x)
  def lbeta[A <: StanScalarType, B <: StanScalarType](
    alpha: StanValue[A], beta: StanValue[B]
  ): StanValue[StanReal] = StanCall("lbeta", alpha, beta)
  def tgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("tgamma", x)
  def lgamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("lgamma", x)
  def digamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("digamma", x)
  def trigamma[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("trigamma", x)
  def lmgamma[T <: StanScalarType](n: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall("lmgamma", n, x)
  def gamma_p[A <: StanScalarType, Z <: StanScalarType](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall("gamma_p", a, z)
  def gamma_q[A <: StanScalarType, Z <: StanScalarType](
    a: StanValue[A], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall("gamma_q", a, z)
  def binomial_coefficient_log[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("binomial_coefficient_log", x, y)
  def choose(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = StanCall("choose", x, y)
  def bessel_first_kind[T <: StanScalarType](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall("bessel_first_kind", v, x)
  def bessel_second_kind[T <: StanScalarType](v: StanValue[StanInt], x: StanValue[T]): StanValue[StanReal] =
    StanCall("bessel_second_kind", v, x)
  def modified_bessel_first_kind[T <: StanScalarType](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall("modified_bessel_first_kind", v, z)
  def modified_bessel_second_kind[T <: StanScalarType](v: StanValue[StanInt], z: StanValue[T]): StanValue[StanReal] =
    StanCall("modified_bessel_second_kind", v, z)
  def falling_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall("falling_factorial", x, n)
  def lchoose[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("lchoose", x, y)
  def log_falling_factorial[T <: StanScalarType](x: StanValue[T], n: StanValue[T]): StanValue[StanReal] =
    StanCall("log_falling_factorial", x, n)
  def rising_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall("rising_factorial", x, n)
  def log_rising_factorial[X <: StanScalarType, N <: StanScalarType](
    x: StanValue[X], n: StanValue[N]
  ): StanValue[StanReal] = StanCall("log_rising_factorial", x, n)

  // Composed functions (40.14).
  def expm1[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("expm1", x)
  def fma[X <: StanScalarType, Y <: StanScalarType, Z <: StanScalarType](
    x: StanValue[X], y: StanValue[Y], z: StanValue[Z]
  ): StanValue[StanReal] = StanCall("fma", x, y, z)
  def lmultiply[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("lmultiply", x, y)
  def log1p[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log1p", x)
  def log1m[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log1m", x)
  def log1p_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log1p_exp", x)
  def log1m_exp[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log1m_exp", x)
  def log_diff_exp[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("log_diff_exp", x, y)
  def log_mix[T <: StanScalarType, A <: StanScalarType, B <: StanScalarType](
    theta: StanValue[T], lp1: StanValue[A], lp2: StanValue[B]
  ): StanValue[StanReal] = StanCall("log_mix", theta, lp1, lp2)
  def log_sum_exp[X <: StanScalarType, Y <: StanScalarType](
    x: StanValue[X], y: StanValue[Y]
  ): StanValue[StanReal] = StanCall("log_sum_exp", x, y)
  def log_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log_inv_logit", x)
  def log1m_inv_logit[T <: StanType](x: StanValue[T]): StanValue[T] = StanCall("log1m_inv_logit", x)

  // Array reductions (41.1).
  def min[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("min", x)
  def max[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("max", x)
  def sum[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("sum", x)
  def prod[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("prod", x)
  def log_sum_exp[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("log_sum_exp", x)
  def mean[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("mean", x)
  def variance[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("variance", x)
  def sd[T <: StanCompoundType](x: StanValue[T]): StanValue[T#ELEMENT_TYPE] = StanCall("sd", x)
  def distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall("distance", x, y)
  def squared_distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = StanCall("squared_distance", x, y)

  // Array size and dimension function (41.2).
  def dims[T <: StanType](x: StanValue[T]): StanValue[StanArray[StanInt]] = StanCall("dims", x)
  def size[T <: StanArray[_]](x: StanValue[T]): StanValue[StanInt] = StanCall("size", x)

  // Array broadcasting (41.3).
  def rep_array[T <: StanType](x: StanValue[T], n: StanValue[StanInt]): StanValue[StanArray[T]]
    = StanCall("rep_array", x, n)
  def rep_array[T <: StanType](
    x: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[T]]] = StanCall("rep_array", x, m, n)
  def rep_array[T <: StanType](
    x: StanValue[T],
    k: StanValue[StanInt],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[StanArray[T]]]] = StanCall("rep_array", x, k, m, n)

  def rep_vector(x: StanValue[StanReal], m: StanValue[StanInt]): StanValue[StanVector] =
    StanCall("rep_vector", x, m)
  def rep_row_vector(x: StanValue[StanReal], n: StanValue[StanInt]): StanValue[StanRowVector] =
    StanCall("rep_row_vector", x, n)
  def rep_matrix(x: StanValue[StanReal], m: StanValue[StanInt], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall("rep_matrix", x, m, n)
  def rep_matrix[T <: StanVectorLike](v: StanValue[T], n: StanValue[StanInt]): StanValue[StanMatrix] =
    StanCall("rep_matrix", v, n)

  // Sorting functions (41.4, 42.14).
  def sort_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] = StanCall("sort_asc", v)
  def sort_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[T] = StanCall("sort_desc", v)
  def sort_indices_asc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall("sort_indices_asc", v)
  def sort_indices_desc[T <: StanType: IsVectorLikeOrArray](v: StanValue[T]): StanValue[StanArray[StanInt]] =
    StanCall("sort_indices_desc", v)
  def rank[T <: StanScalarType: IsVectorLikeOrArray](v: StanValue[T], s: StanValue[StanInt]): StanValue[StanInt] =
    StanCall("rank", v, s)

  // Integer-valued matrix size functions (42.1).
  def numElements[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall("num_elements", x)
  def rows[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall("rows", x)
  def cols[T <: StanVectorOrMatrix](x: StanValue[T]): StanValue[StanInt] = StanCall("cols", x)

  // Dot products and specialized products (42.5).
  def dot_product[A <: StanVectorLike, B <: StanVectorLike](
    x: StanValue[A],
    y: StanValue[B]
  ): StanValue[StanReal] = StanCall("dot_product", x, y)
  def columns_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanRowVector] = StanCall("columns_dot_product", x, y)
  def rows_dot_product[T <: StanVectorOrMatrix](
    x: StanValue[T],
    y: StanValue[T]
  ): StanValue[StanVector] = StanCall("rows_dot_product", x, y)
  def dotSelf[T <: StanVectorLike](x: StanValue[T]): StanValue[StanReal] = StanCall("dot_self", x)
  def columns_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanRowVector] = StanCall("columns_dot_self", x)
  def rows_dot_self[T <: StanVectorOrMatrix](
    x: StanValue[T]
  ): StanValue[StanVector] = StanCall("rows_dot_self", x)
  def tcrossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("tcrossprod", x)
  def crossprod(x: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("crossprod", x)
  def quad_form[B <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[B]
  ): StanValue[R] = StanCall("quad_form", a, b)
  def quad_form_diag[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall("quad_form_diag", m, v)
  def quad_form_sym[T <: StanVectorOrMatrix, R <: StanType](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[R] = StanCall("quad_form_sym", a, b)
  def trace_quad_form(
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall("trace_quad_form", a, b)
  def trace_gen_quad_form(
    d: StanValue[StanMatrix],
    a: StanValue[StanMatrix],
    b: StanValue[StanMatrix]
  ): StanValue[StanReal] = StanCall("trace_gen_quad_form", d, a, b)
  def multiply_lower_tri_self_transpose(
    x: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall("multiply_lower_tri_self_transpose", x)
  def diag_pre_multiply[T <: StanVectorLike](
    v: StanValue[T],
    m: StanValue[StanMatrix]
  ): StanValue[StanMatrix] = StanCall("diag_pre_multiply", v, m)
  def diag_post_multiply[T <: StanVectorLike](
    m: StanValue[StanMatrix],
    v: StanValue[T]
  ): StanValue[StanMatrix] = StanCall("diag_post_multiply", m, v)

  // Diagonal Matrix Functions (42.8).
  def diagonal(x: StanValue[StanMatrix]): StanValue[StanVector] = StanCall("diagonal", x)
  def diag_matrix(x: StanValue[StanVector]): StanValue[StanMatrix] = StanCall("diag_matrix", x)

  // Slicing and blocking functions (42.9).
  def col(x: StanValue[StanMatrix], n: StanValue[StanInt]): StanValue[StanVector] = StanCall("col", x, n)
  def row(x: StanValue[StanMatrix], m: StanValue[StanInt]): StanValue[StanRowVector] = StanCall("row", x, m)
  def block(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall("block", x, i, j, nRows, nCols)
  def sub_col(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nRows: StanValue[StanInt]
  ): StanValue[StanVector] = StanCall("sub_col", x, i, j, nRows)
  def sub_row(
    x: StanValue[StanMatrix],
    i: StanValue[StanInt],
    j: StanValue[StanInt],
    nCols: StanValue[StanInt]
  ): StanValue[StanRowVector] = StanCall("sub_row", x, i, j, nCols)
  def head[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall("head", v, n)
  def tail[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall("tail", v, n)
  def segment[T <: StanCompoundType: IsVectorLikeOrArray](
    v: StanValue[T],
    i: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[T] = StanCall("segment", v, i, n)

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
  def softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall("softmax", x)
  def log_softmax(x: StanValue[StanVector]): StanValue[StanVector] = StanCall("log_softmax", x)
  def cumulative_sum[T <: StanType: IsVectorLikeOrArray: ContinuousType](
    x: StanValue[T]
  ): StanValue[T] = StanCall("cumulative_sum", x)

  // Covariance functions (42.12).
  def cov_exp_quad[T <: StanType: IsVectorLikeOrArray: ContinuousType, A <: StanScalarType, R <: StanScalarType](
    x: StanValue[T],
    alpha: StanValue[A],
    rho: StanValue[R]
  ): StanValue[StanMatrix] = StanCall("cov_exp_quad", x, alpha, rho)

  // Linear Algebra Functions and Solvers (42.13).
  def mdivide_left_tri_low[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall("mdivide_left_tri_low", a, b)
  def mdivide_right_tri_low[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[T] = StanCall("mdivide_right_tri_low", a, b)
  def mdivide_left_spd[T <: StanCompoundType: IsVectorOrMatrix](
    a: StanValue[StanMatrix],
    b: StanValue[T]
  ): StanValue[T] = StanCall("mdivide_left_spd", a, b)
  def mdivide_right_spd[T <: StanCompoundType: IsRowVectorOrMatrix](
    a: StanValue[T],
    b: StanValue[StanMatrix]
  ): StanValue[T] = StanCall("mdivide_right_spd", a, b)
  def matrix_exp(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("matrix_exp", a)
  def trace(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall("trace", a)
  def determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall("determinant", a)
  def log_determinant(a: StanValue[StanMatrix]): StanValue[StanReal] = StanCall("log_determinant", a)
  def inverse(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("inverse", a)
  def inverse_spd(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("inverse_spd", a)
  def eigenvalues_sym(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall("eigenvalues_sym", a)
  def eigenvectors_sym(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("eigenvectors_sym", a)
  def qr_Q(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("qr_Q", a)
  def qr_R(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("qr_R", a)
  def cholesky_decompose(a: StanValue[StanMatrix]): StanValue[StanMatrix] = StanCall("cholesky_decompose", a)
  def singular_values(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall("singular_values", a)

  // Sparse Matrix Operatiosn (43).
  def csr_extract_w(a: StanValue[StanMatrix]): StanValue[StanVector] = StanCall("csr_extract_w", a)
  def csr_extract_v(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] = StanCall("csr_extract_v", a)
  def csr_extract_u(a: StanValue[StanMatrix]): StanValue[StanArray[StanInt]] = StanCall("csr_extract_u", a)
  def csr_to_dense_matrix(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]]
  ): StanValue[StanMatrix] = StanCall("csr_to_dense_matrix", m, n, w, v, u)
  def csr_matrix_times_vector(
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    w: StanValue[StanVector],
    v: StanValue[StanArray[StanInt]],
    u: StanValue[StanArray[StanInt]],
    b: StanValue[StanVector]
  ): StanValue[StanVector] = StanCall("csr_matrix_times_vector", m, n, w, v, u, b)

  // Mixed operations (44).
  def to_matrix[T <: StanType: ToMatrixAllowed](v: StanValue[T]): StanValue[StanMatrix] =
    StanCall("to_matrix", v)
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall("to_matrix", v, m, n)
  def to_matrix[T <: StanType: ToMatrixAllowed](
    v: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt],
    colMajor: StanValue[StanInt]
  ): StanValue[StanMatrix] = StanCall("to_matrix", v, m, n, colMajor)
  def to_vector[T <: StanType: ToVectorAllowed](
    v: StanValue[T]
  ): StanValue[StanVector] = StanCall("to_vector", v)
  def to_row_vector[T <: StanType: ToVectorAllowed](
    v: StanValue[T]
  ): StanValue[StanRowVector] = StanCall("to_row_vector", v)
  def to_array_2d(m: StanValue[StanMatrix]): StanValue[StanArray[StanArray[StanReal]]] = StanCall("to_array_2d", m)
  def to_array_1d[T <: StanCompoundType, R <: StanScalarType](
    v: StanValue[T]
  ): StanValue[StanArray[R]] = StanCall("to_array_1d", v)

  // ODE Solvers (45).
  def integrate_ode_rk45[T <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall("integrate_ode_rk45", StanLiteral(ode.result.emit), initialState, initialTime, times, theta, xr, xi)
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
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(
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

  def integrate_ode_bdf[T <: StanScalarType](
    ode: ScalaStan#Function[StanArray[StanReal]],
    initialState: StanValue[StanArray[StanReal]],
    initialTime: StanValue[T],
    times: StanValue[StanArray[StanReal]],
    theta: StanValue[StanArray[StanReal]],
    xr: StanValue[StanArray[StanReal]],
    xi: StanValue[StanArray[StanInt]]
  ): StanValue[StanArray[StanReal]] = {
    ode.markUsed()
    StanCall("integrate_ode_bdf", StanLiteral(ode.result.emit), initialState, initialTime, times, theta, xr, xi)
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
  ): StanValue[StanArray[StanArray[StanReal]]] = {
    ode.markUsed()
    StanCall(
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
