package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

protected trait StanBuiltInFunctions {

  // Reject (5.10).
  def reject(args: StanValue[_]*)(implicit code: ArrayBuffer[StanNode]): Unit = {
    code += FunctionNode[StanVoid]("reject", args: _*)
  }

  // Print (38.1).
  def print(args: StanValue[_]*)(implicit code: ArrayBuffer[StanNode]): Unit = {
    code += FunctionNode[StanVoid]("print", args: _*)
  }

  // Integer functions (39.2).
  def intStep[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("int_step", x)

  // Bound functions (39.3).
  def min(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = FunctionNode("min", x, y)
  def max(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = FunctionNode("max", x, y)

  // Mathematical constants (40.2).
  def pi: StanValue[StanReal] = FunctionNode("pi")
  def e: StanValue[StanReal] = FunctionNode("e")
  def sqrt2: StanValue[StanReal] = FunctionNode("sqrt2")
  def log2: StanValue[StanReal] = FunctionNode("log2")
  def log10: StanValue[StanReal] = FunctionNode("log10")

  // Special values (40.3)
  def notANumber: StanValue[StanReal] = FunctionNode("not_a_number")
  def positiveInfinity: StanValue[StanReal] = FunctionNode("positive_infinity")
  def negativeInfinity: StanValue[StanReal] = FunctionNode("negative_infinity")
  def machinePrecision: StanValue[StanReal] = FunctionNode("machine_precision")

  // Step-like functions (40.7)
  def step(x: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("step", x)
  def isInf(x: StanValue[StanReal]): StanValue[StanInt] = FunctionNode("is_inf", x)
  def isNan(x: StanValue[StanReal]): StanValue[StanInt] = FunctionNode("is_nan", x)
  def fabs[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("fabs", x)
  def abs[T <: StanScalarType](x: StanValue[T]): StanValue[T] = FunctionNode("abs", x)
  def fdim(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fdim", x, y)
  def fmin(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmin", x, y)
  def fmax(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmax", x, y)
  def fmod(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmod", x, y)
  def floor[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("floor", x)
  def ceil[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("ceil", x)
  def round[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("round", x)
  def trunc[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("trunc", x)

  // Power and Logarithm functions (40.8).
  def sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sqrt", x)
  def cbrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cbrt", x)
  def square[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("square", x)
  def exp[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("exp", x)
  def exp2[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("exp2", x)
  def log[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log", x)
  def log2[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log2", x)
  def log10[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log10", x)
  def pow(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("pow", x, y)
  def inv[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv", x)
  def invSqrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_sqrt", x)
  def invSquare[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_square", x)

  // Trigonometric functions (40.9).
  def hypot(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("hypot", x, y)
  def cos[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cos", x)
  def sin[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sin", x)
  def tan[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("tan", x)
  def acos[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("acos", x)
  def asin[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("asin", x)
  def atan[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("atan", x)
  def atan2(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("atan2", x, y)

  // Hyperbolic Trigonometric functions (40.10).
  def cosh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cosh", x)
  def sinh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sinh", x)
  def tanh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sinh", x)
  def acosh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("acosh", x)
  def asinh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("asinh", x)
  def atanh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("atanh", x)

  // Link functions (40.11).
  def logit[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("logit", x)
  def invLogit[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_logit", x)
  def invCLoglog[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_cloglog", x)

  // Probability related functions (40.12).
  def erf[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("erf", x)
  def erfc[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("erfc", x)
  def phi[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("Phi", x)
  def invPhi[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_Phi", x)
  def phiApprox[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("Phi_approx", x)
  def binaryLogLoss(x: StanValue[StanInt], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("binary_log_loss", x, y)
  def owensT(h: StanValue[StanReal], a: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("owens_t", h, a)

  // Combinatorial functions (40.13).
  def incBeta(alpha: StanValue[StanReal], beta: StanValue[StanReal], x: StanValue[StanReal]): StanValue[StanReal] =
    FunctionNode("inc_beta", alpha, beta, x)
  def lbeta(alpha: StanValue[StanReal], beta: StanValue[StanReal]): StanValue[StanReal] =
    FunctionNode("lbeta", alpha, beta)
  def tgamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("tgamma", x)
  def lgamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("lgamma", x)
  def digamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("digamma", x)
  def trigamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("trigamma", x)
  //TODO

  // Composed functions (40.14).
  //TODO

  // Array reductions (41.1).
  def min[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("min", x)
  def max[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("max", x)
  def sum[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("sum", x)
  def prod[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("prod", x)
  def log_sum_exp[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("log_sum_exp", x)
  def mean[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("mean", x)
  def variance[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("variance", x)
  def sd[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("sd", x)
  def distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = FunctionNode("distance", x, y)
  def squaredDistance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = FunctionNode("squared_distance", x, y)

  // Array size and dimension function (41.2).
  def dims[T <: StanType](x: StanValue[T]): StanValue[StanArray[StanInt]] = FunctionNode("dims", x)
  def size[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("size", x)

  // Array broadcasting (41.3).
  def repArray[T <: StanType](x: StanValue[T], n: StanValue[StanInt]): StanValue[StanArray[T]]
    = FunctionNode("rep_array", x, n)
  def repArray[T <: StanType](
    x: StanValue[T],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[T]]] = FunctionNode("rep_array", x, m, n)
  def repArray[T <: StanType](
    x: StanValue[T],
    k: StanValue[StanInt],
    m: StanValue[StanInt],
    n: StanValue[StanInt]
  ): StanValue[StanArray[StanArray[StanArray[T]]]] = FunctionNode("rep_array", x, k, m, n)

  // Sorting functions (41.4).
  def sortAsc[T <: StanType](v: StanValue[StanArray[T]]): StanValue[StanArray[T]] = FunctionNode("sort_asc", v)
  def sortDesc[T <: StanType](v: StanValue[StanArray[T]]): StanValue[StanArray[T]] = FunctionNode("sort_desc", v)
  def sortIndicesAsc[T <: StanType](v: StanValue[StanArray[T]]): StanValue[StanArray[StanInt]] =
    FunctionNode("sort_indices_asc", v)
  def sortIndicesDesc[T <: StanType](v: StanValue[StanArray[T]]): StanValue[StanArray[StanInt]] =
    FunctionNode("sort_indices_desc", v)
  def rank[T <: StanScalarType](v: StanValue[StanArray[T]], s: StanValue[StanInt]): StanValue[StanInt] =
    FunctionNode("rank", v, s)

  // Integer-valued matrix size functions (42.1).
  def numElements[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("num_elements", x)
  def rows[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("rows", x)
  def cols[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("cols", x)

  // Dot products and specialized products (42.5).
  //TODO

  // Slicing and blocking functions (42.9).
  //TODO

  // Matrix concatenation (42.10).
  //TODO

  // Special matrix functions (42.11).
  //TODO

  // Covariance functions (42.12).
  //TODO

  // Mixed operations (44).
  //TODO

  // ODE Solvers (45).
  //TODO

}
