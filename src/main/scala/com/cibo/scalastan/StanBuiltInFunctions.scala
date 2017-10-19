package com.cibo.scalastan

import scala.collection.mutable.ArrayBuffer

protected trait StanBuiltInFunctions {

  // Print (38.1).
  def print(args: StanValue[_]*)(implicit code: ArrayBuffer[StanValue[_]]): Unit = {
    code += FunctionNode[StanVoid]("print", args)
  }

  // Integer functions (39.2).
  def intStep[T <: StanScalarType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("int_step", Seq(x))

  // Bound functions (39.3).
  def min(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = FunctionNode("min", Seq(x, y))
  def max(x: StanValue[StanInt], y: StanValue[StanInt]): StanValue[StanInt] = FunctionNode("max", Seq(x, y))

  // Mathematical constants (40.2).
  def pi: StanValue[StanReal] = FunctionNode("pi", Seq())
  def e: StanValue[StanReal] = FunctionNode("e", Seq())
  def sqrt2: StanValue[StanReal] = FunctionNode("sqrt2", Seq())
  def log2: StanValue[StanReal] = FunctionNode("log2", Seq())
  def log10: StanValue[StanReal] = FunctionNode("log10", Seq())

  // Special values (40.3)
  def notANumber: StanValue[StanReal] = FunctionNode("not_a_number", Seq())
  def positiveInfinity: StanValue[StanReal] = FunctionNode("positive_infinity", Seq())
  def negativeInfinity: StanValue[StanReal] = FunctionNode("negative_infinity", Seq())
  def machinePrecision: StanValue[StanReal] = FunctionNode("machine_precision", Seq())

  // Step-like functions (40.7)
  def step(x: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("step", Seq(x))
  def isInf(x: StanValue[StanReal]): StanValue[StanInt] = FunctionNode("is_inf", Seq(x))
  def isNan(x: StanValue[StanReal]): StanValue[StanInt] = FunctionNode("is_nan", Seq(x))
  def fabs[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("fabs", Seq(x))
  def abs[T <: StanScalarType](x: StanValue[T]): StanValue[T] = FunctionNode("abs", Seq(x))
  def fdim(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fdim", Seq(x, y))
  def fmin(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmin", Seq(x, y))
  def fmax(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmax", Seq(x, y))
  def fmod(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("fmod", Seq(x, y))
  def floor[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("floor", Seq(x))
  def ceil[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("ceil", Seq(x))
  def round[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("round", Seq(x))
  def trunc[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("trunc", Seq(x))

  // Power and Logarithm functions (40.8).
  def sqrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sqrt", Seq(x))
  def cbrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cbrt", Seq(x))
  def square[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("square", Seq(x))
  def exp[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("exp", Seq(x))
  def exp2[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("exp2", Seq(x))
  def log[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log", Seq(x))
  def log2[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log2", Seq(x))
  def log10[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("log10", Seq(x))
  def pow(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("pow", Seq(x, y))
  def inv[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv", Seq(x))
  def invSqrt[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_sqrt", Seq(x))
  def invSquare[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_square", Seq(x))

  // Trigonometric functions (40.9).
  def hypot(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("hypot", Seq(x, y))
  def cos[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cos", Seq(x))
  def sin[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sin", Seq(x))
  def tan[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("tan", Seq(x))
  def acos[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("acos", Seq(x))
  def asin[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("asin", Seq(x))
  def atan[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("atan", Seq(x))
  def atan2(x: StanValue[StanReal], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("atan2", Seq(x, y))

  // Hyperbolic Trigonometric functions (40.10).
  def cosh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("cosh", Seq(x))
  def sinh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sinh", Seq(x))
  def tanh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("sinh", Seq(x))
  def acosh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("acosh", Seq(x))
  def asinh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("asinh", Seq(x))
  def atanh[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("atanh", Seq(x))

  // Link functions (40.11).
  def logit[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("logit", Seq(x))
  def invLogit[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_logit", Seq(x))
  def invCLoglog[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_cloglog", Seq(x))

  // Probability related functions (40.12).
  def erf[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("erf", Seq(x))
  def erfc[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("erfc", Seq(x))
  def phi[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("Phi", Seq(x))
  def invPhi[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("inv_Phi", Seq(x))
  def phiApprox[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("Phi_approx", Seq(x))
  def binaryLogLoss(x: StanValue[StanInt], y: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("binary_log_loss", Seq(x, y))
  def owensT(h: StanValue[StanReal], a: StanValue[StanReal]): StanValue[StanReal] = FunctionNode("owens_t", Seq(h, a))

  // Combinatorial functions (40.13).
  def incBeta(alpha: StanValue[StanReal], beta: StanValue[StanReal], x: StanValue[StanReal]): StanValue[StanReal] =
    FunctionNode("inc_beta", Seq(alpha, beta, x))
  def lbeta(alpha: StanValue[StanReal], beta: StanValue[StanReal]): StanValue[StanReal] =
    FunctionNode("lbeta", Seq(alpha, beta))
  def tgamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("tgamma", Seq(x))
  def lgamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("lgamma", Seq(x))
  def digamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("digamma", Seq(x))
  def trigamma[T <: StanType](x: StanValue[T]): StanValue[T] = FunctionNode("trigamma", Seq(x))
  //TODO

  // Composed functions (40.14).
  //TODO

  // Array reductions (41.1).
  def min[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("min", Seq(x))
  def max[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("max", Seq(x))
  def sum[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("sum", Seq(x))
  def prod[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("prod", Seq(x))
  def log_sum_exp[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("log_sum_exp", Seq(x))
  def mean[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("mean", Seq(x))
  def variance[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("variance", Seq(x))
  def sd[T <: StanCompoundType, E <: StanType](x: StanValue[T])(implicit ev: T#ELEMENT_TYPE =:= E): StanValue[E] =
    FunctionNode("sd", Seq(x))
  def distance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = FunctionNode("distance", Seq(x, y))
  def squaredDistance[A <: StanType, B <: StanType](
    x: StanValue[A], y: StanValue[B]
  )(implicit ev: DistanceAllowed[A, B]): StanValue[StanReal] = FunctionNode("squared_distance", Seq(x, y))

  // Array size and dimension function (41.2).
  def dims[T <: StanType](x: StanValue[T]): StanValue[StanArray[StanInt]] = FunctionNode("dims", Seq(x))
  def numElements[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("num_elements", Seq(x))
  def size[T <: StanCompoundType](x: StanValue[T]): StanValue[StanInt] = FunctionNode("size", Seq(x))

  // Array broadcasting (41.3).
  //TODO

  // Sorting functions (41.4).
  //TODO

  // Integer-valued matrix size functions (42.1).
  //TODO

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
