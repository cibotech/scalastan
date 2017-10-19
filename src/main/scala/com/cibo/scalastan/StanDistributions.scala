package com.cibo.scalastan

protected trait StanDistributions {

  def Beta(
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanValue[StanReal] = FunctionNode("beta", Seq(alpha, beta))

  def BetaBinomial(
    n: StanValue[StanInt],
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanValue[StanReal] = FunctionNode("beta_binomial", Seq(n, alpha, beta))

  def Bernoulli[T <: StanType](theta: StanValue[T]): StanValue[T] = FunctionNode("bernoulli", Seq(theta))

  def BernoulliLogit[T <: StanType](alpha: StanValue[T]): StanValue[T] = FunctionNode("bernoulli_logit", Seq(alpha))

  def Binomial[T <: StanType](n: StanValue[StanInt], theta: StanValue[T]): StanValue[T] =
    FunctionNode("binomial", Seq(n, theta))

  def BinomialLogit[T <: StanType](n: StanValue[StanInt], alpha: StanValue[T]): StanValue[T] =
    FunctionNode("binomial_logit", Seq(n, alpha))

  def Cauchy[R <: StanType](
    position: StanValue[StanReal],
    scale: StanValue[StanReal]
  ): StanValue[R] = FunctionNode("cauchy", Seq(position, scale))

  def Exponential[R <: StanType](lambda: StanValue[StanReal]): StanValue[R] =
    FunctionNode("exponential", Seq(lambda))

  def Gamma(alpha: StanValue[StanReal], beta: StanValue[StanReal]): StanValue[StanReal] =
    FunctionNode("gamma", Seq(alpha, beta))

  def Hypergeometric[T <: StanType](
    n: StanValue[StanInt],
    a: StanValue[T],
    b: StanValue[T]
  ): StanValue[T] = FunctionNode("hypergeometric", Seq(n, a, b))

  def Normal[A <: StanType, B <: StanType, R <: StanType](
    mu: StanValue[A],
    sigma: StanValue[B]
  ): StanValue[R] = FunctionNode("normal", Seq(mu, sigma))
}
