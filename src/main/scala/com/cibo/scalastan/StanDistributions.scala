package com.cibo.scalastan

protected trait StanDistributions {

  def Beta(
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanDistribution[StanReal] = StanDistribution("beta", alpha, beta)

  def BetaBinomial(
    n: StanValue[StanInt],
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanDistribution[StanReal] = StanDistribution("beta_binomial", n, alpha, beta)

  def Bernoulli[T <: StanType](theta: StanValue[T]): StanDistribution[T] =
    StanDistribution("bernoulli", theta)

  def BernoulliLogit[T <: StanType](alpha: StanValue[T]): StanDistribution[T] =
    StanDistribution("bernoulli_logit", alpha)

  def Binomial[T <: StanType](n: StanValue[StanInt], theta: StanValue[T]): StanDistribution[T] =
    StanDistribution("binomial", n, theta)

  def BinomialLogit[T <: StanType](n: StanValue[StanInt], alpha: StanValue[T]): StanDistribution[T] =
    StanDistribution("binomial_logit", n, alpha)

  def Cauchy[R <: StanType](
    position: StanValue[StanReal],
    scale: StanValue[StanReal]
  ): StanDistribution[R] = StanDistribution("cauchy", position, scale)

  def Exponential[R <: StanType](lambda: StanValue[StanReal]): StanDistribution[R] =
    StanDistribution("exponential", lambda)

  def Gamma(alpha: StanValue[StanReal], beta: StanValue[StanReal]): StanDistribution[StanReal] =
    StanDistribution("gamma", alpha, beta)

  def Hypergeometric[T <: StanType](
    n: StanValue[StanInt],
    a: StanValue[T],
    b: StanValue[T]
  ): StanDistribution[T] = StanDistribution("hypergeometric", n, a, b)

  def Normal[A <: StanType, B <: StanType, R <: StanType](
    mu: StanValue[A],
    sigma: StanValue[B]
  ): StanDistribution[R] = StanDistribution("normal", mu, sigma)
}
