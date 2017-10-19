package com.cibo.scalastan

protected trait StanDistributions {

  def Beta(
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanContinuousDistribution[StanReal] = StanContinuousDistribution("beta", alpha, beta)

  def BetaBinomial(
    n: StanValue[StanInt],
    alpha: StanValue[StanReal],
    beta: StanValue[StanReal]
  ): StanDiscreteDistributionWithCdf[StanReal] = StanDiscreteDistributionWithCdf("beta_binomial", n, alpha, beta)

  def Bernoulli[T <: StanType](theta: StanValue[T]): StanDiscreteDistributionWithCdf[T] =
    StanDiscreteDistributionWithCdf("bernoulli", theta)

  def BernoulliLogit[T <: StanType](alpha: StanValue[T]): StanDiscreteDistributionWithoutCdf[T] =
    StanDiscreteDistributionWithoutCdf("bernoulli_logit", alpha)

  def Binomial[T <: StanType](n: StanValue[StanInt], theta: StanValue[T]): StanDiscreteDistributionWithCdf[T] =
    StanDiscreteDistributionWithCdf("binomial", n, theta)

  def BinomialLogit[T <: StanType](n: StanValue[StanInt], alpha: StanValue[T]): StanDiscreteDistributionWithoutCdf[T] =
    StanDiscreteDistributionWithoutCdf("binomial_logit", n, alpha)

  def Cauchy[R <: StanType](
    position: StanValue[StanReal],
    scale: StanValue[StanReal]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("cauchy", position, scale)

  def Exponential[R <: StanType](lambda: StanValue[StanReal]): StanContinuousDistribution[R] =
    StanContinuousDistribution("exponential", lambda)

  def Gamma(alpha: StanValue[StanReal], beta: StanValue[StanReal]): StanContinuousDistribution[StanReal] =
    StanContinuousDistribution("gamma", alpha, beta)

  def Hypergeometric[T <: StanType](
    n: StanValue[StanInt],
    a: StanValue[T],
    b: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[T] = StanDiscreteDistributionWithoutCdf("hypergeometric", n, a, b)

  def Normal[A <: StanType, B <: StanType, R <: StanType](
    mu: StanValue[A],
    sigma: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("normal", mu, sigma)
}
