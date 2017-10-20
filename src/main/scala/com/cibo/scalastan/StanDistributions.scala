package com.cibo.scalastan

protected trait StanDistributions {

  def Beta[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("beta", alpha, beta)

  def BetaBinomial[N <: StanType: DiscreteType, A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    n: StanValue[N],
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("beta_binomial", n, alpha, beta)

  def Bernoulli[T <: StanType: ContinuousType, R <: StanType](
    theta: StanValue[T]
  ): StanDiscreteDistributionWithCdf[R] =
    StanDiscreteDistributionWithCdf("bernoulli", theta)

  def BernoulliLogit[T <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[R] =
    StanDiscreteDistributionWithoutCdf("bernoulli_logit", alpha)

  def Binomial[N <: StanType: DiscreteType, T <: StanType: ContinuousType, R <: StanType](
    n: StanValue[N],
    theta: StanValue[T]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("binomial", n, theta)

  def BinomialLogit[N <: StanType: DiscreteType, T <: StanType: ContinuousType, R <: StanType](
    n: StanValue[N],
    alpha: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("binomial_logit", n, alpha)

  def Categorical[T <: StanType: ContinuousType, R <: StanType](
    theta: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("categorical", theta)

  def CategoricalLogit[T <: StanType: ContinuousType, R <: StanType](
    beta: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("categorical_logit", beta)

  def Cauchy[P <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    position: StanValue[P],
    scale: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("cauchy", position, scale)

  def Exponential[L <: StanType: ContinuousType, R <: StanType](
    lambda: StanValue[L]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("exponential", lambda)

  def Gamma[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("gamma", alpha, beta)

  def Hypergeometric[N <: StanType: DiscreteType, A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    n: StanValue[N],
    a: StanValue[A],
    b: StanValue[B]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("hypergeometric", n, a, b)

  def Normal[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[A],
    sigma: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("normal", mu, sigma)

  def OrderedLogistic(
    eta: StanValue[StanReal],
    c: StanValue[StanVector]
  ): StanDiscreteDistributionWithoutCdf[StanInt] = StanDiscreteDistributionWithoutCdf("ordered_logistic", eta, c)

  def NegativeBinomial[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("neg_binomial", alpha, beta)
}
