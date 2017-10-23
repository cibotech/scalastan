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

  def ChiSquare[N <: StanType: ContinuousType, R <: StanType](
    nu: StanValue[N]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("chi_square", nu)

  def Dirichlet(alpha: StanValue[StanVector]): StanContinuousDistribution[StanVector] =
    StanContinuousDistribution("dirichlet", alpha)

  def DoubleExponential[M <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("double_exponential", mu, sigma)

  def ExpModNormal[M <: StanType: ContinuousType, S <: StanType: ContinuousType, L <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    sigma: StanValue[S],
    lambda: StanValue[L]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("exp_mod_normal", mu, sigma, lambda)

  def Exponential[L <: StanType: ContinuousType, R <: StanType](
    lambda: StanValue[L]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("exponential", lambda)

  def Frechet[A <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("frechet", alpha, sigma)

  def Gamma[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("gamma", alpha, beta)

  def GaussianDlmObs(
    f: StanValue[StanMatrix],
    g: StanValue[StanMatrix],
    v: StanValue[StanMatrix],
    w: StanValue[StanMatrix],
    m0: StanValue[StanVector],
    c0: StanValue[StanMatrix]
  ): StanContinuousDistribution[StanMatrix] = StanContinuousDistribution("gaussian_dlm_obs", f, g, v, w, m0, c0)

  def Gumbel[M <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("gumbel", mu, beta)

  def Hypergeometric[N <: StanType: DiscreteType, A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    n: StanValue[N],
    a: StanValue[A],
    b: StanValue[B]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("hypergeometric", n, a, b)

  def InvChiSquare[N <: StanType: ContinuousType, R <: StanType](
    nu: StanValue[N]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("inv_chi_square", nu)

  def InvGamma[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("inv_gamma", alpha, beta)

  def InvWishart[N <: StanScalarType](
    nu: StanValue[N],
    sigma: StanValue[StanMatrix]
  ): StanContinuousDistribution[StanMatrix] = StanContinuousDistribution("inv_wishart", nu, sigma)

  def LkjCorr[E <: StanScalarType](
    eta: StanValue[E]
  ): StanContinuousDistribution[StanMatrix] = StanContinuousDistribution("lkj_corr", eta)

  def LkjCholesky[E <: StanScalarType](
    eta: StanValue[E]
  ): StanContinuousDistribution[StanMatrix] = StanContinuousDistribution("lkj_cholesky", eta)

  def Logistic[M <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("logistic", mu, sigma)

  def LogNormal[M <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("lognormal", mu, sigma)

  def MultiGP(sigma: StanValue[StanMatrix], w: StanValue[StanVector]): StanContinuousDistribution[StanReal] =
    StanContinuousDistribution("multi_gp", sigma, w)

  def MultiGPCholesky(l: StanValue[StanMatrix], w: StanValue[StanVector]): StanContinuousDistribution[StanReal] =
    StanContinuousDistribution("multi_gp_cholesky", l, w)

  def Multinomial[T <: StanType: ContinuousType, R <: StanType](
    theta: StanValue[T]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("multinomial", theta)

  def MultiNormal[M <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    sigma: StanValue[StanMatrix]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("multi_normal", mu, sigma)

  def MultiNormalCholesky[M <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    l: StanValue[StanMatrix]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("multi_normal_cholesky", mu, l)

  def MultiNormalPrecision[M <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    omega: StanValue[StanMatrix]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("multi_normal_prec", mu, omega)

  def MultiStudentT[N <: StanScalarType, M <: StanVectorLike: ContinuousType, R <: StanType](
    nu: StanValue[N],
    mu: StanValue[M],
    sigma: StanValue[StanMatrix]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("multi_student_t", nu, mu, sigma)

  def NegativeBinomial[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("neg_binomial", alpha, beta)

  def NegativeBinomial2[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[A],
    phi: StanValue[B]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("neg_binomial_2", mu, phi)

  def NegativeBinomial2Log[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    eta: StanValue[A],
    phi: StanValue[B]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("neg_binomial_2_log", eta, phi)

  def Normal[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[A],
    sigma: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("normal", mu, sigma)

  def OrderedLogistic[E <: StanScalarType](
    eta: StanValue[E],
    c: StanValue[StanVector]
  ): StanDiscreteDistributionWithoutCdf[StanInt] = StanDiscreteDistributionWithoutCdf("ordered_logistic", eta, c)

  def Pareto[Y <: StanType: ContinuousType, A <: StanType: ContinuousType, R <: StanType](
    ymin: StanValue[Y],
    alpha: StanValue[A]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("pareto", ymin, alpha)

  def ParetoType2[M <: StanType: ContinuousType, L <: StanType: ContinuousType, A <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    lambda: StanValue[L],
    alpha: StanValue[A]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("pareto_type_2", mu, lambda, alpha)

  def Poisson[L <: StanType: ContinuousType, R <: StanType](
    lambda: StanValue[L]
  ): StanDiscreteDistributionWithCdf[R] = StanDiscreteDistributionWithCdf("poisson", lambda)

  def PoissonLog[A <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A]
  ): StanDiscreteDistributionWithoutCdf[R] = StanDiscreteDistributionWithoutCdf("poisson_log", alpha)

  def Rayleigh[S <: StanType: ContinuousType, R <: StanType](
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("rayleigh", sigma)

  def ScaledInvChiSquare[N <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    nu: StanValue[N],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("scaled_inv_chi_suqare", nu, sigma)

  def SkewNormal[X <: StanType: ContinuousType, O <: StanType: ContinuousType, A <: StanType: ContinuousType, R <: StanType](
    xi: StanValue[X],
    omega: StanValue[O],
    alpha: StanValue[A]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("skew_normal", xi, omega, alpha)

  def StudentT[N <: StanType: ContinuousType, M <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    nu: StanValue[N],
    mu: StanValue[M],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("student_t", nu, mu, sigma)

  def Uniform[A <: StanType: ContinuousType, B <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    beta: StanValue[B]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("uniform", alpha, beta)

  // CDF functions not available
  def VonMises[M <: StanType: ContinuousType, K <: StanType: ContinuousType, R <: StanType](
    mu: StanValue[M],
    kappa: StanValue[K]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("von_mises", mu, kappa)

  def Weibull[A <: StanType: ContinuousType, S <: StanType: ContinuousType, R <: StanType](
    alpha: StanValue[A],
    sigma: StanValue[S]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("weibull", alpha, sigma)

  def Wiener[
    A <: StanType: ContinuousType,
    T <: StanType: ContinuousType,
    B <: StanType: ContinuousType,
    D <: StanType: ContinuousType,
    R <: StanType
  ](
    alpha: StanValue[A],
    tau: StanValue[T],
    beta: StanValue[B],
    delta: StanValue[D]
  ): StanContinuousDistribution[R] = StanContinuousDistribution("wiener", alpha, tau, beta, delta)

  def Wishart[N <: StanScalarType](
    nu: StanValue[N],
    sigma: StanValue[StanMatrix]
  ): StanContinuousDistribution[StanMatrix] = StanContinuousDistribution("wishart", nu, sigma)
}
