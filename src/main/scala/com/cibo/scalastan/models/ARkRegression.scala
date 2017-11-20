package com.cibo.scalastan.models

import com.cibo.scalastan._

case class ARkRegression(
  xs: Seq[Seq[Double]],                       // Inputs
  ys: Seq[Double],                            // Outputs
  covariates: Seq[Int],                       // Covariates (order within a group)
  groupingFactorOpt: Option[Seq[Int]] = None, // Value to group by (no grouping if not set)
  order: Int = 1,                             // Order of the model (AR(1) by default)
  priorSigma: Double = 1000.0                 // Sigma for coefficient priors
) extends ScalaStan {

  require(xs.length == ys.length, "Length of inputs (xs) must match the length of the outputs (ys)")
  require(covariates.length == ys.length, "Length of covariates must match the length of the outputs")
  require(groupingFactorOpt.map(_.length).getOrElse(ys.length) == ys.length, "Incorrect grouping factor length")
  require(groupingFactorOpt.forall(_.forall(_ > 0)), "Grouping factors must be > 0")
  require(order >= 0, "Order must be >= 0.")

  private val n = data(int(lower = 0))      // Number of observations
  private val m = data(int(lower = 0))      // Number of groups
  private val p = data(int(lower = 0))      // Number of parameters in the model (excluding the covariate and group)
  private val k = data(int(lower = 0))      // Order

  private val x = data(matrix(n, p))
  private val group = data(int()(n))
  private val y = data(vector(n))

  val alpha: StanParameterDeclaration[StanReal] = parameter(real())
  val beta: StanParameterDeclaration[StanMatrix] = parameter(matrix(m, k))
  val xbeta: StanParameterDeclaration[StanVector] = parameter(vector(p))
  val sigma: StanParameterDeclaration[StanReal] = parameter(real(lower = 0))

  private val model = new Model {

    sigma ~ InvGamma(0.01, 0.01)
    xbeta ~ Normal(0, priorSigma)
    for (i <- range(1, m)) {
      beta(i) ~ Normal(0, priorSigma)
    }

    // Number of observations in the current group.
    val count = local(int())

    // ID of the current group.
    val currentGroup = local(int())

    count := 0
    currentGroup := group(1)

    for (i <- range(1, n)) {
      when(group(i) =/= currentGroup) {
        count := 0
        currentGroup := group(i)
      }
      count += 1

      when(count > k) {
        val mu = local(real())
        mu := alpha + dotProduct(x(i), xbeta)
        for (j <- range(1, k)) {
          mu += beta(currentGroup, j) * y(i - j)
        }
        y(i) ~ Normal(mu, sigma)
      }
    }
  }

  // Sort with by group and covariate within group.
  private lazy val groupingFactor = groupingFactorOpt.getOrElse(ys.indices.map(_ => 1))
  private lazy val sortedIndices = groupingFactor.zip(covariates).zipWithIndex.sorted.map(_._2)
  private lazy val sortedGroups = sortedIndices.map(groupingFactor.apply)
  private lazy val sortedCovariates = sortedIndices.map(covariates.apply)
  private lazy val sortedXs = sortedIndices.map(xs.apply)
  private lazy val sortedYs = sortedIndices.map(ys.apply)
  private lazy val groupCount = groupingFactor.max

  def compile: CompiledModel = model.compile
    .withData(x, sortedXs)
    .withData(y, sortedYs)
    .withData(group, sortedGroups)
    .withData(k, order)
    .withData(m, groupCount)

  /** Return predictions for a group.  This assumes the data is sorted by covariate. */
  def predict(data: Seq[Seq[Double]], group: Int, results: StanResults): Vector[Double] = {
    val bestAlpha = results.best(alpha)
    val bestBeta = results.best(beta).apply(group - 1)
    val bestXBeta = results.best(xbeta)
    data.foldLeft(Vector.empty[Double]) { case (lst, ds) =>
      require(ds.length == bestXBeta.length)
      val reg = if (lst.size >= order) {
        bestBeta.zip(lst.takeRight(order)).map { case (b, v) => b * v }.sum
      } else 0.0
      lst :+ bestAlpha + reg + bestXBeta.zip(ds).map { case (b, v) => b * v }.sum
    }
  }
}
