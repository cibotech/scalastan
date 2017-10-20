package com.cibo.scalastan

case class StanResults private (values: Seq[Map[String, String]]) {

  lazy val bestIndex: Int = values.zipWithIndex.maxBy(v => v._1("lp__").toDouble)._2

  def samples[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: T#SCALA_TYPE =:= R): Vector[R] = {
    val name = decl.emit
    values.map { value => decl.typeConstructor.parse(name, value) }.toVector.asInstanceOf[Vector[R]]
  }

  lazy val logProbabilities: Seq[Double] = values.map(v => v("lp__").toDouble)
  lazy val divergent: Seq[Double] = values.map(v => v("divergent__").toDouble)

  def best[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: T#SCALA_TYPE =:= R): R = samples(decl).apply(bestIndex)

  private def combine[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(
    func: Seq[Double] => Double
  )(
    implicit ev: R =:= T#SUMMARY_TYPE
  ): R = {
    val tc = decl.typeConstructor
    tc.combine(samples(decl).asInstanceOf[Seq[tc.SCALA_TYPE]])(func).asInstanceOf[R]
  }

  private def mean(values: Seq[Double]): Double = {
    var current = 0.0
    var count = 0.0
    values.foreach { value =>
      val delta = value - current
      count += 1.0
      current += delta / count
    }
    current
  }

  def mean[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(mean)

  private def variance(values: Seq[Double]): Double = {
    var n = 0.0
    var m1 = 0.0
    var m2 = 0.0
    values.foreach { x =>
      n += 1.0
      val delta = x - m1
      m1 += delta / n
      val delta2 = x - m1
      m2 += delta * delta2
    }
    if (n < 2.0) Double.NaN else m2 / (n - 1.0)
  }

  def variance[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(variance)

  private def sd(values: Seq[Double]): Double = math.sqrt(variance(values))

  def sd[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(sd)

  private def percentile(frac: Double)(values: Seq[Double]): Double = {
    val index = (values.size.toDouble * frac).round.toInt
    values.sorted.apply(index)
  }

  def quantile[T <: StanType, R](
    decl: StanParameterDeclaration[T],
    prob: Double = 0.5
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    require(prob > 0.0 && prob < 1.0)
    combine(decl)(percentile(prob))
  }

  def min[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(_.min)

  def max[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(_.max)

  private def autocovariance(k: Int, x: Seq[Double]): Double = {
    val n = x.size
    val xMean = mean(x)
    (1.0 / (n - 1.0)) * (0 until n - k).map { i =>
      (x(i + k) - xMean) * (x(i) - xMean)
    }.sum
  }

  def effectiveSampleSize[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl) { values =>

    //TODO: support multiple chains

    val n = values.size // Samples per chain
    val m = 1 // Number of chains
    val chainMean = Seq(mean(values))
    val chainVar = Seq(variance(values))
    val meanVar = mean(chainVar)
    val varPlus = meanVar * (n - 1) / n + (if (m > 1) variance(chainMean) else 0.0)

    val rho = (1 until n).map { t =>
      val acov_t = Seq(autocovariance(t, values))
      1.0 - (meanVar - mean(acov_t)) / varPlus
    }.takeWhile(_ >= 0.0)
    (m * n) / (if (rho.size > 1) 1.0 + 2.0 * rho.sum else 1.0)
  }
}
