package com.cibo.scalastan

case class StanResults private (values: Seq[Map[String, String]]) {

  lazy val bestIndex: Int = values.zipWithIndex.maxBy(v => v._1("lp__").toDouble)._2

  def samples[T <: StanType, R](
    decl: StanDeclaration[T, ParameterDeclarationType]
  )(implicit ev: T#SCALA_TYPE =:= R): Vector[R] = {
    val name = decl.emit
    values.map { value => decl.typeConstructor.parse(name, value) }.toVector.asInstanceOf[Vector[R]]
  }

  lazy val logProbabilities: Seq[Double] = values.map(v => v("lp__").toDouble)

  def best[T <: StanType, R](
    decl: StanDeclaration[T, ParameterDeclarationType]
  )(implicit ev: T#SCALA_TYPE =:= R): R = {
    decl.typeConstructor.parse(decl.emit, values(bestIndex)).asInstanceOf[R]
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
    decl: StanDeclaration[T, ParameterDeclarationType]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    val tc = decl.typeConstructor
    tc.combine(samples(decl).asInstanceOf[Seq[tc.SCALA_TYPE]])(mean).asInstanceOf[R]
  }

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
    decl: StanDeclaration[T, ParameterDeclarationType]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    val tc = decl.typeConstructor
    tc.combine(samples(decl).asInstanceOf[Seq[tc.SCALA_TYPE]])(variance).asInstanceOf[R]
  }

  private def sd(values: Seq[Double]): Double = math.sqrt(variance(values))

  def sd[T <: StanType, R](
    decl: StanDeclaration[T, ParameterDeclarationType]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    decl.typeConstructor.combine(samples(decl).asInstanceOf[Seq[decl.typeConstructor.SCALA_TYPE]])(sd).asInstanceOf[R]
  }

  private def percentile(frac: Double)(values: Seq[Double]): Double = {
    val index = (values.size.toDouble * frac).round.toInt
    values.sorted.apply(index)
  }

  def percentile[T <: StanType, R](
    decl: StanDeclaration[T, ParameterDeclarationType],
    frac: Double = 0.5
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    require(frac > 0.0 && frac < 1.0)
    val tc = decl.typeConstructor
    tc.combine(samples(decl).asInstanceOf[Seq[tc.SCALA_TYPE]])(percentile(frac)).asInstanceOf[R]
  }
}
