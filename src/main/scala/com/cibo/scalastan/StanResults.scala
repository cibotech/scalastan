package com.cibo.scalastan

import java.io.PrintStream

import scala.util.Try

case class StanResults private (private val chains: Seq[Seq[Map[String, String]]]) {

  private val lpName = "lp__"
  private val divergentName = "divergent__"

  lazy val bestChain: Int = chains.zipWithIndex.maxBy(_._1.map(_.apply(lpName).toDouble).max)._2
  lazy val bestIndex: Int = chains(bestChain).zipWithIndex.maxBy(_._1.apply(lpName).toDouble)._2

  lazy val logProbabilities: Seq[Seq[Double]] = chains.map(c => c.map(v => v(lpName).toDouble))
  lazy val divergent: Seq[Seq[Double]] = chains.map(c => c.map(v => v(divergentName).toDouble))

  val chainCount: Int = chains.size
  val iterationsPerChain: Int = chains.head.size
  val iterationsTotal: Int = chains.map(_.size).sum

  def samples[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: T#SCALA_TYPE =:= R): Seq[Seq[R]] = {
    val name = decl.emit
    chains.map { chain =>
      chain.map { values =>
        decl.typeConstructor.parse(name, values)
      }
    }.asInstanceOf[Seq[Seq[R]]]
  }

  def best[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: T#SCALA_TYPE =:= R): R = samples(decl).apply(bestChain).apply(bestIndex)

  private def combine[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(
    func: Seq[Seq[Double]] => Double
  )(
    implicit ev: R =:= T#SUMMARY_TYPE
  ): R = {
    val tc = decl.typeConstructor
    tc.combine(samples(decl).asInstanceOf[Seq[Seq[tc.SCALA_TYPE]]])(func).asInstanceOf[R]
  }

  private def combineSingle[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(
    func: Seq[Double] => Double
  )(
    implicit ev: R =:= T#SUMMARY_TYPE
  ): R = combine(decl)(vs => func(vs.flatten))

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
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(mean)

  private def mcse(values: Seq[Seq[Double]]): Double = {
    sd(values.flatten) / math.sqrt(effectiveSampleSize(values))
  }

  def mcse[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(mcse)

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
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(variance)

  private def sd(values: Seq[Double]): Double = math.sqrt(variance(values))

  def sd[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(sd)

  private def quantile(frac: Double)(values: Seq[Double]): Double = {
    val index = (values.size.toDouble * frac).round.toInt
    values.sorted.apply(index)
  }

  def quantile[T <: StanType, R](
    decl: StanParameterDeclaration[T],
    prob: Double = 0.5
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    require(prob > 0.0 && prob < 1.0)
    combineSingle(decl)(quantile(prob))
  }

  def min[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(_.min)

  def max[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(_.max)

  private def autocovariance(k: Int, x: Seq[Double]): Double = {
    val n = x.size
    val xMean = mean(x)
    mean {
      (0 until n - k).map { i =>
        (x(i + k) - xMean) * (x(i) - xMean)
      }
    }
  }

  private def effectiveSampleSize(values: Seq[Seq[Double]]): Double = {
    val n = values.head.size // Samples per chain
    val m = values.size // Number of chains
    val chainMean = values.map(mean)
    val chainVar = values.map(variance)
    val meanVar = mean(chainVar)
    val varPlus = meanVar * (n - 1) / n + (if (m > 1) variance(chainMean) else 0.0)
    val rho = (1 until n).map { t =>
      val acov_t = values.map(vs => autocovariance(t, vs))
      1.0 - (meanVar - mean(acov_t)) / varPlus
    }.takeWhile(_ >= 0.0)
    (m * n) / (if (rho.size > 1) 1.0 + 2.0 * rho.sum else 1.0)
  }

  def effectiveSampleSize[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(effectiveSampleSize)

  private def betweenSampleVariance(values: Seq[Seq[Double]]): Double = {
    val m = values.size.toDouble
    val n = values.head.size.toDouble
    val totalMean = mean(values.flatten)
    (n / (m - 1.0)) * values.map { vs =>
      math.pow(mean(vs) - totalMean, 2)
    }.sum
  }

  private def withinSampleVariance(values: Seq[Seq[Double]]): Double = {
    val m = values.size.toDouble
    values.map(variance).sum / m
  }

  private def prsf(values: Seq[Seq[Double]]): Double = {
    val n = values.head.size.toDouble
    val w = withinSampleVariance(values)
    val b = betweenSampleVariance(values)
    val varPlus = ((n - 1.0) * w + b) / n
    math.sqrt(varPlus / w)
  }

  private def cleanName(name: String, mapping: Map[String, String]): String = {
    val parts = name.split("\\.")
    val first = mapping.getOrElse(parts.head, parts.head)
    if (parts.tail.nonEmpty) {
      val indices = parts.tail.mkString("[", ",", "]")
      s"$first$indices"
    } else {
      first
    }
  }

  def summary(ps: PrintStream)(implicit ss: ScalaStan): Unit = {

    val fieldWidth = 8

    // Get a mapping from Stan name to ScalaStan name.
    val mapping = ss.parameters.map(p => p.emit -> p.name).toMap

    // Build a mapping of name -> chain -> iteration -> value
    val names = chains.head.head.keys
    val results: Map[String, Seq[Seq[Double]]] = names.par.map { name =>
      cleanName(name, mapping) -> chains.map { chain =>
        chain.map { iteration =>
          Try(iteration(name).toDouble).getOrElse(Double.NaN)
        }
      }
    }.seq.toMap

    // The statistics to output.
    val stats: Seq[(String, Seq[Seq[Double]] => Double)] = Seq(
      "Mean" -> (vs => mean(vs.flatten)),
      "MCSE" -> mcse,
      "StdDev" -> (vs => sd(vs.flatten)),
      "5%" -> (vs => quantile(0.05)(vs.flatten)),
      "50%" -> (vs => quantile(0.5)(vs.flatten)),
      "95%" -> (vs => quantile(0.95)(vs.flatten)),
      "N_Eff" -> effectiveSampleSize,
      "R_hat" -> prsf
    )

    // Compute the statistics.
    val data: Seq[(String, Seq[Double])] = results.toSeq.sortBy(_._1).par.map { case (name, values) =>
      name -> stats.map { case (_, stat) => stat(values) }
    }.seq

    ps.println()
    ps.println(s"$chainCount chains with $iterationsPerChain iterations each, $iterationsTotal total")
    ps.println()

    // Write the header.
    val padding = "  "
    val maxNameLength = data.map(_._1.length).max
    ps.print("Name" + " " * (maxNameLength - 4) + padding)
    stats.foreach { case (name, _) =>
      val spaceCount = fieldWidth - name.length
      ps.print(" " * spaceCount + name + padding)
    }
    ps.println()

    // Write the data.
    data.foreach { case (name, values) =>
      val namePadding = " " * (maxNameLength + padding.length - name.length)
      ps.print(s"$name$namePadding")
      values.foreach { value =>
        ps.print(String.format(s"%${fieldWidth}.4g", Double.box(value)) + padding)
      }
      ps.println()
    }

  }
}
