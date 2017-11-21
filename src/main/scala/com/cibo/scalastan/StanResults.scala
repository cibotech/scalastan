/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.{PrintStream, PrintWriter}

import scala.util.Try

case class StanResults private (
  private val chains: Vector[Vector[Map[String, String]]],
  private val model: CompiledModel
) {

  require(chains.nonEmpty, "No results")

  private val lpName = "lp__"
  private val divergentName = "divergent__"
  private val treeDepthName = "treedepth__"
  private val energyName = "energy__"

  lazy val bestChain: Int = chains.zipWithIndex.maxBy(_._1.map(_.apply(lpName).toDouble).max)._2
  lazy val bestIndex: Int = chains(bestChain).zipWithIndex.maxBy(_._1.apply(lpName).toDouble)._2

  lazy val logProbabilities: Seq[Seq[Double]] = chains.map(_.map(_.apply(lpName).toDouble))
  lazy val divergent: Seq[Seq[Double]] = chains.map(_.flatMap(_.get(divergentName).map(_.toDouble)))
  lazy val treeDepth: Seq[Seq[Double]] = chains.map(_.flatMap(_.get(treeDepthName).map(_.toDouble)))
  lazy val energy: Seq[Seq[Double]] = chains.map(_.flatMap(_.get(energyName).map(_.toDouble)))

  val chainCount: Int = chains.size
  val iterationsPerChain: Int = chains.head.size
  val iterationsTotal: Int = chains.map(_.size).sum

  /** Get the specified input data. */
  def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = model.get(decl)

  /** Get all samples by chain and iteration. */
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

  /** Get the best scoring sample. */
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

  private class Cache {
    private val cache = scala.collection.mutable.Map[String, Double]()
    def get(key: String, func: Seq[Double] => Double, values: Seq[Double]): Double = {
      if (key.nonEmpty) {
        cache.getOrElseUpdate(key, func(values))
      } else {
        func(values)
      }
    }
  }

  private def meanAndVariance(values: Seq[Double]): (Double, Double) = {
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
    (m1, if (n < 2.0) Double.NaN else m2 / (n - 1.0))
  }

  private def mean(values: Seq[Double]): Double = {
    var n = 0.0
    var result = 0.0
    values.foreach { x =>
      n += 1.0
      val delta = x - result
      result += delta / n
    }
    result
  }

  private def variance(values: Seq[Double]): Double = meanAndVariance(values)._2

  /** Get the mean of all samples across all chains and iterations. */
  def mean[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(mean)

  private def mcse(v: Double, ess: Double): Double = math.sqrt(v) / math.sqrt(ess)

  private def mcse(values: Seq[Seq[Double]]): Double =
    mcse(variance(values.flatten), effectiveSampleSize(values))

  /** Get the MCSE of a all samples across all chains and iterations. */
  def mcse[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(mcse)

  /** Get the variance of a all samples across all chains and iterations. */
  def variance[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(variance)

  private def sd(values: Seq[Double]): Double = math.sqrt(variance(values))

  /** Get the standard deviation of a all samples across all chains and iterations. */
  def sd[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(sd)

  private def quantileSorted(frac: Double)(sortedValues: Seq[Double]): Double = {
    val index = math.min((sortedValues.size.toDouble * frac).round.toInt, sortedValues.size - 1)
    sortedValues(index)
  }

  private def quantile(frac: Double)(values: Seq[Double]): Double = quantileSorted(frac)(values.sorted)

  /** Get the specified quantile of all samples across all chains and iterations. */
  def quantile[T <: StanType, R](
    decl: StanParameterDeclaration[T],
    prob: Double = 0.5
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    require(prob > 0.0 && prob < 1.0)
    combineSingle(decl)(quantile(prob))
  }

  /** Get the minimum of all samples across all chains and iterations. */
  def min[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combineSingle(decl)(_.min)

  /** Get the maximum of all samples across all chains and iterations. */
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
    val chainMeansAndVars = values.map(meanAndVariance)
    val chainMeans = chainMeansAndVars.map(_._1)
    val meanVar = mean(chainMeansAndVars.map(_._2))
    val varPlus = meanVar * (n - 1) / n + (if (m > 1) variance(chainMeans) else 0.0)
    val rho = (1 until n).view.map { t =>
      val acov_t = values.map(vs => autocovariance(t, vs))
      1.0 - (meanVar - mean(acov_t)) / varPlus
    }.takeWhile(_ >= 0.0)
    (m * n) / (if (rho.size > 1) 1.0 + 2.0 * rho.sum else 1.0)
  }

  /** Get the effective sample size of a parameter. */
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

  private def psrf(values: Seq[Seq[Double]]): Double = {
    val n = values.head.size.toDouble
    val w = withinSampleVariance(values)
    val b = betweenSampleVariance(values)
    val varPlus = ((n - 1.0) * w + b) / n
    math.sqrt(varPlus / w)
  }

  /** Get the potential scale reduction factor for a parameter. */
  def psrf[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = combine(decl)(psrf)

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

  def checkTreeDepth(maxDepth: Int = 10): Boolean = {
    val count = treeDepth.map(_.count(_ > maxDepth)).sum
    val p = (100 * count) / iterationsTotal
    println(s"$count of $iterationsTotal iterations saturated the maximum tree depth of $maxDepth ($p%)")
    count > 0
  }

  def checkEnergy(threshold: Double = 0.2): Boolean = {
    val count = energy.map(_.count(_ < threshold)).sum
    val p = (100 * count) / iterationsTotal
    println(s"$count of $iterationsTotal iterations exceeded the energy threshold of $threshold ($p%)")
    count > 0
  }

  private lazy val divergentCount: Int = divergent.map(_.count(_ > 0.0)).sum
  private lazy val percentDiverged: Int = (100 * divergentCount) / iterationsTotal

  def checkDivergence(): Int = {
    println(s"$divergentCount of $iterationsTotal iterations ended up with a divergence ($percentDiverged%)")
    divergentCount
  }

  /** Partition iterations into (divergent, non-divergent) samples. */
  def partitionByDivergence[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SUMMARY_TYPE): (Seq[R], Seq[R]) = {
    val partitioned = samples(decl).flatten.zip(divergent.flatten).partition { case (_, d) => d > 0.0 }
    (partitioned._1.map(_._1).asInstanceOf[Seq[R]], partitioned._2.map(_._1).asInstanceOf[Seq[R]])
  }

  def summary(ps: PrintStream): Unit = {
    val pw = new PrintWriter(ps)
    summary(pw)
    pw.flush()
  }

  private case class SummaryStats(values: Seq[Seq[Double]]) {
    private val flattened = values.flatten
    private val sortedValues = flattened.sorted
    val (meanResult, varianceResult) = meanAndVariance(flattened)
    val essResult: Double = effectiveSampleSize(values)
    val mcseResult: Double = mcse(varianceResult, essResult)
    val quantile5: Double = quantile(0.05)(sortedValues)
    val quantile50: Double = quantile(0.5)(sortedValues)
    val quantile95: Double = quantile(0.95)(sortedValues)
    val psrfResult: Double = psrf(values)
  }

  def summary(pw: PrintWriter): Unit = {

    val fieldWidth = 8

    // Get a mapping from Stan name to ScalaStan name.
    val mapping = model.ss.parameters.map(p => p.emit -> p.name).toMap

    // Build a mapping of name -> chain -> iteration -> value
    val names = chains.head.headOption.map(_.keys).getOrElse(Seq.empty)
    val results: Seq[(String, Seq[Seq[Double]])] = names.par.map { name =>
      cleanName(name, mapping) -> chains.map { chain =>
        chain.map { iteration =>
          Try(iteration(name).toDouble).getOrElse(Double.NaN)
        }
      }
    }.toVector.sortBy(_._1)

    def stats: Seq[(String, SummaryStats => Double)] = Seq(
      "Mean" -> (s => s.meanResult),
      "MCSE" -> (s => s.mcseResult),
      "StdDev" -> (s => math.sqrt(s.varianceResult)),
      "5%" -> (s => s.quantile5),
      "50%" -> (s => s.quantile50),
      "95%" -> (s => s.quantile95),
      "N_Eff" -> (s => s.essResult),
      "R_hat" -> (s => s.psrfResult)
    )

    // Compute the statistics.
    val data: Seq[(String, Seq[Double])] = results.par.map { case (name, values) =>
      val summary = SummaryStats(values)
      name -> stats.map(s => s._2.apply(summary))
    }.seq

    pw.println()
    pw.println(s"$chainCount chains with $iterationsPerChain iterations each, $iterationsTotal total")
    pw.println()

    // Write the header.
    val padding = "  "
    val maxNameLength = if (data.nonEmpty) data.map(_._1.length).max else 0
    pw.print("Name" + " " * (maxNameLength - 4) + padding)
    stats.foreach { case (name, _) =>
      val spaceCount = fieldWidth - name.length
      pw.print(" " * spaceCount + name + padding)
    }
    pw.println()

    // Write the data.
    data.foreach { case (name, values) =>
      val namePadding = " " * (maxNameLength + padding.length - name.length)
      pw.print(s"$name$namePadding")
      values.foreach { value =>
        pw.print(String.format(s"%${fieldWidth}.4g", Double.box(value)) + padding)
      }
      pw.println()
    }
    pw.println()

  }
}
