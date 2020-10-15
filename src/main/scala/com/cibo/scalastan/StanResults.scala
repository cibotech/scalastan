/*
 * Copyright (c) 2017 - 2020 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.{PrintStream, PrintWriter}

import com.cibo.scalastan.ast.{StanDataDeclaration, StanParameterDeclaration}

import scala.util.Try

case class StanResults(
  parameterChains: Map[String, Vector[Vector[Double]]],
  massMatrix: Vector[Vector[Vector[Double]]],
  model: CompiledModel,
  method: RunMethod.Method
) {

  require(parameterChains.nonEmpty, "No results")

  private val lpName = "lp__"
  private val divergentName = "divergent__"
  private val treeDepthName = "treedepth__"
  private val energyName = "energy__"
  private val acceptName = "accept_stat__"
  private val stepSizeName = "stepsize__"

  lazy val bestChain: Int = parameterChains(lpName).zipWithIndex.maxBy(_._1.max)._2
  lazy val bestIndex: Int = parameterChains(lpName)(bestChain).zipWithIndex.maxBy(_._1)._2

  private def extract(name: String): Seq[Seq[Double]] = parameterChains(name)
  private def extractOption(name: String): Seq[Seq[Double]] = parameterChains.getOrElse(name, Seq.empty)

  lazy val logPosterior: Seq[Seq[Double]] = extract(lpName)
  lazy val divergent: Seq[Seq[Double]] = extractOption(divergentName)
  lazy val treeDepth: Seq[Seq[Double]] = extractOption(treeDepthName)
  lazy val energy: Seq[Seq[Double]] = extractOption(energyName)
  lazy val acceptStat: Seq[Seq[Double]] = extractOption(acceptName)
  lazy val stepSize: Seq[Seq[Double]] = extractOption(stepSizeName)

  val chainCount: Int = parameterChains.head._2.size
  val iterationsPerChain: Int = parameterChains.head._2.head.size
  val iterationsTotal: Int = chainCount * iterationsPerChain

  /** Get all parameter declarations. */
  def parameters: Seq[StanParameterDeclaration[_ <: StanType]] = model.model.program.parameters

  /** Get all transformed parameters. */
  def transformedParameters: Seq[StanParameterDeclaration[_ <: StanType]] =
    model.model.program.transformedParameters.map(_.result)

  /** Get all generated quantities. */
  def generatedQuantities: Seq[StanParameterDeclaration[_ <: StanType]] =
    model.model.program.generatedQuantities.map(_.result)

  /** Get all data declarations. */
  def data: Seq[StanDataDeclaration[_ <: StanType]] = model.model.program.data

  private def declName(decl: StanParameterDeclaration[_ <: StanType]): String = {
    decl.root.emit + (if (decl.indices.nonEmpty) decl.indices.mkString(".", ".", "") else "")
  }

  private def elementsHelper(
    decl: StanParameterDeclaration[_ <: StanType],
    value: Any,
    indices: Vector[Int] = Vector.empty
  ): Seq[StanParameterDeclaration[StanReal]] = {
    value match {
      case vs: Seq[_] => vs.zipWithIndex.flatMap(v => elementsHelper(decl, v._1, indices :+ (v._2 + 1)))
      case x          =>
        val newName = if (indices.nonEmpty) indices.mkString(s"${decl.name}[", ",", "]") else decl.name
        val newReturnType: StanType = indices.foldLeft(decl.returnType: StanType) { (t, _) => t.next }

        // Parameters always have "real" elements.
        val realReturnType: StanReal = newReturnType.asInstanceOf[StanReal]
        Seq(StanParameterDeclaration(realReturnType, newName, decl.rootOpt.orElse(Some(decl)), decl.indices ++ indices))
    }
  }

  /** Get scalar elements for a parameter declaration. */
  def elements(decl: StanParameterDeclaration[_ <: StanType]): Seq[StanParameterDeclaration[StanReal]] = {
    val name = declName(decl)
    val parsed = decl.returnType.parse(name, parameterChains).head.head
    elementsHelper(decl, parsed)
  }

  /** Get the specified input data. */
  def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = model.get(decl)

  /** Get all samples by chain and iteration. */
  def samples[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SCALA_TYPE): Seq[Seq[R]] = {
    val name = declName(decl)
    decl.returnType.parse(name, parameterChains).asInstanceOf[Seq[Seq[R]]]
  }

  /** Get the best scoring sample. */
  def best(values: Seq[Seq[Double]]): Double = values.apply(bestChain).apply(bestIndex)

  /** Get the best scoring sample. */
  def best[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SCALA_TYPE): R =
    samples(decl).apply(bestChain).apply(bestIndex)

  private def combine[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(
    func: Seq[Seq[Double]] => Double
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    val tc = decl.returnType
    tc.combine(samples(decl).asInstanceOf[Seq[Seq[tc.SCALA_TYPE]]])(func).asInstanceOf[R]
  }

  private def meanAndVariance1d(values: Seq[Double]): (Double, Double) = {
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

  private def mean1d(values: Seq[Double]): Double = {
    var n = 0.0
    var result = 0.0
    values.foreach { x =>
      n += 1.0
      val delta = x - result
      result += delta / n
    }
    result
  }

  private def variance1d(values: Seq[Double]): Double = meanAndVariance1d(values)._2

  /** Get the mean of all samples across all chains and iterations. */
  def mean(values: Seq[Seq[Double]]): Double = mean1d(values.flatten)

  /** Get the mean of all samples across all chains and iterations. */
  def mean[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(mean)

  private def mcse(v: Double, ess: Double): Double = math.sqrt(v) / math.sqrt(ess)

  /** Get the MCSE of a all samples across all chains and iterations. */
  def mcse(values: Seq[Seq[Double]]): Double = mcse(variance(values), effectiveSampleSize(values))

  /** Get the MCSE of a all samples across all chains and iterations. */
  def mcse[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(mcse)

  /** Get the variance of a all samples across all chains and iterations. */
  def variance(values: Seq[Seq[Double]]): Double = variance1d(values.flatten)

  /** Get the variance of a all samples across all chains and iterations. */
  def variance[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(variance)

  /** Get the standard deviation of a all samples across all chains and iterations. */
  def sd(values: Seq[Seq[Double]]): Double = math.sqrt(variance(values))

  /** Get the standard deviation of a all samples across all chains and iterations. */
  def sd[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(sd)

  private def quantileSorted(frac: Double)(sortedValues: Seq[Double]): Double = {
    val index = math.min((sortedValues.size.toDouble * frac).round.toInt, sortedValues.size - 1)
    sortedValues(index)
  }

  /** Get the specified quantile of all samples across all chains and iterations. */
  def quantile(values: Seq[Seq[Double]], prob: Double): Double = {
    require(prob > 0.0 && prob < 1.0)
    quantileSorted(prob)(values.flatten.sorted)
  }

  /** Get the specified quantile of all samples across all chains and iterations. */
  def quantile[T <: StanType, R](
    decl: StanParameterDeclaration[T],
    prob: Double = 0.5
  )(implicit ev: R =:= T#SUMMARY_TYPE): R = {
    require(prob > 0.0 && prob < 1.0)
    combine(decl)(vs => quantile(vs, prob))
  }

  /** Get the minimum of all samples across all chains and iterations. */
  def min(values: Seq[Seq[Double]]): Double = values.flatten.min

  /** Get the minimum of all samples across all chains and iterations. */
  def min[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(min)

  /** Get the maximum of all samples across all chains and iterations. */
  def max(values: Seq[Seq[Double]]): Double = values.flatten.max

  /** Get the maximum of all samples across all chains and iterations. */
  def max[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(max)

  private def autocovariance(k: Int, x: Seq[Double]): Double = {
    val n = x.size
    val xMean = mean1d(x)
    mean1d {
      (0 until n - k).map { i =>
        (x(i + k) - xMean) * (x(i) - xMean)
      }
    }
  }

  private def effectiveSampleSize(values: Seq[Seq[Double]]): Double = {
    val n = values.head.size // Samples per chain
    val m = values.size // Number of chains
    val chainMeansAndVars = values.map(meanAndVariance1d)
    val chainMeans = chainMeansAndVars.map(_._1)
    val meanVar = mean1d(chainMeansAndVars.map(_._2))
    val varPlus = meanVar * (n - 1) / n + (if (m > 1) variance1d(chainMeans) else 0.0)
    val rho = (1 until n).view.map { t =>
      val acov_t = values.map(vs => autocovariance(t, vs))
      1.0 - (meanVar - mean1d(acov_t)) / varPlus
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
    val totalMean = mean(values)
    (n / (m - 1.0)) * values.map { vs =>
      math.pow(mean1d(vs) - totalMean, 2)
    }.sum
  }

  private def withinSampleVariance(values: Seq[Seq[Double]]): Double = {
    val m = values.size.toDouble
    values.map(variance1d).sum / m
  }

  /** Get the (split) potential scale reduction factor for a set of chains. */
  def psrf(values: Seq[Seq[Double]]): Double = {
    // Split into 2m chains of length n/2
    val groupSize = (values.head.size + 1) / 2
    val groupedValues = values.flatMap(_.grouped(groupSize))
    val n = groupedValues.head.size.toDouble
    val w = withinSampleVariance(groupedValues)
    val b = betweenSampleVariance(groupedValues)
    val varPlus = ((n - 1.0) * w + b) / n
    math.sqrt(varPlus / w)
  }

  /** Get the (split) potential scale reduction factor for a parameter. */
  def psrf[T <: StanType, R](decl: StanParameterDeclaration[T])(implicit ev: R =:= T#SUMMARY_TYPE): R =
    combine(decl)(psrf)

  private def cleanName(name: String, mapping: Map[String, String]): Option[String] = {
    if (name.endsWith("__")) {
      Some(name)
    } else {
      val parts = name.split('.')
      mapping.get(parts.head).map { first =>
        if (parts.tail.nonEmpty) {
          val indices = parts.tail.mkString("[", ",", "]")
          s"$first$indices"
        } else {
          first
        }
      }
    }
  }

  /** Get the number of iterations that saturated the maximum tree depth. */
  def checkTreeDepth: Int = {
    val maxDepth = method match {
      case sample: RunMethod.Sample =>
        sample.algorithm match {
          case hmc: RunMethod.Hmc =>
            hmc.engine match {
              case nuts: RunMethod.Nuts => nuts.maxDepth
              case _ => 0
            }
          case _ => 0
        }
      case _ => 0
    }
    treeDepth.map(_.count(_ > maxDepth)).sum
  }

  /** Get the number of iterations below the specified energy threshold. */
  def checkEnergy(threshold: Double = 0.2): Int = energy.map(_.count(_ < threshold)).sum

  /** Get the total number of divergences after warmup. */
  def checkDivergence: Int = divergent.map(_.count(_ > 0.0)).sum

  /** Partition iterations into (divergent, non-divergent) samples. */
  def partitionByDivergence[T <: StanType, R](
    decl: StanParameterDeclaration[T]
  )(implicit ev: R =:= T#SCALA_TYPE): (Seq[R], Seq[R]) = {
    val partitioned = samples(decl).flatten.zip(divergent.flatten).partition { case (_, d) => d > 0.0 }
    (partitioned._1.map(_._1), partitioned._2.map(_._1))
  }

  def summary(ps: PrintStream, parameters: StanParameterDeclaration[_]*): Unit = {
    val pw = new PrintWriter(ps)
    summary(pw, parameters: _*)
    pw.flush()
  }

  private case class SummaryStats(values: Seq[Seq[Double]]) {
    private val flattened = values.flatten
    private val sortedValues = flattened.sorted
    val (meanResult, varianceResult) = meanAndVariance1d(flattened)
    val essResult: Double = effectiveSampleSize(values)
    val mcseResult: Double = mcse(varianceResult, essResult)
    val quantile5: Double = quantileSorted(0.05)(sortedValues)
    val quantile50: Double = quantileSorted(0.5)(sortedValues)
    val quantile95: Double = quantileSorted(0.95)(sortedValues)
    val psrfResult: Double = psrf(values)
  }

  def summary(pw: PrintWriter, parameters: StanParameterDeclaration[_]*): Unit = {

    val fieldWidth = 8

    // Get a mapping from Stan name to ScalaStan name.
    val parametersToShow = if (parameters.nonEmpty) parameters else this.parameters ++ this.transformedParameters
    val mapping = parametersToShow.map(p => p.emit -> p.name).toMap

    // Build a mapping of name -> chain -> iteration -> value
    val names = parameterChains.keys
    val results: Seq[(String, Seq[Seq[Double]])] = names.par.flatMap { name =>
      cleanName(name, mapping).map { cleanedName =>
        cleanedName -> parameterChains(name)
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
