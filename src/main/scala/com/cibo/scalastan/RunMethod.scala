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

object RunMethod {

  sealed abstract class Subcommand(name: String) {
    protected val subValue: Option[String] = None
    def arguments: Seq[String]
    protected def build(options: (String, Any, Any)*): Seq[String] = {
      val configuredOptions: Seq[String] = options.flatMap { case (optName, optValue, defaultValue) =>
        optValue match {
          case v if v == defaultValue => Seq.empty
          case sc: Subcommand         => sc.arguments
          case v: Boolean             => Seq(if (v) s"$optName=1" else s"$optName=0")
          case v                      => Seq(s"$optName=$v")
        }
      }
      subValue.map(v => s"$name=$v").getOrElse(name) +: configuredOptions
    }
  }

  sealed abstract class Method(_name: String) extends Subcommand(_name)
  case class Sample(
    samples: Int = 1000,
    warmup: Int = 1000,
    saveWarmup: Boolean = false,
    thin: Int = 1,
    adapt: SampleAdapt = SampleAdapt(),
    algorithm: SampleAlgorithm = Hmc()
  ) extends Method("sample") {
    def arguments: Seq[String] = build(
      ("num_samples", samples, Sample().samples),
      ("num_warmup", warmup, Sample().warmup),
      ("save_warmup", saveWarmup, Sample().saveWarmup),
      ("thin", thin, Sample().thin),
      ("adapt", adapt, Sample().adapt),
      ("algorithm", algorithm, Sample().algorithm)
    )
  }
  case class Optimize(
    algorithm: OptimizeAlgorithm = LBFGS(),
    iter: Int = 2000,
    saveIterations: Boolean = false
  ) extends Method("optimize") {
    require(iter > 0, s"iter out of range: $iter")
    def arguments: Seq[String] = build(
      ("algorithm", algorithm, Optimize().algorithm),
      ("iter", iter, Optimize().iter),
      ("save_iterations", saveIterations, Optimize().saveIterations)
    )
  }
  case class Variational(
    algorithm: VariationalAlgorithm = Meanfield,
    iter: Int = 10000,
    gradSamples: Int = 1,
    elboSamples: Int = 100,
    eta: Double = 1.0,
    adapt: VariationalAdapt = VariationalAdapt()
  ) extends Method("variational") {
    require(iter > 0, s"iter out of range: $iter")
    require(gradSamples > 0, s"gradSamples out of range: $gradSamples")
    require(elboSamples > 0, s"elboSamples out of range: $elboSamples")
    require(eta > 0.0, s"eta out of range: $eta")
    def arguments: Seq[String] = build(
      ("algorithm", algorithm, Variational().algorithm),
      ("iter", iter, Variational().iter),
      ("grad_samples", gradSamples, Variational().gradSamples),
      ("elbo_samples", elboSamples, Variational().elboSamples),
      ("eta", eta, Variational().eta)
    )
  }
  case class Diagnose(
  ) extends Method("diagnose") {
    def arguments: Seq[String] = build() // TODO
  }

  case class SampleAdapt(
    engaged: Boolean = true,
    gamma: Double = 0.05,
    delta: Double = 0.8,
    kappa: Double = 0.75,
    t0: Double = 10,
    initBuffer: Int = 75,
    termBuffer: Int = 50,
    window: Int = 25
  ) extends Subcommand("adapt") {
    require(gamma > 0.0, s"gamma out of range: $gamma")
    require(delta > 0.0 && delta < 1.0, s"delta out of range: $delta")
    require(kappa > 0.0, s"kappa out of range: $kappa")
    require(t0 > 0.0, s"t0 out of range: $t0")
    require(initBuffer > 0, s"initBuffer out of range: $initBuffer")
    require(termBuffer > 0, s"termBuffer out of range: $termBuffer")
    require(window > 0, s"window out of range: $window")

    def arguments: Seq[String] = build(
      ("engaged", engaged, SampleAdapt().engaged),
      ("gamma", gamma, SampleAdapt().gamma),
      ("delta", delta, SampleAdapt().delta),
      ("kappa", kappa, SampleAdapt().kappa),
      ("t0", t0, SampleAdapt().t0),
      ("init_buffer", initBuffer, SampleAdapt().initBuffer),
      ("term_buffer", termBuffer, SampleAdapt().termBuffer),
      ("window", window, SampleAdapt().window)
    )
  }

  case class VariationalAdapt(
    engaged: Boolean = true,
    iter: Int = 50,
    tolRelObj: Double = 0.01,
    evalElbo: Int = 100,
    outputSamples: Int = 1000
  ) extends Subcommand("adapt") {
    require(iter > 0, s"iter out of range $iter")
    require(tolRelObj > .00, s"tolRelObj out of range $tolRelObj")
    require(evalElbo > 0, s"evalElbo out of range $evalElbo")
    require(outputSamples > 0, s"outputSamples out of range $outputSamples")
    def  arguments: Seq[String] = build(
      ("engaged", engaged, VariationalAdapt().engaged),
      ("iter", iter, VariationalAdapt().iter),
      ("tol_rel_obj", tolRelObj, VariationalAdapt().tolRelObj),
      ("eval_elbo", evalElbo, VariationalAdapt().evalElbo),
      ("output_samples", outputSamples, VariationalAdapt().outputSamples)
    )
  }

  abstract class SampleAlgorithm(value: String) extends Subcommand("algorithm") {
    override protected val subValue: Option[String] = Some(value)
  }
  case class Hmc(
    engine: Engine = Nuts(),
    metric: Metric = DiagE,
    stepsize: Int = 1,
    stepsizeJitter: Int = 0
  ) extends SampleAlgorithm("hmc") {
    def arguments: Seq[String] = build(
      ("engine", engine, Hmc().engine),
      ("metric", metric, Hmc().metric),
      ("stepsize", stepsize, Hmc().stepsize),
      ("stepsize_jitter", stepsizeJitter, Hmc().stepsizeJitter)
    )
  }
  case class FixedParam() extends SampleAlgorithm("fixed_param") {
    def arguments: Seq[String] = build()
  }

  sealed abstract class Metric(name: String) {
    override def toString: String = name
  }
  case object UnitE extends Metric("unit_e")
  case object DiagE extends Metric("diag_e")
  case object DenseE extends Metric("dense_e")

  abstract class Engine(value: String) extends Subcommand("engine") {
    override protected val subValue: Option[String] = Some(value)
  }
  case class Static(
    intTime: Double = 2.0 * math.Pi
  ) extends Engine("static") {
    def arguments: Seq[String] = build(
      ("int_time", intTime, Static().intTime)
    )
  }
  case class Nuts(
    maxDepth: Int = 10
  ) extends Engine("nuts") {
    require(maxDepth > 0, s"maxDepth out of range: $maxDepth")
    def arguments: Seq[String] = build(
      ("max_depth", maxDepth, Nuts().maxDepth)
    )
  }

  abstract class OptimizeAlgorithm(value: String) extends Subcommand("algorithm") {
    override protected val subValue: Option[String] = Some(value)
  }
  case class BFGS(
    initAlpha: Double = 0.001,
    tolObj: Double = 1e-12,
    tolRelObj: Double = 1e4,
    tolGrad: Double = 1e-8,
    tolRelGrad: Double = 1e7,
    tolParam: Double = 1e-8
  ) extends OptimizeAlgorithm("bfgs") {
    require(initAlpha >= 0.0, s"initAlpha out of range: $initAlpha")
    require(tolObj >= 0.0, s"tolObj out of range: $tolObj")
    require(tolRelObj >= 0.0, s"tolRelObj out of range: $tolRelObj")
    require(tolGrad >= 0.0, s"tolGrad out of range: $tolGrad")
    require(tolRelGrad >= 0.0, s"tolRelGrad out of range: $tolRelGrad")
    require(tolParam >= 0.0, s"tolParam out of range: $tolParam")
    def arguments: Seq[String] = build(
      ("init_alpha", initAlpha, BFGS().initAlpha),
      ("tol_obj", tolObj, BFGS().tolObj),
      ("tol_rel_obj", tolRelObj, BFGS().tolRelObj),
      ("tol_grad", tolGrad, BFGS().tolGrad),
      ("tol_rel_grad", tolRelGrad, BFGS().tolRelGrad),
      ("tol_param", tolParam, BFGS().tolParam)
    )
  }
  case class LBFGS(
    initAlpha: Double = 0.001,
    tolObj: Double = 1e-12,
    tolRelObj: Double = 1e4,
    tolGrad: Double = 1e-8,
    tolRelGrad: Double = 1e7,
    tolParam: Double = 1e-8,
    historySize: Int = 5
  ) extends OptimizeAlgorithm("lbfgs") {
    require(initAlpha >= 0.0, s"initAlpha out of range: $initAlpha")
    require(tolObj >= 0.0, s"tolObj out of range: $tolObj")
    require(tolRelObj >= 0.0, s"tolRelObj out of range: $tolRelObj")
    require(tolGrad >= 0.0, s"tolGrad out of range: $tolGrad")
    require(tolRelGrad >= 0.0, s"tolRelGrad out of range: $tolRelGrad")
    require(tolParam >= 0.0, s"tolParam out of range: $tolParam")
    require(historySize > 0, s"historySize out of range: $historySize")
    def arguments: Seq[String] = build(
      ("init_alpha", initAlpha, LBFGS().initAlpha),
      ("tol_obj", tolObj, LBFGS().tolObj),
      ("tol_rel_obj", tolRelObj, LBFGS().tolRelObj),
      ("tol_grad", tolGrad, LBFGS().tolGrad),
      ("tol_rel_grad", tolRelGrad, LBFGS().tolRelGrad),
      ("tol_param", tolParam, LBFGS().tolParam),
      ("history_size", historySize, LBFGS().historySize)
    )
  }
  case class Newton() extends OptimizeAlgorithm("newton") {
    def arguments: Seq[String] = Seq.empty
  }

  sealed abstract class VariationalAlgorithm(name: String) {
    override def toString: String = name
  }
  case object Meanfield extends VariationalAlgorithm("meanfield")
  case object Fullrank extends VariationalAlgorithm("fullrank")
}
