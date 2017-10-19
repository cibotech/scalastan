package com.cibo.scalastan

sealed abstract class RunMethod(name: String) {
  def arguments: Seq[String]

  protected def buildCommand(options: (String, Option[_])*): Seq[String] = {
    val configuredOptions = options.flatMap { case (optName, valueOpt) =>
      valueOpt.map {
        case b: Boolean => if (b) "1" else "0"
        case v          => v.toString
      }.map(v => s"$optName=$v")
    }
    name +: configuredOptions
  }

}

case class SampleMethod(
  samples: Option[Int] = None,
  warmup: Option[Int] = None,
  saveWarmup: Option[Boolean] = None,
  thin: Option[Int] = None,
  adaptEngaged: Option[Boolean] = None,
  adaptGamma: Option[Double] = None
) extends RunMethod("sample") {
  def arguments: Seq[String] = buildCommand(
    "num_samples" -> samples,
    "num_warmup" -> warmup,
    "save_warmup" -> saveWarmup,
    "thin" -> thin,
    "adapt engaged" -> adaptEngaged,
    "adapt gamma" -> adaptGamma
  )
}

case class OptimizeMethod(
) extends RunMethod("optimize") {
  def arguments: Seq[String] = buildCommand(
  )
}

case class VariationalMethod(
) extends RunMethod("variational") {
  def arguments: Seq[String] = buildCommand(
  )
}

case class DiagnoseMethod(

) extends RunMethod("diagnose") {
  def arguments: Seq[String] = buildCommand(
  )
}
