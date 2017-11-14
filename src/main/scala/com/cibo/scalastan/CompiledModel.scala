package com.cibo.scalastan

import java.io.{File, PrintWriter}

case class CompiledModel private[scalastan] (
  private val dir: File,
  private[scalastan] val ss: ScalaStan,
  private val dataMapping: Map[String, DataMapping[_]] = Map.empty
) {

  private[scalastan] def emitData(fileName: String): SHA = {
    val dataFile = new File(s"$dir/$fileName")
    val dataWriter = ShaWriter(new PrintWriter(dataFile))
    ss.dataValues.foreach { value =>
      val mapping = dataMapping.getOrElse(value.emit,
        throw new IllegalStateException(s"no data provided for ${value.emit}")
      )
      dataWriter.println(mapping.emit)
    }
    dataWriter.close()
    dataWriter.sha
  }

  def resultsDirectory: File = dir

  def get[T <: StanType, R](
    decl: StanDataDeclaration[T]
  ): T#SCALA_TYPE = dataMapping(decl.emit).values.asInstanceOf[T#SCALA_TYPE]

  def reset: CompiledModel = copy(dataMapping = Map.empty)

  def withData[T <: StanType, V](
    decl: StanDataDeclaration[T],
    data: V
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = {
    val conv = data.asInstanceOf[T#SCALA_TYPE]

    // Check if this parameter has already been assigned and throw an exception if the values are conflicting.
    dataMapping.get(decl.emit) match {
      case Some(s) if s.values != data =>
        throw new IllegalStateException(s"conflicting values assigned to ${decl.name}")
      case _                           => ()
    }

    // Look up and set the size parameters.
    val (withDecls, _) = decl.typeConstructor.getIndices.foldLeft((this, conv: Any)) { case ((old, d), dim) =>
      val ds = d.asInstanceOf[Seq[_]]
      val next = if (ds.nonEmpty) ds.head else Seq.empty
      dim match {
        case indexDecl: StanDataDeclaration[StanInt] => (old.withData(indexDecl, ds.size), next)
        case _                                       => (old, next)
      }
    }

    // Insert the binding.
    withDecls.copy(dataMapping = withDecls.dataMapping.updated(decl.emit, DataMapping[T](decl, conv)))
  }

  def withData[T <: StanType, V](
    value: (StanDataDeclaration[T], V)
  )(implicit ev: V <:< T#SCALA_TYPE): CompiledModel = withData(value._1, value._2)

  def run(
    chains: Int = 1,
    seed: Int = -1,
    cache: Boolean = true,
    method: RunMethod.Method = RunMethod.Sample()
  )(implicit runner: StanRunner): StanResults = {
    require(chains > 0, s"Must run at least one chain")

    // Make sure all the necessary data is provided.
    ss.dataValues.filterNot(v => dataMapping.contains(v.emit)).foreach { v =>
      throw new IllegalStateException(s"data not supplied for ${v.name}")
    }

    runner.run(
      model = this,
      chains = chains,
      seed = seed,
      cache = cache,
      method = method
    )
  }
}
