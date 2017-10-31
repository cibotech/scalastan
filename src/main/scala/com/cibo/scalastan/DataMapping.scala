package com.cibo.scalastan

protected case class DataMapping[T <: StanType] private[scalastan] (
  decl: StanDataDeclaration[T],
  values: T#SCALA_TYPE
) {
  private[scalastan] def emit: String = {
    val nameStr = decl.emit
    val dataStr = decl.typeConstructor.emitData(values.asInstanceOf[decl.typeConstructor.SCALA_TYPE])
    s"$nameStr <- $dataStr"
  }
}
