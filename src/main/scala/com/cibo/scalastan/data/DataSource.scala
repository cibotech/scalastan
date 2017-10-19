package com.cibo.scalastan.data

import com.cibo.scalastan.{DataDeclarationType, StanDeclaration, StanType}

trait DataSource {

  def read[T <: StanType, R](
    decl: StanDeclaration[T, DataDeclarationType],
    name: String
  )(implicit ev: R =:= T#SCALA_TYPE): R

  final def apply[T <: StanType, R](
    decl: StanDeclaration[T, DataDeclarationType],
    name: String
  )(implicit ev: R =:= T#SCALA_TYPE): (StanDeclaration[T, DataDeclarationType], R) = {
    val data: R = read[T, R](decl, name)
    (decl, data)
  }

}
