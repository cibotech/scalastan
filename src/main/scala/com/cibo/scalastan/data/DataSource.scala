package com.cibo.scalastan.data

import com.cibo.scalastan.{StanDataDeclaration, StanType}

trait DataSource {
  def read[T <: StanType, R](
    decl: StanDataDeclaration[T],
    name: String = ""
  )(implicit ev: R =:= T#SCALA_TYPE): R

  final def apply[T <: StanType, R](
    decl: StanDataDeclaration[T],
    name: String = ""
  )(implicit ev: R =:= T#SCALA_TYPE): (StanDataDeclaration[T], R) = {
    val data: R = read[T, R](decl, name)
    (decl, data)
  }
}
