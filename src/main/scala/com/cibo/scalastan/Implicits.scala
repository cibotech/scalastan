package com.cibo.scalastan

import scala.language.implicitConversions

protected trait Implicits {
  implicit def intLiteral2int(value: Int): StanValue[StanInt] = StanConstant[StanInt](value)

  implicit def doubleLiteral2real(value: Double): StanValue[StanReal] = StanConstant[StanReal](value)

  implicit def stringLiteral2string(value: String): StanValue[StanString] = StanStringLiteral(value)

  implicit def intLiteral2optInt(value: Int): Option[StanValue[StanInt]] = Some(StanConstant[StanInt](value))

  implicit def intLiteral2optReal(value: Int): Option[StanValue[StanReal]] = Some(StanConstant[StanReal](value.toDouble))

  implicit def doubleLiteral2optReal(value: Double): Option[StanValue[StanReal]] = Some(StanConstant[StanReal](value))

  implicit def stanValue2optValue[T <: StanType](value: StanValue[T]): Option[StanValue[T]] = Some(value)
}

protected object Implicits extends Implicits
