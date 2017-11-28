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

import scala.language.implicitConversions

protected trait Implicits {
  implicit def intLiteral2int(value: Int): StanConstant[StanInt] = StanConstant[StanInt](value)

  implicit def doubleLiteral2real(value: Double): StanConstant[StanReal] = StanConstant[StanReal](value)

  implicit def stringLiteral2string(value: String): StanValue[StanString] = StanStringLiteral(value)

  implicit def intLiteral2optInt(value: Int): Option[StanConstant[StanInt]] = Some(StanConstant[StanInt](value))

  implicit def intLiteral2optReal(value: Int): Option[StanConstant[StanReal]] = Some(StanConstant[StanReal](value.toDouble))

  implicit def doubleLiteral2optReal(value: Double): Option[StanConstant[StanReal]] = Some(StanConstant[StanReal](value))

  implicit def stanValue2optValue[T <: StanType](value: StanValue[T]): Option[StanValue[T]] = Some(value)
}

protected object Implicits extends Implicits
