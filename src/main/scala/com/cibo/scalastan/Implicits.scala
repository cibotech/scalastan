/*
 * Copyright (c) 2017 - 2021 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.cibo.scalastan.ast.{StanArrayLiteral, StanConstant, StanStringLiteral, StanValue}

import scala.language.implicitConversions

protected trait Implicits {
  implicit def intLiteral2int(value: Int): StanConstant[StanInt] = StanConstant[StanInt](StanInt(), value)

  implicit def doubleLiteral2real(value: Double): StanConstant[StanReal] = StanConstant[StanReal](StanReal(), value)

  implicit def stringLiteral2string(value: String): StanValue[StanString] = StanStringLiteral(value)

  implicit def intLiteral2optInt(value: Int): Option[StanConstant[StanInt]] =
    Some(StanConstant[StanInt](StanInt(), value))

  implicit def intLiteral2optReal(value: Int): Option[StanConstant[StanReal]] =
    Some(StanConstant[StanReal](StanReal(), value.toDouble))

  implicit def doubleLiteral2optReal(value: Double): Option[StanConstant[StanReal]] =
    Some(StanConstant[StanReal](StanReal(), value))

  implicit def seq2array[T <: StanType](values: Seq[StanValue[T]]): StanValue[StanArray[T]] =
    StanArrayLiteral[T, StanArray[T]](values)
}

protected object Implicits extends Implicits
