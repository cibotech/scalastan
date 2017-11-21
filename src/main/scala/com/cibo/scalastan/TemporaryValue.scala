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

private sealed trait TemporaryValue[T <: StanType] {
  def create(args: StanValue[StanInt]*): T
}

private object TemporaryValue {

  implicit val intTemporary: TemporaryValue[StanInt] = new TemporaryValue[StanInt] {
    def create(args: StanValue[StanInt]*): StanInt = StanInt()
  }

  implicit val vectorTemporary: TemporaryValue[StanVector] = new TemporaryValue[StanVector] {
    def create(args: StanValue[StanInt]*): StanVector = StanVector(args(0))
  }
}
