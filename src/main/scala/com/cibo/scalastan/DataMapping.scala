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

import com.cibo.scalastan.ast.StanDataDeclaration

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
