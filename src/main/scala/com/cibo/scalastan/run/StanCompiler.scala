/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.run

import com.cibo.scalastan.{CompiledModel, StanModel}

trait StanCompiler {
  def compile(model: StanModel): CompiledModel
}

object StanCompiler {

  // Use CmdStan to compile and run the model.
  implicit val defaultCompiler: StanCompiler = CmdStanCompiler
}
