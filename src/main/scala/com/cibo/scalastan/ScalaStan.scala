/*
 * Copyright (c) 2017 - 2019 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.cibo.scalastan.ast._
import com.typesafe.scalalogging.LazyLogging

@deprecated("Use StanModel", "2018-10-02")
trait ScalaStan extends StanModel with LazyLogging { ss =>

  trait Model extends StanModel {
    override implicit val _context: StanContext = ss._context
  }

  val Model: StanModel.type = StanModel
}
