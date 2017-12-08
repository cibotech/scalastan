/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.examples

import org.scalatest.{FunSpec, Matchers}

class AppRunnerSpec(app: App) extends FunSpec with Matchers {
  lazy val description: String = app.getClass.getSimpleName.stripSuffix("$")

  describe(description) {
    it("can run the app") {
      noException should be thrownBy app.main(Array.empty)
    }
  }
}
