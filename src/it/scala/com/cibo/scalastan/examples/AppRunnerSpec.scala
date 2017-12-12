package com.cibo.scalastan.examples

import org.scalatest.FunSpec

class AppRunnerSpec(app: App) extends FunSpec {

  private lazy val description = app.getClass.getSimpleName.stripSuffix("$")

  describe(description) {
    it("can run the app") {
      app.main(Array.empty)
    }
  }
}
