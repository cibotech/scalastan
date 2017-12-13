package com.cibo.scalastan.examples

import org.scalatest.FunSpec

import scala.util.{Failure, Success, Try}

class AppRunnerSpec(app: App) extends FunSpec {

  private lazy val description = app.getClass.getSimpleName.stripSuffix("$")

  describe(description) {
    it("can run the app") {
      Try(app.main(Array.empty)) match {
        case Success(_) => ()
        case Failure(t) =>
          t.printStackTrace()
          fail(t)
      }
    }
  }
}
