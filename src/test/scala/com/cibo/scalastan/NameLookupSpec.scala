package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}
import scala.language.reflectiveCalls

object StanObjectTop extends ScalaStan {
  val x = parameter(real())
}

class NameLookupSpec extends FunSpec with Matchers {

  object StanObjectInClass extends ScalaStan {
    val y = parameter(real())
  }

  describe("NameLookup") {
    it("finds names at the top level") {
      StanObjectTop.x.name shouldBe "x"
    }

    it("finds names in an inner class") {
      StanObjectInClass.y.name shouldBe "y"
    }

    ignore("finds names in an anonymous class") {
      val anon = new ScalaStan {
        val z = parameter(real())
      }
      anon.z.name shouldBe "z"
    }

    ignore("finds names in an object in an anonymous class") {
      class T
      val anon = new T {
        object Test extends ScalaStan {
          val t = parameter(real())
        }
      }
      anon.Test.t.name shouldBe "t"
    }
  }
}
