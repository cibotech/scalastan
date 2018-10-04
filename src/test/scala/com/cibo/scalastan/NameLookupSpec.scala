package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}
import scala.language.reflectiveCalls

object StanObjectTop extends StanModel {
  val x = parameter(real())
}

class NameLookupSpec extends FunSpec with Matchers {

  object StanObjectInClass extends StanModel {
    val y = parameter(real())
  }

  describe("NameLookup") {
    it("finds names at the top level") {
      StanObjectTop.x.name shouldBe "x"
    }

    it("finds names in an inner class") {
      StanObjectInClass.y.name shouldBe "y"
    }

    it("finds names in an anonymous class") {
      val anon = new StanModel {
        val z = parameter(real())
      }
      anon.z.name shouldBe "z"
    }

    it("finds names in an object in an anonymous class") {
      class T
      val anon = new T {
        object Test extends StanModel {
          val t = parameter(real())
        }
      }
      anon.Test.t.name shouldBe "t"
    }

    it("finds names in functions") {
      new StanModel {
        def func(): Unit = {
          val a = parameter(real())
          a.name shouldBe "a"
        }
        func()
      }
    }
  }
}
