package com.cibo.scalastan.transform

import com.cibo.scalastan.{StanModel, ScalaStanBaseSpec}

class CopyPropagationSpec extends ScalaStanBaseSpec {
  describe("CopyPropagation") {
    it("handles a simple case") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())

        y := z
        x := y
      }
      checkCode(model.transform(CopyPropagation()), "model { real z; real x; x = z; }")
    }

    it("handles conditionals") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())

        y := z
        when(1) {
          x := y
        }
      }
      checkCode(model.transform(CopyPropagation()), "model { real z; real x; if(1) { x = z; } }")
    }

    it("is conservative") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())

        y := z
        when(1) {
          y := 3
        }
        x := y
      }
      checkCode(model.transform(CopyPropagation()), "model { real z; real y; real x; y = z; if(1) { y = 3; } x = y; }")
    }
  }
}
