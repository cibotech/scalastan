package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, ScalaStanBaseSpec}

class CopyPropagationSpec extends ScalaStanBaseSpec {
  describe("CopyPropagation") {
    it("handles a simple case") {
      object Test extends ScalaStan {
        val model = new Model {
          val x = local(real())
          val y = local(real())
          val z = local(real())

          y := z
          x := y
        }
        val updated = model.transform(CopyPropagation())
      }
      checkCode(Test.updated, "model { real z; real x; x = z; }")
    }
  }
}
