package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, ScalaStanBaseSpec}

class CSESpec extends ScalaStanBaseSpec {
  describe("CSE") {
    it("combines common expressions") {
      object Test extends ScalaStan {
        val model = new Model {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y + 1
          z := y + 1
        }
        val updated = model.transform(CSE())
      }

      checkCode(Test.updated, "model { real z; real y; real x; x = (y) + (1); z = x; }")
    }

    it("re-orders multiplication") {
      object Test extends ScalaStan {
        val model = new Model {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y * 1
          z := 1 * y
        }
        val updated = model.transform(CSE())
      }

      checkCode(Test.updated, "model { real z; real y; real x; x = (y) * (1); z = x; }")
    }

    it("does not re-order division") {
      object Test extends ScalaStan {
        val model = new Model {
          val x = local(real())
          val y = local(real())
          val z = local(real())
          x := y / 1
          z := 1 / y
        }
        val updated = model.transform(CSE())
      }

      checkCode(Test.updated, "model { real z; real y; real x; x = (y) / (1); z = (1) / (y); }")
    }
  }
}
