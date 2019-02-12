package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStanBaseSpec, StanModel}

class CSESpec extends ScalaStanBaseSpec {
  describe("CSE") {
    it("combines common expressions") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())
        x := y + 1
        z := y + 1
      }

      checkCode(model.transform(CSE()), "model { real x; real y; real z; x = (y + 1); z = x; }")
    }

    it("re-orders multiplication") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())
        x := y * 1
        z := 1 * y
      }

      checkCode(model.transform(CSE()), "model { real x; real y; real z; x = (y * 1); z = x; }")
    }

    it("does not re-order division") {
      val model = new StanModel {
        val x = local(real())
        val y = local(real())
        val z = local(real())
        x := y / 1
        z := 1 / y
      }

      checkCode(model.transform(CSE()), "model { real x; real y; real z; x = (y / 1); z = (1 / y); }")
    }
  }
}
