package com.cibo.scalastan.transform

import com.cibo.scalastan.analysis.UseDefinitions
import com.cibo.scalastan.{ScalaStan, ScalaStanBaseSpec}

class UseDefinitionsSpec extends ScalaStanBaseSpec {
  describe("UseDefinitions") {
    it("finds uses and definitions for a simple example") {
      new ScalaStan {

        object model extends Model {
          val x = local(real())
          val y = local(real())
          val z = local(real())

          val a1 = x := y
          val a2 = z := x
          val a3 = y := z
        }

        // Look up the definition for a use.
        val ud = UseDefinitions(model.program.model).ud
        ud.get(model.a1.id) shouldBe None         // No definition available to the first assignment.
        ud(model.a2.id) shouldBe Set(model.a1.id)  // Definition for x available
        ud(model.a3.id) shouldBe Set(model.a2.id)  // Definition for z available.

        // Look up the use for a definition.
        val du = UseDefinitions(model.program.model).du
        du(model.a1.id) shouldBe Set(model.a2.id)
        du(model.a2.id) shouldBe Set(model.a3.id)
      }
    }
  }
}
