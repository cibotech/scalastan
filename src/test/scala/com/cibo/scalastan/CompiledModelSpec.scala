package com.cibo.scalastan

import com.cibo.scalastan.run.StanRunner
import org.scalatest.{FunSpec, Matchers}

class CompiledModelSpec extends FunSpec with Matchers {

  object TestStanRunner extends StanRunner {
    def run(model: CompiledModel, chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults = {
      ???
    }
  }

  object model extends StanModel {
    val n = data(int())
    val v = data(vector(n))
    val r = parameter(real())
    val p = parameter(vector(n))

    v ~ stan.normal(p *:* v, r)
  }

  describe("withData") {
    it("set scalars") {
      val cm = CompiledModel(model, TestStanRunner).withData(model.n, 5)
      cm.dataMapping.size shouldBe 1
      cm.dataMapping(model.n.emit).values shouldBe 5
    }

    it("updates bounds") {
      val cm = CompiledModel(model, TestStanRunner).withData(model.v, Seq(1.0, 2.0))
      cm.dataMapping.size shouldBe 2
      cm.dataMapping(model.n.emit).values shouldBe 2
    }
  }

  describe("withInitialValue(param, value)") {
    it("set scalars") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(model.r, 4.0)
      val mapping = cm.initialValue.asInstanceOf[InitialValueMapping].mapping
      mapping.size shouldBe 1
      mapping(model.r.emit).values shouldBe 4.0
    }

    it("updates bounds") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(model.p, Seq(1.0, 2.0))
      val mapping = cm.initialValue.asInstanceOf[InitialValueMapping].mapping
      mapping.size shouldBe 1
      mapping(model.p.emit).values shouldBe Seq(1.0, 2.0)
      cm.dataMapping(model.n.emit).values shouldBe 2
    }

    it("throws an exception if the initial value is already set") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(1.0)
      an[IllegalStateException] should be thrownBy cm.withInitialValue(model.r, 5.0)
    }

    it("is cleared by a reset") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(model.p, Seq(1.0, 2.0)).reset
      cm.withInitialValue(model.r, 5.0).initialValue.asInstanceOf[InitialValueMapping].mapping.size shouldBe 1
    }
  }

  describe("withInitialValue(value)") {
    it("updates the initial value") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(4.0)
      cm.initialValue.asInstanceOf[InitialValueDouble].v shouldBe 4.0
    }

    it("throws an exception if the initial value is already set") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(4.0)
      an[IllegalStateException] should be thrownBy cm.withInitialValue(1.0)
    }

    it("is cleared by a reset") {
      val cm = CompiledModel(model, TestStanRunner).withInitialValue(1.0).reset
      cm.withInitialValue(5.0).initialValue.asInstanceOf[InitialValueDouble].v shouldBe 5.0
    }
  }
}
