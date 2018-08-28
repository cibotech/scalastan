package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}

class CompiledModelSpec extends FunSpec with Matchers with ScalaStan {

  case class TestCompiledModel(
    model: ScalaStan#Model,
    dataMapping: Map[String, DataMapping[_]] = Map.empty,
    initialValue: InitialValue = DefaultInitialValue
  ) extends CompiledModel {
    protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CompiledModel =
      copy(dataMapping = newMapping)
    protected def replaceInitialValue(newInitialValue: InitialValue): CompiledModel =
      copy(initialValue = newInitialValue)
    protected def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults = ???
  }

  val n = data(int())
  val v = data(vector(n))
  val r = parameter(real())
  val p = parameter(vector(n))

  val model = new Model {
    v ~ stan.normal(p *:* v, r)
  }

  describe("withData") {
    it("set scalars") {
      val cm = TestCompiledModel(model).withData(n, 5)
      cm.dataMapping.size shouldBe 1
      cm.dataMapping(n.emit).values shouldBe 5
    }

    it("updates bounds") {
      val cm = TestCompiledModel(model).withData(v, Seq(1.0, 2.0))
      cm.dataMapping.size shouldBe 2
      cm.dataMapping(n.emit).values shouldBe 2
    }
  }

  describe("withInitialValue(param, value)") {
    it("set scalars") {
      val cm = TestCompiledModel(model).withInitialValue(r, 4.0)
      val mapping = cm.initialValue.asInstanceOf[InitialValueMapping].mapping
      mapping.size shouldBe 1
      mapping(r.emit).values shouldBe 4.0
    }

    it("updates bounds") {
      val cm = TestCompiledModel(model).withInitialValue(p, Seq(1.0, 2.0))
      val mapping = cm.initialValue.asInstanceOf[InitialValueMapping].mapping
      mapping.size shouldBe 1
      mapping(p.emit).values shouldBe Seq(1.0, 2.0)
      cm.dataMapping(n.emit).values shouldBe 2
    }

    it("throws an exception if the initial value is already set") {
      val cm = TestCompiledModel(model).withInitialValue(1.0)
      an[IllegalStateException] should be thrownBy cm.withInitialValue(r, 5.0)
    }

    it("is cleared by a reset") {
      val cm = TestCompiledModel(model).withInitialValue(p, Seq(1.0, 2.0)).reset
      cm.withInitialValue(r, 5.0).initialValue.asInstanceOf[InitialValueMapping].mapping.size shouldBe 1
    }
  }

  describe("withInitialValue(value)") {
    it("updates the initial value") {
      val cm = TestCompiledModel(model).withInitialValue(4.0)
      cm.initialValue.asInstanceOf[InitialValueDouble].v shouldBe 4.0
    }

    it("throws an exception if the initial value is already set") {
      val cm = TestCompiledModel(model).withInitialValue(4.0)
      an[IllegalStateException] should be thrownBy cm.withInitialValue(1.0)
    }

    it("is cleared by a reset") {
      val cm = TestCompiledModel(model).withInitialValue(1.0).reset
      cm.withInitialValue(5.0).initialValue.asInstanceOf[InitialValueDouble].v shouldBe 5.0
    }
  }
}
