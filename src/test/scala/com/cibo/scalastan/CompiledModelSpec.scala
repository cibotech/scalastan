package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}

class CompiledModelSpec extends FunSpec with Matchers with ScalaStan {

  case class TestCompiledModel(
    model: ScalaStan#Model,
    dataMapping: Map[String, DataMapping[_]] = Map.empty,
    initialValues: Map[String, DataMapping[_]] = Map.empty
  ) extends CompiledModel {
    protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CompiledModel =
      copy(dataMapping = newMapping)
    protected def updateInitialValue(name: String, value: DataMapping[_]): CompiledModel =
      copy(initialValues = initialValues.updated(name, value))
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

  describe("withInitialValue") {
    it("set scalars") {
      val cm = TestCompiledModel(model).withInitialValue(r, 4.0)
      cm.initialValues.size shouldBe 1
      cm.initialValues(r.emit).values shouldBe 4.0
    }

    it("updates bounds") {
      val cm = TestCompiledModel(model).withInitialValue(p, Seq(1.0, 2.0))
      cm.dataMapping.size shouldBe 1
      cm.initialValues.size shouldBe 1
      cm.initialValues(p.emit).values shouldBe Seq(1.0, 2.0)
      cm.dataMapping(n.emit).values shouldBe 2
    }
  }
}
