package com.cibo.scalastan

import com.cibo.scalastan.ast.{StanBlock, StanStatement}
import com.cibo.scalastan.transform.StanTransform
import org.scalatest.{FunSpec, Matchers}

class TransformedModelSpec extends FunSpec with Matchers with ScalaStanBaseSpec {
  describe("TransformedModel") {
    it("emits the transformed model") {
      // Simple transform to clear out the model.
      case class TestTransform() extends StanTransform[Unit]() {
        def initialState: Unit = ()
        override def handleModel(
          statement: StanStatement
        )(implicit context: StanContext): State[StanStatement] = State.pure(StanBlock())
      }

      val model = new StanModel {
        val x = parameter(real())
        x ~ stan.normal(0, 1)
      }

      // Make sure the untransformed model is what we expect.
      checkCode(model, "model { x ~ normal(0, 1); } ")

      // Make sure the transformed model is empty.
      checkCode(model.transform(TestTransform()), "model { }")
    }
  }
}
