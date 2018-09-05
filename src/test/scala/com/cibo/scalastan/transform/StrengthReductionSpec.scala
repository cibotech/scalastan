package com.cibo.scalastan.transform

import com.cibo.scalastan.{ScalaStan, ScalaStanBaseSpec}

class StrengthReductionSpec extends ScalaStanBaseSpec with ScalaStan {
  describe("fma") {
    it("converts a * b + c") {
      val model = new Model {
        val x = local(real())
        val a = local(real())
        val b = local(real())
        val c = local(real())
        x := a * b + c
      }
      checkCode(model.transform(StrengthReduction()), "x = fma(a, b, c)")
    }

    it("converts a + b * c") {
      val model = new Model {
        val x = local(real())
        val a = local(real())
        val b = local(real())
        val c = local(real())
        x := a + b * c
      }
      checkCode(model.transform(StrengthReduction()), "x = fma(b, c, a)")
    }

    it("does not converts a * b * c") {
      val model = new Model {
        val x = local(real())
        val a = local(real())
        val b = local(real())
        val c = local(real())
        x := a * b * c
      }
      checkCode(model.transform(StrengthReduction()), "x = ((a) * (b)) * (c)")
    }
  }

  describe("log_sum_exp") {
    it("converts log(exp(a) + exp(b))") {
      val model = new Model {
        val x = local(real())
        val a = local(real())
        val b = local(real())
        x := stan.log(stan.exp(a) + stan.exp(b))
      }
      checkCode(model.transform(StrengthReduction()), "x = log_sum_exp(a, b)")
    }

    it("converts log(sum(exp(vs)))") {
      val model = new Model {
        val x = local(real())
        val vs = local(vector(5))
        x := stan.log(stan.sum(stan.exp(vs)))
      }
      checkCode(model.transform(StrengthReduction()), "x = log_sum_exp(vs)")
    }

    it("does not converts log(sum(vs))") {
      val model = new Model {
        val x = local(real())
        val vs = local(vector(5))
        x := stan.log(stan.sum(vs))
      }
      checkCode(model.transform(StrengthReduction()), "x = log(sum(vs))")
    }
  }
}
