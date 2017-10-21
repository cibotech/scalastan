package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}

trait ScalaStanBaseSpec extends FunSpec with Matchers with ScalaStan {

  private def compare(actual: String, template: String): Boolean = {
    (actual, template) match {
      case (_, "")                                                  => true
      case ("", _)                                                  => false
      case (as, ts) if as.head == ts.head                           => compare(as.tail, ts.tail)
      case (as, ts) if Character.isSpaceChar(as.head)               => compare(as.tail, ts)
      case (as, ts) if Character.isSpaceChar(ts.head)               => compare(as, ts.tail)
      case (as, ts) if Character.isDigit(as.head) && ts.head == '#' => compare(as.dropWhile(Character.isDigit), ts.tail)
      case (as, ts) if as.head != ts.head                           => compare(as.tail, template)
      case _                                                        => false
    }
  }

  def check(actual: String, template: String): Unit = {
    withClue(s"actual:\n$actual\nexpected:\n$template\n") {
      compare(actual, template) shouldBe true
    }
  }

  // Compare the code output of the model with a template.
  // Spaces are ignored and "#" in the template matches any integer.
  def checkCode(model: ScalaStan#Model, template: String): Unit = check(model.getCode, template)
}
