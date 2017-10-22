package com.cibo.scalastan

import org.scalatest.{FunSpec, Matchers}

trait ScalaStanBaseSpec extends FunSpec with Matchers with ScalaStan {

  private def removeSpaces(str: String): String = str.replaceAllLiterally(" ", "").replaceAllLiterally("\n", "")

  private def removeNumbers(str: String): String = str.replaceAll("[0-9]+", "#")

  private def compare(actual: String, template: String, originalTemplate: String = ""): Boolean = {
    val fixedActual = removeNumbers(removeSpaces(actual))
    val fixedExpected = removeNumbers(removeSpaces(template))
    fixedActual.contains(fixedExpected)
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
