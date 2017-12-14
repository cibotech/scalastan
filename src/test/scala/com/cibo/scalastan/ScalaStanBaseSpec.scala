package com.cibo.scalastan

import com.cibo.scalastan.ast.StanParameterDeclaration
import org.scalatest.{FunSpec, Matchers}

trait ScalaStanBaseSpec extends FunSpec with Matchers {

  case class MockCompiledModel(
    private[scalastan] val ss: ScalaStan,
    private[scalastan] val code: String,
    private[scalastan] val data: Vector[Map[String, String]] = Vector.empty,
    protected val dataMapping: Map[String, DataMapping[_]] = Map.empty
  ) extends CompiledModel {

    private def set(prefix: String, values: Any, mapping: Map[String, String]): Map[String, String] = {
      values match {
        case s: Seq[_] =>
          s.zipWithIndex.foldLeft(mapping) { case (m, (v, i)) =>
            set(s"$prefix.${i + 1}", v, m)
          }
        case _ => mapping + (prefix -> values.toString)
      }
    }

    def set[T <: StanType](decl: StanParameterDeclaration[T], values: Seq[T#SCALA_TYPE]): MockCompiledModel = {
      require(data.isEmpty || data.length == values.length)
      val dataBefore = if (data.isEmpty) values.map(_ => Map[String, String]("lp__" -> "1")) else data
      val prefix = decl.emit
      val newData = values.zip(dataBefore).map { case (v, d) => set(prefix, v, d) }
      copy(data = newData.toVector)
    }

    protected def replaceMapping(newMapping: Map[String, DataMapping[_]]): CompiledModel =
      copy(dataMapping = newMapping)
    protected def runChecked(chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults =
      MockRunner.run(this, chains, seed, cache, method)
  }

  implicit object MockRunner extends StanRunner[MockCompiledModel] {
    def compile(ss: ScalaStan, model: ScalaStan#Model): CompiledModel = MockCompiledModel(
      ss = ss,
      code = model.getCode
    )
    def run(
      model: MockCompiledModel,
      chains: Int,
      seed: Int,
      cache: Boolean,
      method: RunMethod.Method
    ): StanResults = {
      StanResults((0 until chains).map(_ => model.data).toVector, model)
    }
  }

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
