package com.cibo.scalastan

import java.io.{PrintWriter, StringWriter}

import com.cibo.scalastan.ast.StanParameterDeclaration
import com.cibo.scalastan.run.{StanCompiler, StanRunner}
import org.scalatest.{FunSpec, Matchers}

trait ScalaStanBaseSpec extends FunSpec with Matchers {

  case class MockRunner(data: Vector[Map[String, String]] = Vector.empty) extends StanRunner {

    private val results = scala.collection.mutable.ArrayBuffer[(String, Vector[Vector[String]])]()

    private def set(prefix: String, values: Any, mapping: Map[String, String]): Map[String, String] = {
      values match {
        case s: Seq[_] =>
          s.zipWithIndex.foldLeft(mapping) { case (m, (v, i)) =>
            set(s"$prefix.${i + 1}", v, m)
          }
        case _ => mapping + (prefix -> values.toString)
      }
    }


    def set[T <: StanType](decl: StanParameterDeclaration[T], values: Seq[T#SCALA_TYPE]): MockRunner = {
      require(data.isEmpty || data.length == values.length)
      val dataBefore = if (data.isEmpty) values.map(_ => Map[String, String]("lp__" -> "1")) else data
      val prefix = decl.emit
      val newData = values.zip(dataBefore).map { case (v, d) => set(prefix, v, d) }
      copy(data = newData.toVector)
    }

    def run(model: CompiledModel, chains: Int, seed: Int, cache: Boolean, method: RunMethod.Method): StanResults = {
      val mappedData: Map[String, Vector[Vector[String]]] = data.flatten.groupBy(_._1).mapValues { grouped =>
        val iterations = grouped.map { case (k, v) => v }
        Vector.fill(chains)(iterations)
      }
      StanResults(mappedData, Vector.empty, model, method)
    }
  }

  implicit object MockCompiler extends StanCompiler {
    def compile(ss: ScalaStan, model: ScalaStan#Model): CompiledModel = CompiledModel(
      model = model,
      runner = MockRunner()
    )
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
  def checkCode(model: ScalaStan#Model, template: String): Unit = {
    val sw = new StringWriter()
    model.emit(new PrintWriter(sw))
    sw.close()
    check(sw.toString, template)
  }
}
