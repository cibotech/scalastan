package com.cibo.scalastan.data

import java.io.FileReader

import com.cibo.scalastan.{StanDataDeclaration, StanType}

import scala.util.parsing.combinator.JavaTokenParsers

object RDataSource {

  private case class Value(name: String, dims: Vector[Int], values: Vector[String])

  private class RDataSource(values: Seq[Value]) extends DataSource {
    def read[T <: StanType, R](
      decl: StanDataDeclaration[T],
      name: String
    )(implicit ev: R =:= T#SCALA_TYPE): R = {
      values.find(_.name == name) match {
        case Some(value) => decl.typeConstructor.parse(value.dims, value.values).asInstanceOf[R]
        case None        => throw new IllegalArgumentException(s"$name not found")
      }
    }
  }

  private object RDataParser extends JavaTokenParsers {

    override protected val whiteSpace = """(\s|#.*)+""".r

    private def value: Parser[String] = floatingPointNumber | (stringLiteral ^^ { s => s.tail.dropRight(1) })

    private def valueList: Parser[Vector[String]] =
      (value ~ "," ~ valueList ^^ { case (n ~ "," ~ ns) => n +: ns }) |
        (value ^^ { n => Vector(n) })

    private def label: Parser[String] = (stringLiteral ^^ { s => s.tail.dropRight(1) }) | ident

    private def vector: Parser[Vector[String]] = "c" ~ "(" ~ valueList ~ ")" ^^ { case _ ~ _ ~ ns ~ _ => ns }

    private def structure: Parser[(Vector[Int], Vector[String])] =
      "structure" ~ "(" ~ vector ~ "," ~ ".Dim" ~ "=" ~ vector ~ ")" ^^ { case _ ~ _ ~ vs ~ _ ~ _ ~ _ ~ ds ~ _ =>
        (ds.map(_.toInt), vs)
      }

    private def assignment: Parser[String] = "<-" | "="

    private def scalarValue: Parser[Value] = label ~ assignment ~ value ^^ { case name ~ _ ~ v =>
      Value(name, Vector.empty, Vector(v))
    }

    private def vectorValue: Parser[Value] = label ~ assignment ~ vector ^^ { case name ~ _ ~ vs =>
      Value(name, Vector(vs.length), vs)
    }

    private def structureValue: Parser[Value] = label ~ assignment ~ structure ^^ { case name ~ _ ~ v =>
      Value(name, v._1, v._2)
    }

    private def statement: Parser[Value] =
      (scalarValue | vectorValue | structureValue) ~ opt(";") ^^ { case v ~ _ => v }

    private def statements: Parser[Seq[Value]] = statement.*

    def parse(s: String): ParseResult[Seq[Value]] = parseAll(statements, s)

    def parseFile(fileName: String): ParseResult[Seq[Value]] = parseAll(statements, new FileReader(fileName))
  }

  def fromString(content: String): DataSource = {
    val values = RDataParser.parse(content) match {
      case RDataParser.Success(vs, _)  => vs
      case RDataParser.Error(msg, _)   => throw new IllegalArgumentException(s"error parsing string: $msg")
      case RDataParser.Failure(msg, _) => throw new IllegalArgumentException(s"error parsing string: $msg")
    }
    new RDataSource(values)
  }

  def fromFile(fileName: String): DataSource = {
    val values = RDataParser.parseFile(fileName) match {
      case RDataParser.Success(vs, _)  => vs
      case RDataParser.Error(msg, _)   => throw new IllegalArgumentException(s"error parsing $fileName: $msg")
      case RDataParser.Failure(msg, _) => throw new IllegalArgumentException(s"error parsing $fileName: $msg")
    }
    new RDataSource(values)
  }
}
