package com.cibo.scalastan.data

import java.io.FileReader

import com.cibo.scalastan.{DataDeclarationType, StanDeclaration, StanType}

import scala.util.parsing.combinator.JavaTokenParsers

case class RDataSource(fileName: String) extends DataSource {

  private case class Value(name: String, dims: Vector[Int], values: Vector[Double])

  private val parser = new JavaTokenParsers {
    private def number: Parser[Double] = floatingPointNumber ^^ { s => s.toDouble }

    private def numberList: Parser[Vector[Double]] =
      (number ~ "," ~ numberList ^^ { case (n ~ "," ~ ns) => n +: ns }) |
      (number ^^ { n => Vector(n) })

    private def label: Parser[String] = (stringLiteral ^^ { s => s.tail.dropRight(1) }) | ident

    private def vector: Parser[Vector[Double]] = "c" ~ "(" ~ numberList ~ ")" ^^ { case "c" ~ "(" ~ ns ~ ")" => ns }

    private def structure: Parser[(Vector[Int], Vector[Double])] =
      "structure" ~ "(" ~ vector ~ "," ~ ".Dim" ~ "=" ~ vector ~ ")" ^^ { case _ ~ _ ~ vs ~ _ ~ _ ~ _ ~ ds ~ _ =>
        (ds.map(_.toInt), vs)
      }

    private def assignment: Parser[String] = "<-" | "="

    private def scalarValue: Parser[Value] = label ~ assignment ~ number ^^ { case name ~ _ ~ v =>
      Value(name, Vector.empty, Vector(v))
    }

    private def vectorValue: Parser[Value] = label ~ assignment ~ vector ^^ { case name ~ _ ~ vs =>
      Value(name, Vector(vs.length), vs)
    }

    private def structureValue: Parser[Value] = label ~ assignment ~ structure ^^ { case name ~ _ ~ v =>
      Value(name, v._1, v._2)
    }

    private def value: Parser[Value] = scalarValue | vectorValue | structureValue

    private def values: Parser[Seq[Value]] = value.*

    def parse(s: String): ParseResult[Seq[Value]] = parseAll(values, s)

    def parseFile: ParseResult[Seq[Value]] = parseAll(values, new FileReader(fileName))
  }

  private lazy val values: Seq[Value] = parser.parseFile match {
    case parser.Success(vs, _)  => vs
    case parser.Error(msg, _)   => throw new IllegalArgumentException(s"error parsing $fileName: $msg")
    case parser.Failure(msg, _) => throw new IllegalArgumentException(s"error parsing $fileName: $msg")
  }

  def read[T <: StanType, R](
    decl: StanDeclaration[T, DataDeclarationType],
    name: String
  )(implicit ev: R =:= T#SCALA_TYPE): R = {
    values.find(_.name == name) match {
      case Some(value) => decl.typeConstructor.parse(value.dims, value.values).asInstanceOf[R]
      case None        => throw new IllegalArgumentException(s"$name not found in $fileName")
    }
  }

}
