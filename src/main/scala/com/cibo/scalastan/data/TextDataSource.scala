package com.cibo.scalastan.data

import com.cibo.scalastan._

object TextDataSource {

  private case class TextDataSource private (values: Seq[Seq[String]]) extends DataSource {
    def read[T <: StanType, R](
      decl: StanDataDeclaration[T],
      name: String
    )(implicit ev: R =:= T#SCALA_TYPE): R = {
      decl.typeConstructor match {
        case d: StanReal       => values.head.head.toDouble.asInstanceOf[R]
        case i: StanInt        => values.head.head.toInt.asInstanceOf[R]
        case v: StanVectorLike => values.flatten.map(_.toDouble).asInstanceOf[R]
        case m: StanMatrix     => values.map(_.map(_.toDouble)).asInstanceOf[R]
        case a: StanArray[_]   =>
          a.inner match {
            case d: StanReal       => values.flatten.map(_.toDouble).asInstanceOf[R]
            case i: StanInt        => values.flatten.map(_.toInt).asInstanceOf[R]
            case v: StanVectorLike => values.map(_.map(_.toDouble)).asInstanceOf[R]
            case b: StanArray[_]   =>
              b.inner match {
                case d: StanReal => values.map(_.map(_.toDouble)).asInstanceOf[R]
                case i: StanInt  => values.map(_.map(_.toInt)).asInstanceOf[R]
                case _           =>
                  throw new IllegalStateException(s"invalid type for TextDataSource: ${decl.typeConstructor}")
              }
            case _                 =>
              throw new IllegalStateException(s"invalid type for TextDataSource: ${decl.typeConstructor}")
          }
        case _                 =>
          throw new IllegalStateException(s"invalid type for TextDataSource: ${decl.typeConstructor}")
      }
    }
  }

  def fromString(content: String): DataSource = {
    val lines = content.split('\n')
    TextDataSource(lines.map(_.split(' ').toSeq).toSeq)
  }

  def fromFile(fileName: String): DataSource = {
    fromString(scala.io.Source.fromFile(fileName).getLines.mkString("\n"))
  }

}
