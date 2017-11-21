/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan.data

import com.cibo.scalastan.{StanDataDeclaration, StanType}

protected case class DataValue private[data] (
  name: String,
  dims: Vector[Int],
  values: Vector[String]
) {
  require(dims.product == values.length)

  def toVector: Vector[Double] = values.map(_.toDouble)

  def toMatrix: Vector[Vector[Double]] = dims match {
    case Vector(x, y) => values.map(_.toDouble).grouped(y).toVector
    case _            => throw new IllegalArgumentException(s"$name cannot be converted to a matrix")
  }
}

case class DataSource private[data] (
  private val rawSeq: Seq[DataValue],
  private val enumerations: Map[String, Map[String, Int]] = Map.empty
) {

  private lazy val raw: Map[String, DataValue] = rawSeq.map(v => v.name -> v).toMap
  private lazy val reverseEnumerations: Map[String, Map[Int, String]] = enumerations.mapValues(_.map(_.swap).toMap)

  /** Get the names of the data values. */
  def names: Set[String] = raw.keySet

  /** Get the dimensions of a particular data value. */
  def dims(name: String = ""): Seq[Int] = raw(name).dims

  /** Read a data value as a vector. */
  def readVector(name: String = ""): Vector[Double] = raw(name).toVector

  /** Read a data value as a matrix. */
  def readMatrix(name: String = ""): Vector[Vector[Double]] = raw(name).toMatrix

  /** Create a matrix by combining multiple vectors. */
  def readMatrix(columns: Seq[String]): Vector[Vector[Double]] = {
    require(columns.nonEmpty)
    val len = raw(columns.head).values.length
    require(columns.tail.forall(c => raw(c).values.length == len), "All columns must have the same length")
    columns.map { column =>
      raw(column).toVector
    }.toVector.transpose
  }

  /** Return a DataSource with one or more values sorted.
    * `key` is the column to use for sorting (assumed to be a numeric type).
    * `others` are additional columns to sort (must have the same length as the key column). */
  def sorted(key: String, others: String*): DataSource = {
    require(others.forall(raw.contains))
    val ordering = raw(key).toVector.zipWithIndex.sorted.map(_._2)
    val len = ordering.length
    val newValues = raw.map { case (name, value) =>
      if (others.contains(name) || key == name) {
        require(value.values.length == len, s"$name does not have the same length as $key")
        name -> DataValue(name, Vector(len), ordering.map(i => value.values(i)))
      } else {
        name -> value
      }
    }.values.toSeq
    copy(rawSeq = newValues)
  }

  /** Create an enumeration for the specified column. */
  def createEnumeration(key: String): DataSource = {
    val mapping = raw(key).values.distinct.zipWithIndex.toMap
    val newValues = rawSeq.map { dv =>
      if (dv.name == key) {
        dv.copy(values = dv.values.map(v => mapping(v).toString))
      } else dv
    }
    copy(rawSeq = newValues, enumerations = enumerations + (key -> mapping))
  }

  /** Get enumeration string the specified index. */
  def getEnumeration(key: String, index: Int): String = reverseEnumerations(key)(index)

  /** Create indicators (a separate column for each value) for the specified column(s). */
  def createIndicators(keys: String*): DataSource = {
    require(keys.nonEmpty, "Must specify at least one column")
    val columns = keys.map(raw.apply)
    require(columns.forall(_.dims.length == 1), "Indicators can only be created for one-dimensional types")
    require(columns.forall(_.values.length == columns.head.values.length), "All columns must have the same length")
    val names = columns.map(_.values).transpose.map(_.mkString("."))
    val prefix = keys.mkString(".")
    val indicators = names.distinct.map { name =>
      val newName = s"$prefix.$name"
      val newValue = names.map(v => if (v == name) "1" else "0").toVector
      DataValue(newName, Vector(newValue.length), newValue)
    }
    copy(rawSeq ++ indicators)
  }

  /** Get the names of the indicator vectors for a value. */
  def getIndicators(keys: String*): Seq[String] = {
    val columns = keys.map(raw.apply)
    val prefix = keys.mkString(".")
    val names = columns.map(_.values).transpose.map(_.mkString("."))
    names.distinct.map(name => s"$prefix.$name")
  }

  /** Add a scalar value to the data source. */
  def withScalar[T](key: String, value: T): DataSource = {
    copy(rawSeq = rawSeq :+ DataValue(key, Vector.empty, Vector(value.toString)))
  }

  /** Add a vector to the data source. */
  def withVector[T](key: String, value: Seq[T]): DataSource = {
    copy(rawSeq = rawSeq :+ DataValue(key, Vector(value.length), value.map(_.toString).toVector))
  }

  /** Read the specified data declaration using the named data value. */
  def read[T <: StanType](decl: StanDataDeclaration[T], name: String = ""): T#SCALA_TYPE = {
    raw.get(name) match {
      case Some(value) => decl.typeConstructor.parse(value.dims, value.values)
      case None        => throw new IllegalArgumentException(s"$name not found")
    }
  }

  /** Read the specified data declaration using the named data value. */
  def apply[T <: StanType](decl: StanDataDeclaration[T], name: String = ""): (StanDataDeclaration[T], T#SCALA_TYPE) = {
    (decl, read[T](decl, name))
  }

  /** Set a value. */
  def write[T <: StanType](
    decl: StanDataDeclaration[T],
    name: String,
    data: T#SCALA_TYPE
  ): DataSource = {
    val converted = data.asInstanceOf[decl.typeConstructor.SCALA_TYPE]
    val dims = decl.typeConstructor.getDims(converted).toVector
    val values = decl.typeConstructor.getData(converted).toVector
    copy(rawSeq :+ DataValue(name, dims, values))
  }
}

object DataSource {
  def apply(): DataSource = DataSource(Seq.empty)
}
