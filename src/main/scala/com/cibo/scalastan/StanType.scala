/*
 * Copyright (c) 2017 - 2018 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.cibo.scalastan.ast.StanValue

sealed trait StanType {

  // Our type (for apply).
  protected type THIS_TYPE <: StanType

  // The type of elements for this type (real for vector, etc.)
  private[scalastan] type ELEMENT_TYPE <: StanType

  // The type with one level dereferenced.
  private[scalastan] type NEXT_TYPE <: StanType

  // The type with the same shape, but with StanReal elements.
  private[scalastan] type REAL_TYPE <: StanType

  // The Scala analog to this type.
  private[scalastan] type SCALA_TYPE

  // The Scala analog to this type used for summary statistics.
  private[scalastan] type SUMMARY_TYPE

  // Emit the name and bounds for this type.
  private[scalastan] def typeName: String

  // Emit additional dimensions for this type (to be placed after the name in a declaration).
  // matrix[2, 3] name[this, and_this]
  private[scalastan] def emitDims: Seq[String] = Seq.empty

  // Get declarations for indices.
  private[scalastan] def getIndices: Seq[StanValue[StanInt]] = Seq.empty

  // Lower bound on each element.
  private[scalastan] val lower: Option[StanValue[ELEMENT_TYPE]]

  // Upper bound on each element.
  private[scalastan] val upper: Option[StanValue[ELEMENT_TYPE]]

  // Get the data and dimensions for this type to be used by Stan.
  // These are used by the emitData function below.
  private[scalastan] def getData(data: SCALA_TYPE): Seq[String]
  private[scalastan] def getDims(data: SCALA_TYPE): Seq[Int]

  // Emit the data in R format.
  final private[scalastan] def emitData(data: SCALA_TYPE): String = {
    val dims = getDims(data)
    val strs = getData(data)
    dims.length match {
      case 0 =>
        require(strs.length == 1)
        strs.head
      case 1 =>
        require(strs.length == dims.head)
        strs.mkString("c(", ",", ")")
      case _ =>
        require(strs.length == dims.product)
        val cstr = strs.mkString("c(", ",", ")")
        val dstr = dims.mkString("c(", ",", ")")
        s"structure($cstr, .Dim = $dstr)"
    }
  }

  // Parse data from the iterations CSV.
  private[scalastan] def parse(name: String, values: Map[String, String]): SCALA_TYPE

  // Parse data from a vector of dimensions and values.
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): SCALA_TYPE

  // Combine values using the specified function.
  // The function takes a seq of chain -> iteration -> value
  private[scalastan] def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): SUMMARY_TYPE

  // Emit a full Stan declaration for this type given the specified name.
  final private[scalastan] def emitDeclaration(name: String): String = {
    val dims = emitDims
    if (dims.nonEmpty) {
      val dimString = dims.mkString(",")
      s"$typeName $name[$dimString]"
    } else {
      s"$typeName $name"
    }
  }

  final private[scalastan] def emitFunctionDeclaration: String = {
    val dims = emitDims
    if (dims.nonEmpty) {
      val dimString = dims.mkString(",")
      s"$typeName[$dimString]"
    } else {
      typeName
    }
  }

  final private[scalastan] def emitFunctionDeclaration(name: String): String = {
    s"$emitFunctionDeclaration $name"
  }

  // Emit the upper/lower bounds on elements.
  final protected def emitBounds: String =
    if (lower.isDefined || upper.isDefined) {
      val lowerStr = lower.map(l => s"lower=${l.emit}").toSeq
      val upperStr = upper.map(u => s"upper=${u.emit}").toSeq
      (lowerStr ++ upperStr).mkString("<", ",", ">")
    } else {
      ""
    }

  // Determine if the bounds are all derived from data.
  final private[scalastan] def isDerivedFromData: Boolean =
    lower.forall(_.isDerivedFromData) && upper.forall(_.isDerivedFromData) && getIndices.forall(_.isDerivedFromData)

  // Get the type constructor for the next type.
  private[scalastan] def next: NEXT_TYPE

  def apply(): StanArray[THIS_TYPE] = {
    StanArray(None, this.asInstanceOf[THIS_TYPE])
  }

  def apply(
    dim: StanValue[StanInt]
  ): StanArray[THIS_TYPE] =
    StanArray(dim, this.asInstanceOf[THIS_TYPE])

  def apply(
    dim1: StanValue[StanInt],
    dim2: StanValue[StanInt]
  ): StanArray[StanArray[THIS_TYPE]] =
    StanArray(dim1, StanArray(dim2, this.asInstanceOf[THIS_TYPE]))

  def apply(
    dim1: StanValue[StanInt],
    dim2: StanValue[StanInt],
    dim3: StanValue[StanInt]
  ): StanArray[StanArray[StanArray[THIS_TYPE]]] =
    StanArray(dim1, StanArray(dim2, StanArray(dim3, this.asInstanceOf[THIS_TYPE])))

  def apply(
    dim1: StanValue[StanInt],
    dim2: StanValue[StanInt],
    dim3: StanValue[StanInt],
    dim4: StanValue[StanInt]
  ): StanArray[StanArray[StanArray[StanArray[THIS_TYPE]]]] =
    StanArray(dim1, StanArray(dim2, StanArray(dim3, StanArray(dim4, this.asInstanceOf[THIS_TYPE]))))
}

trait StanScalarType extends StanType {
  private[scalastan] type SUMMARY_TYPE = Double
  private[scalastan] type NEXT_TYPE = StanVoid
  private[scalastan] type REAL_TYPE = StanReal
  final private[scalastan] def next: StanVoid = StanVoid()
}

trait StanCompoundType extends StanType
trait StanVectorOrMatrix extends StanCompoundType

case class StanVoid private[scalastan] (
  private[scalastan] val lower: Option[StanValue[StanVoid]] = None,
  private[scalastan] val upper: Option[StanValue[StanVoid]] = None
) extends StanType {
  protected type THIS_TYPE = StanVoid
  private[scalastan] type ELEMENT_TYPE = StanVoid
  private[scalastan] type NEXT_TYPE = StanVoid
  private[scalastan] type REAL_TYPE = StanReal
  private[scalastan] type SCALA_TYPE = Unit
  private[scalastan] type SUMMARY_TYPE = Unit
  private[scalastan] def typeName: String = "void"
  private[scalastan] def getData(data: Unit): Seq[String] = Seq.empty
  private[scalastan] def getDims(data: Unit): Seq[Int] = Seq.empty
  private[scalastan] def parse(name: String, values: Map[String, String]): Unit = ()
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): Unit = ()
  private[scalastan] def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): Unit = ()
  final private[scalastan] def next: StanVoid = this
}

case class StanString private[scalastan] () extends StanType {
  protected type THIS_TYPE = StanString
  private[scalastan] type ELEMENT_TYPE = StanString
  private[scalastan] type NEXT_TYPE = StanVoid
  private[scalastan] type REAL_TYPE = StanReal
  private[scalastan] type SCALA_TYPE = String
  private[scalastan] type SUMMARY_TYPE = String
  private[scalastan] val lower: Option[StanValue[StanString]] = None
  private[scalastan] val upper: Option[StanValue[StanString]] = None
  private[scalastan] def typeName: String = "string"
  private[scalastan] def getData(data: String): Seq[String] = Seq(s""""$data"""")
  private[scalastan] def getDims(data: String): Seq[Int] = Seq.empty
  private[scalastan] def parse(name: String, values: Map[String, String]): String = values(name)
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): String = ""
  private[scalastan] def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): String = ""
  final private[scalastan] def next: StanVoid = StanVoid()
}

abstract class StanDiscreteType extends StanScalarType {
  private[scalastan] def typeName: String = s"int$emitBounds"
}

case class StanInt private[scalastan] (
  private[scalastan] val lower: Option[StanValue[StanInt]] = None,
  private[scalastan] val upper: Option[StanValue[StanInt]] = None
) extends StanDiscreteType {
  protected type THIS_TYPE = StanInt
  private[scalastan] type ELEMENT_TYPE = StanInt
  private[scalastan] type SCALA_TYPE = Int
  private[scalastan] def getDims(data: Int): Seq[Int] = Seq.empty
  private[scalastan] def getData(data: Int): Seq[String] = Seq(data.toString)
  private[scalastan] def parse(name: String, values: Map[String, String]): Int = values(name).toInt
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): Int = {
    require(dims.isEmpty, s"int must have a dimensionality of 0, got ${dims.length}")
    values.head.toInt
  }
  private[scalastan] def combine(values: Seq[Seq[Int]])(func: Seq[Seq[Double]] => Double): Double =
    func(values.map(_.map(_.toDouble)))
}

case class StanCategorical private[scalastan] () extends StanDiscreteType {
  protected type THIS_TYPE = StanCategorical
  private[scalastan] type ELEMENT_TYPE = StanCategorical
  private[scalastan] type SCALA_TYPE = String

  private[scalastan] val lower: Option[StanValue[ELEMENT_TYPE]] = None
  private[scalastan] val upper: Option[StanValue[ELEMENT_TYPE]] = None

  private val categories = scala.collection.mutable.Map[String, Int]()

  private def lookup(value: String): Int = categories.getOrElseUpdate(value, categories.size)

  private[scalastan] def getDims(data: String): Seq[Int] = Seq.empty
  private[scalastan] def getData(data: String): Seq[String] = Seq(lookup(data).toString)
  private[scalastan] def parse(name: String, values: Map[String, String]): String = values(name)
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): String = {
    require(dims.isEmpty, s"categorical must have a dimensionality of 0, got ${dims.length}")
    values.head
  }
  private[scalastan] def combine(values: Seq[Seq[String]])(func: Seq[Seq[Double]] => Double): Double =
    func(values.map(_.map(v => lookup(v).toDouble)))
}

case class StanArray[CONTAINED <: StanType] private[scalastan] (
  private val dim: Option[StanValue[StanInt]],
  private[scalastan] val inner: CONTAINED
) extends StanCompoundType {
  protected type THIS_TYPE = StanArray[CONTAINED]
  private[scalastan] type ELEMENT_TYPE = CONTAINED#ELEMENT_TYPE
  private[scalastan] type NEXT_TYPE = CONTAINED
  private[scalastan] type REAL_TYPE = StanArray[CONTAINED#REAL_TYPE]
  private[scalastan] type SCALA_TYPE = Seq[CONTAINED#SCALA_TYPE]
  private[scalastan] type SUMMARY_TYPE = Seq[CONTAINED#SUMMARY_TYPE]

  private[scalastan] val lower: Option[StanValue[CONTAINED#ELEMENT_TYPE]] =
    inner.lower.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]
  private[scalastan] val upper: Option[StanValue[CONTAINED#ELEMENT_TYPE]] =
    inner.upper.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]

  override private[scalastan] def emitDims: Seq[String] = dim.map(_.emit).getOrElse("") +: inner.emitDims
  override private[scalastan] def getIndices: Seq[StanValue[StanInt]] = dim.get +: inner.getIndices
  private[scalastan] def typeName: String = inner.typeName
  private[scalastan] def getData(data: SCALA_TYPE): Seq[String] =
    data.map(d => inner.getData(d.asInstanceOf[inner.SCALA_TYPE])).transpose.flatten
  private[scalastan] def getDims(data: SCALA_TYPE): Seq[Int] = {
    if (data.nonEmpty) {
      data.length +: inner.getDims(data.head.asInstanceOf[inner.SCALA_TYPE])
    } else if (inner.isInstanceOf[StanScalarType]) {
      Seq(data.length)
    } else {
      data.length +: inner.getDims(Seq.empty.asInstanceOf[inner.SCALA_TYPE])
    }
  }
  private[scalastan] def parse(name: String, values: Map[String, String]): SCALA_TYPE = {
    // The name will be the prefix up to the number we need to parse.
    val prefix = s"$name."
    val prefixLength = prefix.length

    // Get the size of this dimension (note that arrays in STAN are 1-based).
    val size = values.keys.filter(_.startsWith(prefix)).map(_.drop(prefixLength).takeWhile(Character.isDigit).toInt).max

    // Parse the next level for each element of this array.
    Vector.tabulate[CONTAINED#SCALA_TYPE](size) { i =>
      val nextName = s"$prefix${i + 1}"
      inner.parse(nextName, values)
    }
  }

  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): SCALA_TYPE = {
    require(dims.nonEmpty)
    val length = dims.head       // The length of the outer-most vector
    val size = dims.tail.product // The number of elements contained in each dimension.
    Vector.tabulate[CONTAINED#SCALA_TYPE](length) { i =>
      inner.parse(dims.tail, values.drop(i * size))
    }
  }

  private[scalastan] def combine(
    values: Seq[Seq[Seq[CONTAINED#SCALA_TYPE]]]
  )(
    func: Seq[Seq[Double]] => Double
  ): Seq[CONTAINED#SUMMARY_TYPE] = {
    values.transpose.map { v =>
      inner.combine(v.asInstanceOf[Seq[Seq[inner.SCALA_TYPE]]])(func)
    }.toVector
  }

  final private[scalastan] def next: NEXT_TYPE = inner
}

case class StanReal private[scalastan] (
  private[scalastan] val lower: Option[StanValue[StanReal]] = None,
  private[scalastan] val upper: Option[StanValue[StanReal]] = None
) extends StanScalarType {
  protected type THIS_TYPE = StanReal
  private[scalastan] type ELEMENT_TYPE = StanReal
  private[scalastan] type SCALA_TYPE = Double
  private[scalastan] def typeName: String = s"real$emitBounds"
  private[scalastan] def getData(data: Double): Seq[String] = Seq(data.toString)
  private[scalastan] def getDims(data: Double): Seq[Int] = Seq.empty
  private[scalastan] def parse(name: String, values: Map[String, String]): Double = values(name).toDouble
  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): Double = {
    require(dims.isEmpty, s"real must have a dimensionality of 0, got ${dims.length}")
    values.head.toDouble
  }
  private[scalastan] def combine(values: Seq[Seq[Double]])(func: Seq[Seq[Double]] => Double): Double = func(values)
}

trait StanVectorLike extends StanVectorOrMatrix {
  private[scalastan] type ELEMENT_TYPE = StanReal
  private[scalastan] type NEXT_TYPE = StanReal
  private[scalastan] type REAL_TYPE = THIS_TYPE
  private[scalastan] type SCALA_TYPE = Seq[Double]
  private[scalastan] type SUMMARY_TYPE = Seq[Double]

  protected val dim: StanValue[StanInt]

  override private[scalastan] def getIndices: Seq[StanValue[StanInt]] = Seq(dim)

  private[scalastan] def getData(data: Seq[Double]): Seq[String] = data.map(_.toString)
  private[scalastan] def getDims(data: Seq[Double]): Seq[Int] = Seq(data.length)

  private[scalastan] def parse(name: String, values: Map[String, String]): Seq[Double] = {
    val prefix = s"$name."
    values.filterKeys(_.startsWith(prefix)).toVector.map { case (key, value) =>
      key.slice(prefix.length, key.length).toInt -> value
    }.sortBy(_._1).map(_._2.toDouble)
  }

  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): Seq[Double] = {
    require(dims.length == 1, s"vector must have dimensionality of 1, got ${dims.length}")
    values.take(dims.head).map(_.toDouble).toVector
  }

  private[scalastan] def combine(values: Seq[Seq[Seq[Double]]])(func: Seq[Seq[Double]] => Double): Seq[Double] = {
    values.map(_.transpose).transpose.map { v =>
      func(v)
    }.toVector
  }

  final private[scalastan] def next: NEXT_TYPE = StanReal(lower, upper)
}

sealed abstract class VectorConstraint(val name: String)
object VectorConstraint {
  case object Default extends VectorConstraint("vector")
  case object Simplex extends VectorConstraint("simplex")
  case object UnitVector extends VectorConstraint("unit_vector")
  case object Ordered extends VectorConstraint("ordered")
  case object PositiveOrdered extends VectorConstraint("positive_ordered")
}

case class StanVector private[scalastan] (
  protected val dim: StanValue[StanInt],
  private[scalastan] val lower: Option[StanValue[StanReal]] = None,
  private[scalastan] val upper: Option[StanValue[StanReal]] = None,
  private[scalastan] val constraint: VectorConstraint = VectorConstraint.Default
) extends StanVectorLike {
  protected type THIS_TYPE = StanVector
  private[scalastan] def typeName: String = s"${constraint.name}$emitBounds[${dim.emit}]"
}

case class StanRowVector private[scalastan] (
  protected val dim: StanValue[StanInt],
  private[scalastan] val lower: Option[StanValue[StanReal]] = None,
  private[scalastan] val upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  protected type THIS_TYPE = StanRowVector
  private[scalastan] def typeName: String = s"row_vector$emitBounds[${dim.emit}]"
}

sealed abstract class MatrixConstraint(val name: String, val dims: Int) {
  final def emitSizes(rows: StanValue[StanInt], cols: StanValue[StanInt]): String = dims match {
    case 1 =>
      require(rows == cols)
      s"[${rows.emit}]"
    case 2 => s"[${rows.emit},${cols.emit}]"
    case _ => throw new IllegalStateException(s"Invalid number of distinct dimensions for matrix: $dims")
  }
}

object MatrixConstraint {
  case object Default extends MatrixConstraint("matrix", 2)
  case object CorrMatrix extends MatrixConstraint("corr_matrix", 1)
  case object CholeskyFactorCorr extends MatrixConstraint("cholesky_factor_corr", 1)
  case object CovMatrix extends MatrixConstraint("cov_matrix", 1)
  case object CholeskyFactorCov extends MatrixConstraint("cholesky_factor_cov", 1)
}

case class StanMatrix private[scalastan] (
  private val rows: StanValue[StanInt],
  private val cols: StanValue[StanInt],
  private[scalastan] val lower: Option[StanValue[StanReal]] = None,
  private[scalastan] val upper: Option[StanValue[StanReal]] = None,
  private[scalastan] val constraint: MatrixConstraint = MatrixConstraint.Default
) extends StanVectorOrMatrix {
  protected type THIS_TYPE = StanMatrix
  private[scalastan] type ELEMENT_TYPE = StanReal
  private[scalastan] type NEXT_TYPE = StanVector
  private[scalastan] type REAL_TYPE = StanMatrix
  private[scalastan] type SCALA_TYPE = Seq[Seq[Double]]
  private[scalastan] type SUMMARY_TYPE = Seq[Seq[Double]]

  private[scalastan] def typeName: String = s"${constraint.name}$emitBounds${constraint.emitSizes(rows, cols)}"
  private[scalastan] def getData(data: Seq[Seq[Double]]): Seq[String] = data.transpose.flatMap(_.map(_.toString))
  private[scalastan] def getDims(data: Seq[Seq[Double]]): Seq[Int] = {
    if (data.nonEmpty) {
      Seq(data.length, data.head.length)
    } else {
      Seq(0, 0)
    }
  }

  override private[scalastan] def getIndices: Seq[StanValue[StanInt]] = Seq(rows, cols)

  private[scalastan] def parse(name: String, values: Map[String, String]): Seq[Seq[Double]] ={
    // Determine the size of the matrix.
    val prefix1 = s"$name."
    val dim1Keys = values.keys.filter(_.startsWith(prefix1))
    if (dim1Keys.isEmpty) {
      Vector.empty // Empty matrix
    } else {
      val dim1 = dim1Keys.map(_.drop(prefix1.length).takeWhile(Character.isDigit).toInt).max
      val prefix2 = s"$name.1."
      val dim2 = dim1Keys.filter(_.startsWith(prefix2)).map(_.drop(prefix2.length).takeWhile(Character.isDigit).toInt).max
      Vector.tabulate[Vector[Double]](dim1) { i =>
        Vector.tabulate[Double](dim2) { j =>
          val innerName = s"$name.${i + 1}.${j + 1}"
          values(innerName).toDouble
        }
      }
    }
  }

  private[scalastan] def parse(dims: Seq[Int], values: Seq[String]): Seq[Seq[Double]] = {
    require(dims.length == 2, s"matrix must have dimensionality of 2, got ${dims.length}")
    val innerSize = dims(1)
    Vector.tabulate[Vector[Double]](dims.head) { i =>
      values.slice(i * innerSize, i * innerSize + innerSize).map(_.toDouble).toVector
    }
  }

  private[scalastan] def combine(
    values: Seq[Seq[Seq[Seq[Double]]]]
  )(func: Seq[Seq[Double]] => Double): Seq[Seq[Double]] = {
    values.head.head.indices.map { i =>
      values.head.head.head.indices.map { j =>
        func(values.map(_.map(v => v(i)(j))))
      }.toVector
    }.toVector
  }

  final private[scalastan] def next: NEXT_TYPE = StanVector(cols, lower, upper)
}
