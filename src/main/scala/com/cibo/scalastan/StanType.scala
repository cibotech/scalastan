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

import com.cibo.scalastan.ast.{StanUnknownInt, StanValue}

sealed trait StanType {

  // Our type (for apply).
  protected type THIS_TYPE <: StanType

  // The type of elements for this type (real for vector, etc.)
  type ELEMENT_TYPE <: StanType

  // The type with one level dereferenced.
  type NEXT_TYPE <: StanType

  // The type with the same shape, but with StanReal elements.
  type REAL_TYPE <: StanType

  // The Scala analog to this type.
  type SCALA_TYPE

  // The Scala analog to this type used for summary statistics.
  type SUMMARY_TYPE

  // Get a copy of this type without constraints.
  def unconstrained: THIS_TYPE

  // Get a copy of this type that is real-valued.
  def realType: REAL_TYPE

  // Get the type constructor for the next type.
  def next: NEXT_TYPE

  // Get the type constructor for the element type.
  def element: ELEMENT_TYPE

  // Emit the name of this type (without bounds).
  def baseTypeName: String

  // Emit the name and bounds for this type.
  def typeName: String

  // Emit additional dimensions for this type (to be placed after the name in a declaration).
  // matrix[2, 3] name[this, and_this]
  def emitDims: Seq[String] = Seq.empty

  // Get declarations for indices.
  def getIndices: Seq[StanValue[StanInt]] = Seq.empty

  // Lower bound on each element.
  val lower: Option[StanValue[ELEMENT_TYPE]]

  // Upper bound on each element.
  val upper: Option[StanValue[ELEMENT_TYPE]]

  // Get the data and dimensions for this type to be used by Stan.
  // These are used by the emitData function below.
  def getData(data: SCALA_TYPE): Seq[String]
  def getDims(data: SCALA_TYPE): Seq[Int]

  // Emit the data in R format.
  final def emitData(data: SCALA_TYPE): String = {
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

  protected def mapResults(
    parameterChains: Map[String, Vector[Vector[Double]]],
    f: (Int, Int) => SCALA_TYPE
  ): Vector[Vector[SCALA_TYPE]] = {
    parameterChains.head._2.zipWithIndex.map { case (chain, chainIndex) =>
      Vector.range(0, chain.size).map { iterationIndex =>
        f(chainIndex, iterationIndex)
      }
    }
  }

  // Parse data from the iterations CSV.
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[SCALA_TYPE]]

  // Parse data from a vector of dimensions and values.
  def parse(dims: Seq[Int], values: Seq[String]): SCALA_TYPE

  // Combine values using the specified function.
  // The function takes a seq of chain -> iteration -> value
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): SUMMARY_TYPE

  // Emit a full Stan declaration for this type given the specified name.
  final def emitDeclaration(name: String): String = {
    val dims = emitDims
    if (dims.nonEmpty) {
      val dimString = dims.mkString(",")
      s"$typeName $name[$dimString]"
    } else {
      s"$typeName $name"
    }
  }

  final def emitFunctionDeclaration: String = {
    val dims = emitDims
    if (dims.nonEmpty) {
      val dimString = "," * (dims.length - 1)
      s"$baseTypeName[$dimString]"
    } else {
      baseTypeName
    }
  }

  final def emitFunctionDeclaration(name: String): String = {
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
  final def isDerivedFromData: Boolean =
    lower.forall(_.isDerivedFromData) && upper.forall(_.isDerivedFromData) && getIndices.forall(_.isDerivedFromData)

  def apply(): StanArray[THIS_TYPE] = {
    StanArray(StanUnknownInt, this.asInstanceOf[THIS_TYPE])
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
  type SUMMARY_TYPE = Double
  type NEXT_TYPE = StanVoid
  type REAL_TYPE = StanReal
  def realType: REAL_TYPE = StanReal()
  def next: NEXT_TYPE = StanVoid()
}

trait StanCompoundType extends StanType
trait StanVectorOrMatrix extends StanCompoundType

case class StanVoid(
  lower: Option[StanValue[StanVoid]] = None,
  upper: Option[StanValue[StanVoid]] = None
) extends StanType {
  protected type THIS_TYPE = StanVoid
  type ELEMENT_TYPE = StanVoid
  type NEXT_TYPE = StanVoid
  type REAL_TYPE = StanReal
  type SCALA_TYPE = Unit
  type SUMMARY_TYPE = Unit
  def unconstrained: StanVoid = StanVoid()
  def realType: REAL_TYPE = StanReal()
  def next: StanVoid = this
  def element: StanVoid = this
  def baseTypeName: String = "void"
  def typeName: String = baseTypeName
  def getData(data: Unit): Seq[String] = Seq.empty
  def getDims(data: Unit): Seq[Int] = Seq.empty
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[Unit]] = parameterChains(name).map(_.map( _ => () ))
  def parse(dims: Seq[Int], values: Seq[String]): Unit = ()
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): Unit = ()
}

case class StanString() extends StanType {
  protected type THIS_TYPE = StanString
  type ELEMENT_TYPE = StanString
  type NEXT_TYPE = StanVoid
  type REAL_TYPE = StanReal
  type SCALA_TYPE = String
  type SUMMARY_TYPE = String
  val lower: Option[StanValue[StanString]] = None
  val upper: Option[StanValue[StanString]] = None
  def unconstrained: StanString = StanString()
  def realType: REAL_TYPE = StanReal()
  def next: NEXT_TYPE = StanVoid()
  def element: StanString = this
  def baseTypeName: String = "string"
  def typeName: String = baseTypeName
  def getData(data: String): Seq[String] = Seq(s""""$data"""")
  def getDims(data: String): Seq[Int] = Seq.empty
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[String]] = throw new IllegalStateException("parse should not be called on a string")
  def parse(dims: Seq[Int], values: Seq[String]): String = ""
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): String = ""
}

abstract class StanDiscreteType extends StanScalarType {
  def baseTypeName: String = "int"
  def typeName: String = s"$baseTypeName$emitBounds"
}

case class StanInt(
  lower: Option[StanValue[StanInt]] = None,
  upper: Option[StanValue[StanInt]] = None
) extends StanDiscreteType {
  protected type THIS_TYPE = StanInt

  type ELEMENT_TYPE = StanInt
  type SCALA_TYPE = Int
  def unconstrained: StanInt = copy(lower = None, upper = None)
  def element: StanInt = this
  def getDims(data: Int): Seq[Int] = Seq.empty
  def getData(data: Int): Seq[String] = Seq(data.toString)
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[Int]] = parameterChains(name).map(_.map(_.toInt))
  def parse(dims: Seq[Int], values: Seq[String]): Int = {
    require(dims.isEmpty, s"int must have a dimensionality of 0, got ${dims.length}")
    values.head.toInt
  }
  def combine(values: Seq[Seq[Int]])(func: Seq[Seq[Double]] => Double): Double =
    func(values.map(_.map(_.toDouble)))
}

case class StanCategorical() extends StanDiscreteType {
  protected type THIS_TYPE = StanCategorical
  type SCALA_TYPE = String
  type ELEMENT_TYPE = StanCategorical

  val lower: Option[StanValue[ELEMENT_TYPE]] = None
  val upper: Option[StanValue[ELEMENT_TYPE]] = None

  def unconstrained: StanCategorical = this
  def element: StanCategorical = this

  private val categories = scala.collection.mutable.Map[String, Int]()

  private def lookup(value: String): Int = categories.getOrElseUpdate(value, categories.size)

  def getDims(data: String): Seq[Int] = Seq.empty
  def getData(data: String): Seq[String] = Seq(lookup(data).toString)
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[String]] = throw new IllegalStateException("parse should not be called on a categorical")
  def parse(dims: Seq[Int], values: Seq[String]): String = {
    require(dims.isEmpty, s"categorical must have a dimensionality of 0, got ${dims.length}")
    values.head
  }
  def combine(values: Seq[Seq[String]])(func: Seq[Seq[Double]] => Double): Double =
    func(values.map(_.map(v => lookup(v).toDouble)))
}

case class StanArray[CONTAINED <: StanType](
  dim: StanValue[StanInt],
  inner: CONTAINED
) extends StanCompoundType {
  type THIS_TYPE = StanArray[CONTAINED]
  type ELEMENT_TYPE = CONTAINED#ELEMENT_TYPE
  type NEXT_TYPE = CONTAINED
  type REAL_TYPE = StanArray[CONTAINED#REAL_TYPE]
  type SCALA_TYPE = Seq[CONTAINED#SCALA_TYPE]
  type SUMMARY_TYPE = Seq[CONTAINED#SUMMARY_TYPE]

  val lower: Option[StanValue[CONTAINED#ELEMENT_TYPE]] =
    inner.lower.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]
  val upper: Option[StanValue[CONTAINED#ELEMENT_TYPE]] =
    inner.upper.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]

  def unconstrained: THIS_TYPE = StanArray(dim, inner.unconstrained.asInstanceOf[CONTAINED])
  def element: ELEMENT_TYPE = inner.element
  def realType: REAL_TYPE = StanArray(dim, inner.realType)

  override def emitDims: Seq[String] = dim.emit +: inner.emitDims
  override def getIndices: Seq[StanValue[StanInt]] = dim +: inner.getIndices
  def baseTypeName: String = inner.baseTypeName
  def typeName: String = inner.typeName
  def getData(data: SCALA_TYPE): Seq[String] =
    data.map(d => inner.getData(d.asInstanceOf[inner.SCALA_TYPE])).transpose.flatten
  def getDims(data: SCALA_TYPE): Seq[Int] = {
    if (data.nonEmpty) {
      data.length +: inner.getDims(data.head.asInstanceOf[inner.SCALA_TYPE])
    } else if (inner.isInstanceOf[StanScalarType]) {
      Seq(data.length)
    } else {
      data.length +: inner.getDims(Seq.empty.asInstanceOf[inner.SCALA_TYPE])
    }
  }
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[SCALA_TYPE]] = {
    // The name will be the prefix up to the number we need to parse.
    val prefix = s"$name."
    val prefixLength = prefix.length

    // Get the size of this dimension (note that arrays in STAN are 1-based).
    val size = parameterChains.keys.filter(_.startsWith(prefix)).map(
      _.drop(prefixLength).takeWhile(Character.isDigit).toInt
    ).max

    // Parse the next level for each element of this array.
    val allValues = Vector.tabulate[Vector[Vector[CONTAINED#SCALA_TYPE]]](size) { i =>
      val nextName = s"$prefix${i + 1}"
      inner.parse(nextName, parameterChains)
    }
    mapResults(parameterChains, (chainIndex: Int, iterationIndex: Int) => {
      allValues.map(_(chainIndex)(iterationIndex))
    })
  }

  def parse(dims: Seq[Int], values: Seq[String]): SCALA_TYPE = {
    require(dims.nonEmpty)
    val length = dims.head       // The length of the outer-most vector
    val size = dims.tail.product // The number of elements contained in each dimension.
    Vector.tabulate[CONTAINED#SCALA_TYPE](length) { i =>
      inner.parse(dims.tail, values.drop(i * size))
    }
  }

  def combine(
    values: Seq[Seq[Seq[CONTAINED#SCALA_TYPE]]]
  )(
    func: Seq[Seq[Double]] => Double
  ): Seq[CONTAINED#SUMMARY_TYPE] = {
    values.map(_.transpose).transpose.map { v =>
      inner.combine(v.asInstanceOf[Seq[Seq[inner.SCALA_TYPE]]])(func)
    }.toVector
  }

  final def next: NEXT_TYPE = inner
}

case class StanReal(
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanScalarType {
  protected type THIS_TYPE = StanReal
  type ELEMENT_TYPE = StanReal
  type SCALA_TYPE = Double
  def unconstrained: StanReal = copy(lower = None, upper = None)
  def element: StanReal = this
  def baseTypeName: String = "real"
  def typeName: String = s"$baseTypeName$emitBounds"
  def getData(data: Double): Seq[String] = Seq(data.toString)
  def getDims(data: Double): Seq[Int] = Seq.empty
  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[Double]] = parameterChains(name)
  def parse(dims: Seq[Int], values: Seq[String]): Double = {
    require(dims.isEmpty, s"real must have a dimensionality of 0, got ${dims.length}")
    values.head.toDouble
  }
  def combine(values: Seq[Seq[Double]])(func: Seq[Seq[Double]] => Double): Double = func(values)
}

trait StanVectorLike extends StanVectorOrMatrix {
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanReal
  type REAL_TYPE = THIS_TYPE
  type SCALA_TYPE = Seq[Double]
  type SUMMARY_TYPE = Seq[Double]

  final def realType: REAL_TYPE = this.asInstanceOf[REAL_TYPE]
  final def element: ELEMENT_TYPE = StanReal()

  val dim: StanValue[StanInt]

  override def getIndices: Seq[StanValue[StanInt]] = Seq(dim)

  def getData(data: Seq[Double]): Seq[String] = data.map(_.toString)
  def getDims(data: Seq[Double]): Seq[Int] = Seq(data.length)

  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[Seq[Double]]] = {
    val prefix = s"$name."
    val sorted = parameterChains.keys.filter(_.startsWith(prefix)).toVector.sortBy(_.drop(prefix.length).toInt)
    mapResults(parameterChains, (chainIndex: Int, iterationIndex: Int) => {
      sorted.map(key => parameterChains(key)(chainIndex)(iterationIndex).toDouble)
    })
  }

  def parse(dims: Seq[Int], values: Seq[String]): Seq[Double] = {
    require(dims.length == 1, s"vector must have dimensionality of 1, got ${dims.length}")
    values.take(dims.head).map(_.toDouble).toVector
  }

  def combine(values: Seq[Seq[Seq[Double]]])(func: Seq[Seq[Double]] => Double): Seq[Double] = {
    values.map(_.transpose).transpose.map { v =>
      func(v)
    }.toVector
  }

  def typeName: String = s"$baseTypeName$emitBounds[${dim.emit}]"

  final def next: NEXT_TYPE = StanReal(lower, upper)
}

sealed abstract class VectorConstraint(val name: String)
object VectorConstraint {
  case object Default extends VectorConstraint("vector")
  case object Simplex extends VectorConstraint("simplex")
  case object UnitVector extends VectorConstraint("unit_vector")
  case object Ordered extends VectorConstraint("ordered")
  case object PositiveOrdered extends VectorConstraint("positive_ordered")
}

case class StanVector(
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None,
  constraint: VectorConstraint = VectorConstraint.Default
) extends StanVectorLike {
  protected type THIS_TYPE = StanVector
  def unconstrained: THIS_TYPE = copy(lower = None, upper = None)
  def baseTypeName: String = constraint.name
}

case class StanRowVector(
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  protected type THIS_TYPE = StanRowVector
  def unconstrained: THIS_TYPE = copy(lower = None, upper = None)
  def baseTypeName: String = "row_vector"
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
  case object CholeskyFactorCov extends MatrixConstraint("cholesky_factor_cov", 2)
}

case class StanMatrix(
  rows: StanValue[StanInt],
  cols: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None,
  constraint: MatrixConstraint = MatrixConstraint.Default
) extends StanVectorOrMatrix {
  protected type THIS_TYPE = StanMatrix
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanRowVector
  type REAL_TYPE = StanMatrix
  type SCALA_TYPE = Seq[Seq[Double]]
  type SUMMARY_TYPE = Seq[Seq[Double]]

  def unconstrained: THIS_TYPE = copy(lower = None, upper = None)
  def realType: REAL_TYPE = this
  def element: ELEMENT_TYPE = StanReal()

  def baseTypeName: String = constraint.name
  def typeName: String = s"$baseTypeName$emitBounds${constraint.emitSizes(rows, cols)}"
  def getData(data: Seq[Seq[Double]]): Seq[String] = data.transpose.flatMap(_.map(_.toString))
  def getDims(data: Seq[Seq[Double]]): Seq[Int] = {
    if (data.nonEmpty) {
      Seq(data.length, data.head.length)
    } else {
      Seq(0, 0)
    }
  }

  override def getIndices: Seq[StanValue[StanInt]] = Seq(rows, cols)

  def parse(
    name: String,
    parameterChains: Map[String, Vector[Vector[Double]]]
  ): Vector[Vector[Seq[Seq[Double]]]] = {
    // Determine the size of the matrix.
    val prefix1 = s"$name."
    val dim1Keys = parameterChains.keys.filter(_.startsWith(prefix1))
    if (dim1Keys.isEmpty) {
      Vector.empty // Empty matrix
    } else {
      val dim1 = dim1Keys.map(_.drop(prefix1.length).takeWhile(Character.isDigit).toInt).max
      val prefix2 = s"$name.1."
      val dim2 = dim1Keys.filter(_.startsWith(prefix2)).map(_.drop(prefix2.length).takeWhile(Character.isDigit).toInt).max

      mapResults(parameterChains, (chainIndex: Int, iterationIndex: Int) => {
        Vector.tabulate[Vector[Double]](dim1) { i =>
          Vector.tabulate[Double](dim2) { j =>
            val innerName = s"$name.${i + 1}.${j + 1}"
            parameterChains(innerName)(chainIndex)(iterationIndex).toDouble
          }
        }
      })
    }
  }

  def parse(dims: Seq[Int], values: Seq[String]): Seq[Seq[Double]] = {
    require(dims.length == 2, s"matrix must have dimensionality of 2, got ${dims.length}")
    val innerSize = dims(1)
    Vector.tabulate[Vector[Double]](dims.head) { i =>
      values.slice(i * innerSize, i * innerSize + innerSize).map(_.toDouble).toVector
    }
  }

  def combine(
    values: Seq[Seq[Seq[Seq[Double]]]]
  )(func: Seq[Seq[Double]] => Double): Seq[Seq[Double]] = {
    values.head.head.indices.map { i =>
      values.head.head.head.indices.map { j =>
        func(values.map(_.map(v => v(i)(j))))
      }.toVector
    }.toVector
  }

  final def next: NEXT_TYPE = StanRowVector(cols, lower, upper)
}
