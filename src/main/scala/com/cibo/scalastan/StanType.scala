package com.cibo.scalastan

sealed trait StanType {

  // Our type (for apply).
  type THIS_TYPE <: StanType

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

  // Parse data from the iterations CSV.
  def parse(name: String, values: Map[String, String]): SCALA_TYPE

  // Parse data from a vector of dimensions and values.
  def parse(dims: Seq[Int], values: Seq[String]): SCALA_TYPE

  // Combine values using the specified function.
  // The function takes a seq of chain -> iteration -> value
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): SUMMARY_TYPE

  // Emit a full Stan declaration for this type given the specified name.
  final def emitDeclaration(name: String): String = {
    val dims = emitDims.mkString(",")
    if (dims.nonEmpty) s"$typeName $name[$dims]" else s"$typeName $name"
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
}

trait StanCompoundType extends StanType
trait StanVectorOrMatrix extends StanCompoundType

case class StanVoid private[scalastan] (
  lower: Option[StanValue[StanVoid]] = None,
  upper: Option[StanValue[StanVoid]] = None
) extends StanType {
  type THIS_TYPE = StanVoid
  type ELEMENT_TYPE = StanVoid
  type NEXT_TYPE = StanVoid
  type REAL_TYPE = StanReal
  type SCALA_TYPE = Unit
  type SUMMARY_TYPE = Unit
  def typeName: String = "void"
  def getData(data: Unit): Seq[String] = Seq.empty
  def getDims(data: Unit): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): Unit = ()
  def parse(dims: Seq[Int], values: Seq[String]): Unit = ()
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): Unit = ()
}

case class StanString private[scalastan] () extends StanType {
  type THIS_TYPE = StanString
  type ELEMENT_TYPE = StanString
  type REAL_TYPE = StanReal
  type SCALA_TYPE = String
  type SUMMARY_TYPE = String
  val lower: Option[StanValue[StanString]] = None
  val upper: Option[StanValue[StanString]] = None
  def typeName: String = "string"
  def getData(data: String): Seq[String] = Seq(data)
  def getDims(data: String): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): String = values(name)
  def parse(dims: Seq[Int], values: Seq[String]): String = ""
  def combine(values: Seq[Seq[SCALA_TYPE]])(func: Seq[Seq[Double]] => Double): String = ""
}

abstract class StanDiscreteType extends StanScalarType {
  def typeName: String = s"int$emitBounds"
}

case class StanInt private[scalastan] (
  lower: Option[StanValue[StanInt]] = None,
  upper: Option[StanValue[StanInt]] = None
) extends StanDiscreteType {
  type THIS_TYPE = StanInt
  type ELEMENT_TYPE = StanInt
  type SCALA_TYPE = Int
  def getDims(data: Int): Seq[Int] = Seq.empty
  def getData(data: Int): Seq[String] = Seq(data.toString)
  def parse(name: String, values: Map[String, String]): Int = values(name).toInt
  def parse(dims: Seq[Int], values: Seq[String]): Int = values.head.toInt
  def combine(values: Seq[Seq[Int]])(func: Seq[Seq[Double]] => Double): Double = func(values.map(_.map(_.toDouble)))
}

case class StanCategorical private[scalastan] () extends StanDiscreteType {
  type THIS_TYPE = StanCategorical
  type ELEMENT_TYPE = StanCategorical
  type SCALA_TYPE = String

  val lower: Option[StanValue[ELEMENT_TYPE]] = None
  val upper: Option[StanValue[ELEMENT_TYPE]] = None

  private val categories = scala.collection.mutable.Map[String, Int]()

  private def lookup(value: String): Int = categories.getOrElseUpdate(value, categories.size)

  def getDims(data: String): Seq[Int] = Seq.empty
  def getData(data: String): Seq[String] = Seq(lookup(data).toString)
  def parse(name: String, values: Map[String, String]): String = values(name)
  def parse(dims: Seq[Int], values: Seq[String]): String = values.head
  def combine(values: Seq[Seq[String]])(func: Seq[Seq[Double]] => Double): Double =
    func(values.map(_.map(v => lookup(v).toDouble)))
}

case class StanArray[CONTAINED <: StanType] private[scalastan] (
  dim: StanValue[StanInt],
  inner: CONTAINED
) extends StanCompoundType {
  type THIS_TYPE = StanArray[CONTAINED]
  type ELEMENT_TYPE = CONTAINED#ELEMENT_TYPE
  type NEXT_TYPE = CONTAINED
  type REAL_TYPE = StanArray[CONTAINED#REAL_TYPE]
  type SCALA_TYPE = Seq[CONTAINED#SCALA_TYPE]
  type SUMMARY_TYPE = Seq[CONTAINED#SUMMARY_TYPE]

  val lower: Option[StanValue[CONTAINED#ELEMENT_TYPE]] = inner.lower.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]
  val upper: Option[StanValue[CONTAINED#ELEMENT_TYPE]] = inner.upper.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]

  override def emitDims: Seq[String] = dim.emit +: inner.emitDims
  override def getIndices: Seq[StanValue[StanInt]] = dim +: inner.getIndices
  def typeName: String = inner.typeName
  def getData(data: SCALA_TYPE): Seq[String] = data.flatMap(d => inner.getData(d.asInstanceOf[inner.SCALA_TYPE]))
  def getDims(data: SCALA_TYPE): Seq[Int] = data.length +: inner.getDims(data.head.asInstanceOf[inner.SCALA_TYPE])
  def parse(name: String, values: Map[String, String]): SCALA_TYPE = {
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
    values.transpose.map { v =>
      inner.combine(v.asInstanceOf[Seq[Seq[inner.SCALA_TYPE]]])(func)
    }.toVector
  }
}

case class StanReal private[scalastan] (
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanScalarType {
  type THIS_TYPE = StanReal
  type ELEMENT_TYPE = StanReal
  type SCALA_TYPE = Double
  def typeName: String = s"real$emitBounds"
  def getData(data: Double): Seq[String] = Seq(data.toString)
  def getDims(data: Double): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): Double = values(name).toDouble
  def parse(dims: Seq[Int], values: Seq[String]): Double = values.head.toDouble
  def combine(values: Seq[Seq[Double]])(func: Seq[Seq[Double]] => Double): Double = func(values)
}

trait StanVectorLike extends StanVectorOrMatrix {
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanReal
  type REAL_TYPE = THIS_TYPE
  type SCALA_TYPE = Seq[Double]
  type SUMMARY_TYPE = Seq[Double]

  val dim: StanValue[StanInt]

  override def getIndices: Seq[StanValue[StanInt]] = Seq(dim)

  def getData(data: Seq[Double]): Seq[String] = data.map(_.toString)
  def getDims(data: Seq[Double]): Seq[Int] = Seq(data.length)

  def parse(name: String, values: Map[String, String]): Seq[Double] = {
    val prefix = s"$name."
    values.filterKeys(_.startsWith(prefix)).toVector.map { case (key, value) =>
      key.slice(prefix.length, key.length).toInt -> value
    }.sortBy(_._1).map(_._2.toDouble)
  }

  def parse(dims: Seq[Int], values: Seq[String]): Seq[Double] = values.take(dims.head).map(_.toDouble).toVector

  def combine(values: Seq[Seq[Seq[Double]]])(func: Seq[Seq[Double]] => Double): Seq[Double] = {
    values.map(_.transpose).transpose.map { v =>
      func(v)
    }.toVector
  }
}

case class StanVector private[scalastan] (
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  type THIS_TYPE = StanVector
  def typeName: String = s"vector$emitBounds[${dim.emit}]"
}

case class StanRowVector private[scalastan] (
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  type THIS_TYPE = StanRowVector
  def typeName: String = s"row_vector$emitBounds[${dim.emit}]"
}

case class StanMatrix private[scalastan] (
  rows: StanValue[StanInt],
  cols: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorOrMatrix {
  type THIS_TYPE = StanMatrix
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanVector
  type REAL_TYPE = StanMatrix
  type SCALA_TYPE = Seq[Seq[Double]]
  type SUMMARY_TYPE = Seq[Seq[Double]]

  def typeName: String = s"matrix$emitBounds[${rows.emit},${cols.emit}]"
  def getData(data: Seq[Seq[Double]]): Seq[String] = data.flatMap(_.map(_.toString))
  def getDims(data: Seq[Seq[Double]]): Seq[Int] = Seq(data.length, data.head.length)

  override def getIndices: Seq[StanValue[StanInt]] = Seq(rows, cols)

  def parse(name: String, values: Map[String, String]): Seq[Seq[Double]] ={
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

  def parse(dims: Seq[Int], values: Seq[String]): Seq[Seq[Double]] = {
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
}
