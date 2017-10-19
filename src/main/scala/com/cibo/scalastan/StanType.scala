package com.cibo.scalastan

sealed trait StanType {

  // The type of elements for this type (real for vector, etc.)
  type ELEMENT_TYPE <: StanType

  // The type with one level dereferenced.
  type NEXT_TYPE <: StanType

  // The Scala analog to this type.
  type SCALA_TYPE

  // The Scala analog to this type used for summary statistics.
  type SUMMARY_TYPE

  // Emit the name and bounds for this type.
  def typeName: String

  // Emit additional dimensions for this type (to be placed after the name in a declaration).
  // matrix[2, 3] name[this, and_this]
  def emitDims: Seq[String] = Seq.empty

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
      case n =>
        require(strs.length == dims.product)
        val cstr = strs.mkString("c(", ",", ")")
        val dstr = dims.mkString("c(", ",", ")")
        s"structure($cstr, .Dim = $dstr)"
    }
  }

  // Parse data from the iterations CSV.
  def parse(name: String, values: Map[String, String]): SCALA_TYPE

  // Parse data from a vector of dimensions and values.
  def parse(dims: Seq[Int], values: Seq[Double]): SCALA_TYPE

  // Combine values using the specified function.
  def combine(values: Seq[SCALA_TYPE])(func: Seq[Double] => Double): SUMMARY_TYPE

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
}

trait StanScalarType extends StanType {
  type SUMMARY_TYPE = Double
}

trait StanCompoundType extends StanType

case class StanVoid private[scalastan] (
  lower: Option[StanValue[StanVoid]] = None,
  upper: Option[StanValue[StanVoid]] = None
) extends StanType {
  type ELEMENT_TYPE = StanVoid
  type NEXT_TYPE = StanVoid
  type SCALA_TYPE = Unit
  type SUMMARY_TYPE = Unit
  def typeName: String = "void"
  def getData(data: Unit): Seq[String] = Seq.empty
  def getDims(data: Unit): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): Unit = ()
  def parse(dims: Seq[Int], values: Seq[Double]): Unit = ()
  def combine(values: Seq[Unit])(func: Seq[Double] => Double): Unit = ()
}

case class StanInt private[scalastan] (
  lower: Option[StanValue[StanInt]] = None,
  upper: Option[StanValue[StanInt]] = None
) extends StanScalarType {
  type ELEMENT_TYPE = StanInt
  type NEXT_TYPE = StanVoid
  type SCALA_TYPE = Int
  def typeName: String = s"int$emitBounds"
  def getData(data: Int): Seq[String] = Seq(data.toString)
  def getDims(data: Int): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): Int = values(name).toInt
  def parse(dims: Seq[Int], values: Seq[Double]): Int = values.head.toInt
  def combine(values: Seq[Int])(func: Seq[Double] => Double): Double = func(values.map(_.toDouble))
}

case class StanArray[CONTAINED <: StanType] private[scalastan] (
  dim: StanValue[StanInt],
  inner: CONTAINED
) extends StanCompoundType {
  type ELEMENT_TYPE = CONTAINED#ELEMENT_TYPE
  type NEXT_TYPE = CONTAINED
  type SCALA_TYPE = Vector[CONTAINED#SCALA_TYPE]
  type SUMMARY_TYPE = Vector[CONTAINED#SUMMARY_TYPE]

  val lower: Option[StanValue[CONTAINED#ELEMENT_TYPE]] = inner.lower.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]
  val upper: Option[StanValue[CONTAINED#ELEMENT_TYPE]] = inner.upper.asInstanceOf[Option[StanValue[CONTAINED#ELEMENT_TYPE]]]

  override def emitDims: Seq[String] = dim.emit +: inner.emitDims
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

  def parse(dims: Seq[Int], values: Seq[Double]): SCALA_TYPE = {
    require(dims.nonEmpty)
    val length = dims.head       // The length of the outer-most vector
    val size = dims.tail.product // The number of elements contained in each dimension.
    Vector.tabulate[CONTAINED#SCALA_TYPE](length) { i =>
      inner.parse(dims.tail, values.drop(i * size))
    }
  }

  def combine(values: Seq[Vector[CONTAINED#SCALA_TYPE]])(func: Seq[Double] => Double): Vector[CONTAINED#SUMMARY_TYPE] = {
    values.transpose.map { v =>
      inner.combine(v.asInstanceOf[Seq[inner.SCALA_TYPE]])(func)
    }.toVector
  }
}

case class StanReal private[scalastan] (
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanScalarType {
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanVoid
  type SCALA_TYPE = Double
  def typeName: String = s"real$emitBounds"
  def getData(data: Double): Seq[String] = Seq(data.toString)
  def getDims(data: Double): Seq[Int] = Seq.empty
  def parse(name: String, values: Map[String, String]): Double = values(name).toDouble
  def parse(dims: Seq[Int], values: Seq[Double]): Double = values.head
  def combine(values: Seq[Double])(func: Seq[Double] => Double): Double = func(values)
}

trait StanVectorLike extends StanCompoundType {
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanReal
  type SCALA_TYPE = Vector[Double]
  type SUMMARY_TYPE = Vector[Double]

  def getData(data: Vector[Double]): Seq[String] = data.map(_.toString)
  def getDims(data: Vector[Double]): Seq[Int] = Seq(data.length)

  def parse(name: String, values: Map[String, String]): Vector[Double] = {
    val prefix = s"$name."
    values.filterKeys(_.startsWith(prefix)).toVector.map { case (key, value) =>
      key.slice(prefix.length, key.length).toInt -> value
    }.sortBy(_._1).map(_._2.toDouble)
  }

  def parse(dims: Seq[Int], values: Seq[Double]): Vector[Double] = values.take(dims.head).toVector

  def combine(values: Seq[Vector[Double]])(func: Seq[Double] => Double): Vector[Double] = {
    values.transpose.map { v =>
      func(v)
    }.toVector
  }
}

case class StanVector private[scalastan] (
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  def typeName: String = s"vector$emitBounds[${dim.emit}]"
}

case class StanRowVector private[scalastan] (
  dim: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanVectorLike {
  def typeName: String = s"row_vector$emitBounds[${dim.emit}]"
}

case class StanMatrix private[scalastan] (
  rows: StanValue[StanInt],
  cols: StanValue[StanInt],
  lower: Option[StanValue[StanReal]] = None,
  upper: Option[StanValue[StanReal]] = None
) extends StanCompoundType {
  type ELEMENT_TYPE = StanReal
  type NEXT_TYPE = StanVector
  type SCALA_TYPE = Vector[Vector[Double]]
  type SUMMARY_TYPE = Vector[Vector[Double]]

  def typeName: String = s"matrix$emitBounds[${rows.emit},${cols.emit}]"
  def getData(data: Vector[Vector[Double]]): Seq[String] = data.flatMap(_.map(_.toString))
  def getDims(data: Vector[Vector[Double]]): Seq[Int] = Seq(data.length, data.head.length)

  def parse(name: String, values: Map[String, String]): Vector[Vector[Double]] ={
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

  def parse(dims: Seq[Int], values: Seq[Double]): Vector[Vector[Double]] = {
    val innerSize = dims(1)
    Vector.tabulate[Vector[Double]](dims.head) { i =>
      values.slice(i * innerSize, i * innerSize + innerSize).toVector
    }
  }

  def combine(values: Seq[Vector[Vector[Double]]])(func: Seq[Double] => Double): Vector[Vector[Double]] = {
    values.head.indices.map { i =>
      values.head.head.indices.map { j =>
        func(values.map(v => v(i)(j)))
      }.toVector
    }.toVector
  }
}
