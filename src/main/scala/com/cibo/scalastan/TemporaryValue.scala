package com.cibo.scalastan

private sealed trait TemporaryValue[T <: StanType] {
  def create(args: StanValue[StanInt]*): T
}

private object TemporaryValue {

  implicit val intTemporary: TemporaryValue[StanInt] = new TemporaryValue[StanInt] {
    def create(args: StanValue[StanInt]*): StanInt = StanInt()
  }

  implicit val vectorTemporary: TemporaryValue[StanVector] = new TemporaryValue[StanVector] {
    def create(args: StanValue[StanInt]*): StanVector = StanVector(args(0))
  }
}
