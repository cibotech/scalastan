package com.cibo.scalastan

import scala.reflect.ClassTag

trait NameLookup {
  protected val _ctag: ClassTag[_]
  protected val _id: Int = NameLookup.nextId
}

object NameLookup {
  private var counter: Int = 0

  private def nextId: Int = {
    counter += 1
    counter
  }

  private[scalastan] def lookupName(obj: NameLookup)(implicit ss: ScalaStan): String = {
    ss.getClass.getDeclaredMethods.find { m =>
      m.getParameterCount == 0 &&
        m.getReturnType == obj._ctag.runtimeClass &&
        m.invoke(ss).asInstanceOf[NameLookup]._id == obj._id
    }.map(_.getName).getOrElse(s"v${obj._id}")
  }
}
