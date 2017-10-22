package com.cibo.scalastan

import scala.reflect.ClassTag

trait NameLookup {
  protected val _ctag: ClassTag[_]
  protected val _id: Int = NameLookup.nextId
  protected lazy val _internalName: String = s"v${_id}"
}

object NameLookup {
  private var counter: Int = 0

  private def nextId: Int = {
    synchronized {
      counter += 1
      counter
    }
  }

  private[scalastan] def lookupName(obj: NameLookup)(implicit ss: ScalaStan): String = {
    ss.getClass.getDeclaredMethods.find { m =>
      if (m.getParameterCount == 0 && m.getReturnType == obj._ctag.runtimeClass) {
        m.setAccessible(true)
        m.invoke(ss).asInstanceOf[NameLookup]._id == obj._id
      } else {
        false
      }
    }.map { m =>
      m.getName.split("\\$").last
    }.getOrElse(s"v${obj._id}")
  }
}
