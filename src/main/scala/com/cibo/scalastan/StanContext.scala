package com.cibo.scalastan

trait StanContext {

  implicit val _context: StanContext = this

  private var idCounter: Int = 0

  private def nextId: Int = {
    synchronized {
      idCounter += 1
      idCounter
    }
  }

  def newName: String = s"ss_v$nextId"

  def fixName(name: String): String = {
    val pattern = raw"[A-Za-z][A-Za-z0-9_]*".r
    name match {
      case pattern(_*) if !name.startsWith("ss_v") => name
      case _                                       => newName
    }
  }

  def fixEnclosingName(name: String): String = {
    // These names have the form of:
    //    "com.cibo.scalastan.GeneratedQuantitySpec#$anon#model $anon#t $anon" or
    //    "com.cibo.scalastan.GeneratedQuantitySpec#$anon#t $anon"
    // where "t" is the name of interest.
    val parts = name.split(' ')
    if (parts.length > 1) {
      fixName(parts.dropRight(1).last.split('#').last.split('.').last)
    } else {
      newName
    }
  }
}
