/*
 * Copyright (c) 2017 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import scala.util.Try

protected trait NameLookup {
  // Subclasses should override "_userName" and set it to the result of lookupName from the right context.
  protected def _userName: Option[String]

  protected def _ss: ScalaStan

  // Unique ID for identifiers.
  protected lazy val _id: Int = _ss.nextId

  private lazy val defaultName: String = s"v${_id}"

  // A user-facing name to use for this identifier.
  lazy val name: String = _userName.getOrElse(defaultName)
}

protected object NameLookup {
  import scala.reflect.runtime.{universe => ru}

  private def findInInstance(obj: NameLookup, mirror: ru.InstanceMirror): Option[String] = {
    if (mirror.instance.getClass.isSynthetic || mirror.instance.getClass.isPrimitive) {
      None
    } else {
      Try {
        val scope = mirror.symbol.typeSignature.decls
        scope.find { decl =>
          if (decl.isMethod && decl.asMethod.isGetter && decl.asMethod.returnType <:< ru.typeOf[NameLookup]) {
            mirror.reflectMethod(decl.asMethod).apply().asInstanceOf[NameLookup]._id == obj._id
          } else {
            false
          }
        }.map(_.name.decodedName.toString).orElse {
          scope.view.filter { decl =>
            decl.isMethod && decl.asMethod.isGetter
          }.flatMap { decl =>
            val method = mirror.reflectMethod(decl.asMethod)
            val instanceOrNull = method.apply()
            Option(instanceOrNull).flatMap { instance =>
              val innerMirror = ru.runtimeMirror(instance.getClass.getClassLoader).reflect(instance)
              findInInstance(obj, innerMirror)
            }
          }.headOption
        }
      }.toOption.flatten
    }
  }

  // A list of valid characters to place in the generated Stan.
  private val validCharacters: Set[Char] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789".toSet

  private def fixName(name: String): Option[String] = {
    val filtered = name.filter(c => validCharacters.contains(c))
    if (filtered.nonEmpty) Some(filtered) else None
  }

  private[scalastan] def lookupName(obj: NameLookup)(implicit ss: ScalaStan): Option[String] = {
    val mirror = ru.runtimeMirror(ss.getClass.getClassLoader)
    val classLoader = ss.getClass.getClassLoader
    val instanceMirror = mirror.reflect(ss)
    findInInstance(obj, instanceMirror).flatMap(fixName)
  }
}
