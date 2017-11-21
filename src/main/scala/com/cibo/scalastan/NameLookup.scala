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

  // A list of valid characters to place in the generated Stan.
  private val validCharacters: Set[Char] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789".toSet

  // A cleaned name to use in the generated Stan code.
  // This needs to be unique and valid.
  protected lazy val _internalName: String = _userName.map { un =>
    val cleanedName = un.filter(c => validCharacters.contains(c))
    s"${defaultName}_$cleanedName"
  }.getOrElse(defaultName)
}

protected object NameLookup {
  private[scalastan] def lookupName(obj: NameLookup)(implicit ss: ScalaStan): Option[String] = {
    Try {
      import scala.reflect.runtime.{universe => ru}
      val mirror = ru.runtimeMirror(ss.getClass.getClassLoader)
      val ssMirror = mirror.reflect(ss)
      ssMirror.symbol.typeSignature.decls.find { decl =>
        if (decl.isMethod && decl.asMethod.isGetter && decl.asMethod.returnType <:< ru.typeOf[NameLookup]) {
          ssMirror.reflectMethod(decl.asMethod).apply().asInstanceOf[NameLookup]._id == obj._id
        } else {
          false
        }
      }
    }.toOption.flatten.map { decl =>
      decl.name.decodedName.toString
    }
  }
}
