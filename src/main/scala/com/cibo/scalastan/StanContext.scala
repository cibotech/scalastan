/*
 * Copyright (c) 2017 - 2023 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import com.typesafe.scalalogging.LazyLogging

trait StanContext extends LazyLogging {

  implicit val _context: StanContext = this

  private var idCounter: Int = 0

  private def nextId: Int = {
    synchronized {
      idCounter += 1
      idCounter
    }
  }

  private def listMembers(obj: Any): Seq[String] = {
    import scala.reflect.runtime.universe._
    val mirror = runtimeMirror(obj.getClass.getClassLoader)
    mirror.reflect(obj).symbol.info.decls.filter { d =>
      d.isPublic && !d.isConstructor && d.info.paramLists.head.nonEmpty
    }.map(_.name.toString).toSeq
  }

  // Stan keywords (section 4.2).
  private val keywords: Set[String] = Set(
    // Stan keywords (that aren't also C++ keywords).
    "in", "repeat", "until", "then",

    // Stan types (that aren't also C++ keywords).
    "real", "vector", "simplex", "unit_vector", "ordered", "positive_ordered", "row_vector", "matrix",
    "cholesky_factor_corr", "cholesky_factor_cov", "corr_matrix", "cov_matrix",

    // Stan block identifiers.
    "functions", "model", "data", "parameters", "quantities", "transformed", "generated",

    // Reserved words from the Stan implementation.
    "var", "fvar", "STAN_MAJOR", "STAN_MINOR", "STAN_PATCH", "STAN_MATH_MAJOR", "STAN_MATH_MINOR", "STAN_MATH_PATH",


    // C++ keywords.
    "alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break", "case", "catch",
    "char", "char16_t", "char32_t", "class", "compl", "const", "constexpr", "const_cast", "continue", "decltype",
    "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false",
    "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace", "new", "noexcept",
    "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected", "public", "register",
    "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
    "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union",
    "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
  )

  // Distribution functions.
  private val distributionSuffixes: Seq[String] = Seq(
    "_lpdf", "_lpmf", "_lcdf", "_lccdf", "_cdf", "_ccdf", "_log", "_cdf_log", "_ccdf_log"
  )
  private lazy val distributions: Seq[String] = {
    val obj = new StanDistributions {}
    listMembers(obj)
  }
  private lazy val distributionFunctions: Set[String] = {
    distributions.flatMap { dist =>
      distributionSuffixes.map { suffix =>
        s"$dist$suffix"
      }
    }.toSet
  }

  // Stan Functions
  private lazy val functions: Set[String] = {
    val obj = new StanFunctions {}
    listMembers(obj).toSet
  }

  private lazy val reservedNames: Set[String] = keywords ++ distributionFunctions ++ functions

  private def validName(name: String): Boolean = {
    val pattern = raw"[A-Za-z][A-Za-z0-9_]*".r
    name match {
      case pattern(_*) => !name.startsWith("ss_v") && !reservedNames.contains(name)
      case _ => false
    }
  }

  def newName: String = s"ss_v$nextId"

  def fixName(name: String): String = {
    if (validName(name)) name else {
      val n = newName
      logger.debug(s"'$name' not a valid variable name, using '$n'")
      n
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
