/*
 * Copyright (c) 2017 - 2019 CiBO Technologies - All Rights Reserved
 * You may use, distribute, and modify this code under the
 * terms of the BSD 3-Clause license.
 *
 * A copy of the license can be found on the root of this repository,
 * at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
 * or at https://opensource.org/licenses/BSD-3-Clause
 */

package com.cibo.scalastan

import java.io.Writer

case class SHA() {

  private val md = java.security.MessageDigest.getInstance("SHA-1")

  def update(str: String): SHA = update(str.getBytes)

  def update(bytes: Array[Byte]): SHA = {
    md.update(bytes)
    this
  }

  def digest: String = {
    val hashBytes = md.digest.flatMap { b =>
      val digits = "0123456789abcdef"
      digits((b.toInt & 255) >> 4).toString + digits(b.toInt & 15).toString
    }
    new String(hashBytes)
  }
}

case class ShaWriter(writer: Writer) extends Writer {

  val sha = SHA()

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    sha.update(new String(cbuf.slice(off, len)).getBytes)
    writer.write(cbuf, off, len)
  }

  override def flush(): Unit = writer.flush()

  override def close(): Unit = writer.close()
}

object SHA {
  def hash(str: String): String = SHA().update(str).digest
}
