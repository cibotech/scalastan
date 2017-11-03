package com.cibo.scalastan

import java.io.Writer

private case class SHA() {

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

private case class ShaWriter(writer: Writer) extends Writer {

  val sha = SHA()

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    sha.update(new String(cbuf.slice(off, len)).getBytes)
    writer.write(cbuf, off, len)
  }

  override def flush(): Unit = writer.flush()

  override def close(): Unit = writer.close()

  def println(str: String): Unit = write(s"$str\n")
}

private object SHA {
  def hash(str: String): String = SHA().update(str).digest
}
