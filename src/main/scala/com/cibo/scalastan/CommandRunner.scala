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

import java.io.File

import com.typesafe.scalalogging.LazyLogging

trait CommandRunner extends LazyLogging {
  def runCommand(dir: File, command: Seq[String]): Int = {
    val pb = new ProcessBuilder(command: _*).directory(dir).redirectErrorStream(true)
    val process = pb.start()
    io.Source.fromInputStream(process.getInputStream).getLines().foreach { line =>
      logger.info(line)
    }
    process.waitFor()
  }
}
