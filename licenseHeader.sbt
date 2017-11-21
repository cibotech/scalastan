headerLicense := Some(HeaderLicense.Custom({
  val dateRange = Seq(java.time.Year.now().toString).fold(startYear.value.get.toString){ (start, now) =>
    if (start == now) start else s"$start - $now"
  }

  f"""Copyright (c) $dateRange CiBO Technologies - All Rights Reserved
     |You may use, distribute, and modify this code under the
     |terms of the BSD 3-Clause license.
     |
      |A copy of the license can be found on the root of this repository,
     |at https://github.com/cibotech/ScalaStan/blob/master/LICENSE,
     |or at https://opensource.org/licenses/BSD-3-Clause
     |""".stripMargin
}))
