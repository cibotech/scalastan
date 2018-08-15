organization  := "com.cibo"
name          := "scalastan"

homepage      := Some(url("https://github.com/cibotech/ScalaStan"))
startYear     := Some(2017)
description   := "A Scala DSL for Stan."
licenses      += ("BSD Simplified", url("https://opensource.org/licenses/BSD-3-Clause"))

organizationName     := "CiBO Technologies, Inc."
organizationHomepage := Some(url("https://www.cibotechnologies.com"))


crossScalaVersions  := Seq("2.12.4", "2.11.11")
scalaVersion        := crossScalaVersions.value.head

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

fork := true

javaOptions += "-Xmx2G"

Defaults.itSettings
lazy val root = project.in(file(".")).configs(IntegrationTest)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scala-lang"         %  "scala-reflect"            % scalaVersion.value,
  "org.scalatest"          %% "scalatest"                % "3.0.0" % "test,it",
  "com.nrinaudo"           %% "kantan.csv"               % "0.4.0",
  "com.lihaoyi"            %% "sourcecode"               % "0.1.4"
)
