organization  := "io.github.cibotech"
name          := "scalastan"

homepage      := Some(url("https://github.com/cibotech/ScalaStan"))
startYear     := Some(2017)
description   := "A Scala DSL for Stan."
licenses      += ("BSD Simplified", url("https://opensource.org/licenses/BSD-3-Clause"))

organizationName     := "CiBO Technologies, Inc."
organizationHomepage := Some(url("https://www.cibotechnologies.com"))

crossScalaVersions  := Seq("2.12.12")
scalaVersion        := crossScalaVersions.value.head

developers := List(
  Developer(
    id    = "CiBO",
    name  = "CiBO Technologies",
    email = "devops@cibotechnologies.com",
    url   = url("https://www.cibotechnologies.com")
  )
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/cibotech/scalastan"),
    "scm:git@github.com:cibotech/scalastan.git"
  )
)

pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.8")

fork := true

javaOptions += "-Xmx2G"

Defaults.itSettings
lazy val root = project.in(file(".")).configs(IntegrationTest)

libraryDependencies ++= Seq(
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.1.2",
  "org.scala-lang"             %  "scala-reflect"            % scalaVersion.value,
  "com.nrinaudo"               %% "kantan.csv"               % "0.6.0",
  "com.lihaoyi"                %% "sourcecode"               % "0.2.1",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.2",
  "org.scalatest"              %% "scalatest"                % "3.0.8" % "test,it",
  "ch.qos.logback"             % "logback-classic"           % "1.2.3" % "test,it"
)

