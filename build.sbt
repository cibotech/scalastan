organization  := "com.cibo"
name          := "ScalaStan"
licenses      += ("BSD Simplified", url("https://opensource.org/licenses/BSD-3-Clause"))

version := "0.1-SNAPSHOT"

crossScalaVersions  := Seq("2.12.4", "2.11.11")
scalaVersion        := crossScalaVersions.value.head

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scala-lang"         % "scala-reflect"             % scalaVersion.value,
  "org.scalatest"          %% "scalatest"                % "3.0.0" % "test"
)
        
