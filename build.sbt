name := "spire-ecm"

version := "0.1.0"

scalaVersion := "2.12.8"

val catsVersion = "2.0.0-M4"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "spire" % "0.16.2",

  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
