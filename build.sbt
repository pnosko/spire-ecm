name := "spire-ecm"

version := "0.1.0"

scalaVersion := "2.12.8"

val spireVersion = "0.16.2"
val catsVersion = "2.0.0-M4"
val scodecVersion = "1.1.6"
val scalatestVersion = "3.0.5"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "spire" % spireVersion,
  "org.scodec" %% "scodec-bits" % scodecVersion,

  "org.scalatest" %% "scalatest" % scalatestVersion % "test"
)
