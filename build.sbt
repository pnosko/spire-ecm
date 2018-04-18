name := "spire-ecm"

version := "0.1.0"

scalaVersion := "2.12.2"

val catsVersion = "1.0.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "spire" % "0.14.1",
  "org.scodec" %% "scodec-bits" % "1.1.5",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
