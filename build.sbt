name := "spire-ecm"

version := "0.1.0"

scalaVersion := "2.12.2"

val catsVersion = "0.9.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % catsVersion,
  "org.typelevel" %% "spire" % "0.14.1"
)
