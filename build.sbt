name := "spire-ecm"

version := "0.1.0"

scalaVersion := "2.13.0"

val spireVersion = "0.17.0-M1"
val catsVersion = "2.0.0-M4"
val scodecVersion = "1.1.12"
val scalatestVersion = "3.1.0-SNAP13"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-Xlint"
)

  resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "spire" % spireVersion,
  "org.scodec" %% "scodec-bits" % scodecVersion,

  "org.scalatest" %% "scalatest" % scalatestVersion % "test"
)

//wartremoverErrors ++= Warts.unsafe
