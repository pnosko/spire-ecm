name := "ecm"

version := "0.1.0"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.11" % "test",
  "org.scalatest" %% "scalatest" % "2.0" % "test->default",
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full,
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "org.spire-math" %% "spire" % "0.7.1"
)

// Use Java 7 (change to JavaSE16 if you don't have Java 7 installed)
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

// Adding src/main/resources etc. to the source entries, so Eclipse "compiles" them, i.e. copies them to the target
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

// In order to avoid Eclipse and sbt working on the same files: At least in theory there could be race conditions and such
EclipseKeys.eclipseOutput := Some(".target")

// Add source entries to library dependencies
EclipseKeys.withSource := true