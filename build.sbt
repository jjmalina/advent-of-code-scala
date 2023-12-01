lazy val aoc = (project in file(".")).settings(
  organization := "com.jjmalina",
  name := "aoc",
  version := "0.1.0",
  scalaVersion := "3.3.1",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.10.0",
    "org.typelevel" %% "cats-effect" % "3.5.2",
    "co.fs2" %% "fs2-core" % "3.9.3",
    "org.typelevel" %% "cats-parse" % "0.3.9",
    "org.http4s" %% "http4s-core" % "1.0.0-M37",
    "org.http4s" %% "http4s-jdk-http-client" % "1.0.0-M7",
    "io.circe" %% "circe-core" % "0.14.6",
    "io.circe" %% "circe-generic" % "0.14.6",
    "io.circe" %% "circe-parser" % "0.14.6",
  ),
)
