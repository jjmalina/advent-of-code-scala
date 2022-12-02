lazy val aoc = (project in file(".")).settings(
  organization := "com.jjmalina",
  name := "aoc",
  version := "0.1.0",
  scalaVersion := "3.2.1",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-effect" % "3.4.2",
    "co.fs2" %% "fs2-core" % "3.4.0",
    "org.typelevel" %% "cats-parse" % "0.3.8",
    "org.http4s" %% "http4s-core" % "1.0.0-M37",
    "org.http4s" %% "http4s-jdk-http-client" % "1.0.0-M7",
  ),
)