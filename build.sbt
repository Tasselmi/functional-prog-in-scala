
lazy val scala32 = "3.2.2"
lazy val scala31 = "3.1.3"
lazy val scala213 = "2.13.8"
lazy val scala212 = "2.12.11"
lazy val supportedVersions = List(scala32, scala31, scala213, scala212)

name := "functional-prog-in-scala"

ThisBuild / scalaVersion := scala32

ThisBuild / version := "1.0"

lazy val root = (project in file("."))
  .aggregate(projScala3, projScala213)
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true,
    update / aggregate := false
  )

lazy val zioVersion = "2.0.10"
lazy val slf4jVer = "1.7.15"
lazy val log4jVer = "1.2.17"

lazy val projScala3 = (project in file("scala3")).settings(
  scalaVersion := scala32,
  scalacOptions ++= List("-feature", "-deprecation"),

  libraryDependencies ++= List(
    "org.scalameta" %% "munit" % "0.7.29" % Test,
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "org.apache.commons" % "commons-lang3" % "3.12.0",
    "org.springframework" % "spring-core" % "6.0.2",

    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-test" % zioVersion,
    "dev.zio" %% "zio-test-sbt" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-test-junit" % zioVersion,

    ("org.slf4j" % "slf4j-log4j12" % slf4jVer).withSources(),
    ("log4j" % "log4j" % log4jVer).withSources()
  ),

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

lazy val projScala213 = (project in file("scala213")).settings(
  scalaVersion := scala213,

  libraryDependencies ++= List(
    "org.scala-lang" % "scala-reflect" % scala213
  ),

  scalacOptions ++= List("-feature", "-deprecation")
)
