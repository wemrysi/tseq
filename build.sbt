name := "tseq"

organization := "org.estewei"

version := "0.0.1"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

// Resolvers
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

// Compile options
// http://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Yno-imports"
)

// Compile Dependencies
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalaz" %% "scalaz-iteratee" % "7.1.0",
  "com.storm-enroute" %% "scalameter" % "0.6" % Test
)

// Wartremover
wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.NoNeedForMonad)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
