name := "Functional Countdown"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "org.scalaz" %% "scalaz-effect" % "7.0.0",
  "org.scalaz" %% "scalaz-typelevel" % "7.0.0"
)

mainClass in (Compile, run) := Some("cntdn.NumbersGame")




