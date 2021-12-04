val scala3Version = "2.13.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0",
    libraryDependencies += "com.softwaremill.quicklens" %% "quicklens" % "1.7.5"
  )
