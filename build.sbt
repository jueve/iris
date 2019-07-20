lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val libDependInScalaFx = Seq(
  libraryDependencies += "org.scalafx" %% "scalafx" % "11-R16",
  libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "11" classifier osName))

lazy val root = (project in file(".")).settings(
  name := "iris",
  scalaVersion := "2.12.8",
  libDependInScalaFx,
  libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  scalacOptions in Test ++= Seq("-Yrangepos"),
)