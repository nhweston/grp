ThisBuild/organization := "com.github.nhweston"
ThisBuild/scalaVersion := "2.13.1"
ThisBuild/scalacOptions := Seq (
    "-deprecation",
    "-feature",
    "-unchecked"
)

lazy val root = (project in file (".")) .settings (
    name := "grp",
    libraryDependencies ++= Seq ()
)

lazy val examples = (project in file ("./examples")) .settings (
    name := "examples",
    scalaSource in Compile := baseDirectory.value / "src",
    libraryDependencies ++= Seq()
) dependsOn root
