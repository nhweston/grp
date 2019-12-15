ThisBuild/organization := "com.github.nhweston"
ThisBuild/scalaVersion := "2.13.1"
ThisBuild/scalacOptions := Seq (
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlog-implicits",
    "-language:implicitConversions"
)

lazy val root = (project in file (".")) .settings (
    name := "grp",
    libraryDependencies ++= Seq (
        "com.chuusai" %% "shapeless" % "2.3.3"
    )
)
