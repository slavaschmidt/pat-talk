name := "pbt"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "jline" % "jline" % "2.14.3",

  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % Test exclude("junit", "junit-dep"),

  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",

  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "org.specs2" %% "specs2-core" % "3.8.8" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.8.8" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
