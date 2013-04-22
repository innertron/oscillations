import sbt._
import sbt.Keys._

object RatDataSimBuild extends Build {

  lazy val ratDataSim = Project(
    id = "rat-data-sim",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Rat Data Sim",
      organization := "edu.uccs",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      resolvers ++= Seq(
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"),
      libraryDependencies ++= Seq(
        "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
        "org.scalanlp" %% "breeze-learn" % "0.2-SNAPSHOT",
        "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT",
        "org.scalanlp" %% "breeze-viz" % "0.2-SNAPSHOT"
      )
    )
  )

}
