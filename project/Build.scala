
import sbt._
import sbt.Keys._
import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import com.github.retronym.SbtOneJar

object SpaceMattersBuild extends Build {
  def defaultSettings = scalariformSettings ++
    Seq (
      scalaVersion := "2.11.6",
      ScalariformKeys.preferences :=
        ScalariformKeys.preferences.value
          .setPreference(AlignSingleLineCaseStatements, true)
          .setPreference(RewriteArrowSymbols, true),
      organization := "fr.iscpif.spacematters"
    )

  lazy val model = Project("model", file("model"), settings = defaultSettings)
  lazy val initialise = Project("initialise", file("initialise"), settings = defaultSettings) dependsOn(model) settings (
    libraryDependencies += "fr.iscpif" %% "mgo" % "1.79"
    )
}

