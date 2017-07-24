
import sbt._
import sbt.Keys._
import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.SbtScalariform._
import xsbti.Predefined
import scalariform.formatter.preferences._
import com.github.retronym.SbtOneJar

object SpaceMattersBuild extends Build {
  def defaultSettings = scalariformSettings ++
    Seq (
      scalaVersion := "2.11.7",
      ScalariformKeys.preferences :=
        ScalariformKeys.preferences.value
          .setPreference(AlignSingleLineCaseStatements, true)
          .setPreference(RewriteArrowSymbols, true),
      organization := "fr.iscpif.spacematters",
      resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    )

  val monocleVersion = "1.1.1"

  lazy val model = Project("model", file("model"), settings = defaultSettings ++ osgiSettings) settings (
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",
    libraryDependencies += "com.nrinaudo" %% "scala-csv" % "0.1.1",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
      "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
      "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
    ),
    libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
    exportPackage := Seq("fr.iscpif.spacematters.*"),
    importPackage := Seq("*"),
    privatePackage := Seq("!scala.*", "*")
    )

  lazy val initialise = Project("initialise", file("initialise"), settings = defaultSettings) dependsOn(model) settings (
    libraryDependencies += "fr.iscpif" %% "mgo" % "1.81-SNAPSHOT"
    )
}

