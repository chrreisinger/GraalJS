import sbt._
import Keys._
import Package.ManifestAttributes
import java.util.jar.Attributes.Name._

object GraalJSBuild extends Build {
  val dependencies = Seq(
    "org.mozilla" % "rhino" % "1.7R3"
  )

  val buildVersion = "0.1"
  val buildSettings = Defaults.defaultSettings ++ sbtassembly.Plugin.assemblySettings ++ SbtShPlugin.settings ++
    ProguardPlugin.proguardSettings ++ Seq(
    name := "GraalJS",
    organization := "com.github.chrreisinger",
    scalaVersion := "2.9.0-1",
    javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-g:vars", "-target:jvm-1.5", "-explaintypes"),
    scaladocOptions := Seq("-unchecked", "-doc-title", "GraalJS scaladoc", "-doc-version", buildVersion),
    version := buildVersion,
    packageOptions ++= Seq[PackageOption](ManifestAttributes(
      (IMPLEMENTATION_TITLE, "GraalJS"),
      (IMPLEMENTATION_URL, "https://github.com/chrreisinger/GraalJS"),
      (IMPLEMENTATION_VENDOR, "Christian Reisinger"),
      (IMPLEMENTATION_VERSION, buildVersion),
      (SEALED, "true"))
    ),
    libraryDependencies := dependencies,
    ProguardPlugin.makeInJarFilter <<= (ProguardPlugin.makeInJarFilter) {
      (makeInJarFilter) => {
        (file) => file match {
          case "scala-library.jar" => "!META-INF/**" + ",!library.properties"
          case _ => "!META-INF/**"
        }
      }
    },
    ProguardPlugin.proguardOptions ++= Seq(
      ProguardPlugin.keepMain("at.jku.ssw.graalJS.*"),
      ProguardPlugin.keepLimitedSerializability,
      "-keep class scala.ScalaObject",
      "-keepclassmembers class scala.Some,scala.Tuple*,scala.collection.** { *; }",
      "-keep class at.jku.ssw.openvs.** { *; }",
      "-dontskipnonpubliclibraryclasses",
      "-dontskipnonpubliclibraryclassmembers",
      "-dontnote",
      "-printconfiguration"
    )
  )

  lazy val common = Project("GraalJS", file("."), settings = buildSettings)
}


object SbtShPlugin extends Plugin {
  override lazy val settings = Seq(Keys.commands += shCommand)

  def shCommand = Command.args("sh", "<shell command>") {
    (state, args) =>
      args.mkString(" ").!
      state
  }

}
