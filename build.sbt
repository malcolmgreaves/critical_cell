// use sbt-dev-settings to configure
import com.nitro.build._
import PublishHelpers._

// GAV
//
lazy val pName  = "critical_cell"
lazy val semver = SemanticVersion(0, 0, 0, isSnapshot = false)
organization   := "io.malcolmgreaves"
name           := pName
version        := semver.toString

// compilation & runtime
//
lazy val devConfig = {
  import CompileScalaJava._
  Config.default
}
scalaVersion := "2.11.7"
CompileScalaJava.librarySettings(devConfig)
javaOptions := JvmRuntime.settings(devConfig.jvmVer)

// dependencies and their resolvers
//
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % Test
)
resolvers := Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// publishing settings
//
Publish.settings(
  repo = Repository.github("malcolmgreaves", pName),
  developers =
    Seq(
      Developer("mgreaves", "Malcolm Greaves", "greaves.malcolm@gmail.com", new URL("https", "github.com", "/malcolmgreaves"))
    ),
  art = ArtifactInfo.sonatype(semver),
  lic = License.apache20
)

// unit test configuration
//
fork in Test              := false
parallelExecution in Test := true
