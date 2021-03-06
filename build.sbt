import com.typesafe.sbt.pgp.PgpKeys

import scala.xml.Elem
import scala.xml.transform.{RewriteRule, RuleTransformer}

addCommandAlias("ci", "; clean; test")

lazy val scalaTestVersion = "3.0.1"
lazy val disciplineVersion = "0.7.3"

lazy val sharedSettings = Seq(
  organization := "org.effects4s",

  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1"),

  scalacOptions ++= Seq(
    // warnings
    "-unchecked", // able additional warnings where generated code depends on assumptions
    "-deprecation", // emit warning for usages of deprecated APIs
    "-feature", // emit warning usages of features that should be imported explicitly
    // Features enabled by default
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    // possibly deprecated options
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible"
  ),

  // Shared sources for JVM / JS
  unmanagedSourceDirectories in Compile += {
    baseDirectory.value.getParentFile / "shared" / "src" / "main" / "scala"
  },
  unmanagedSourceDirectories in Test += {
    baseDirectory.value.getParentFile / "shared" / "src" / "test" / "scala"
  },

  // Force building with Java 8
  initialize := {
    val required = "1.8"
    val current  = sys.props("java.specification.version")
    assert(current == required,
      s"Unsupported build JDK: java.specification.version $current != $required "
    )
  },

  // Targeting Java 6, but only for Scala <= 2.11
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, majorVersion)) if majorVersion <= 11 =>
      // generates code with the Java 6 class format
      Seq("-target:jvm-1.6")
    case _ =>
      // For 2.12 we are targeting the Java 8 class format (the default)
      Seq.empty
  }),

  // Linter options, only enabled if Scala version >= 2.11
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, majorVersion)) if majorVersion >= 11 =>
      Seq(
        // Turns all warnings into errors ;-)
        "-Xfatal-warnings",
        // Enables linter options
        "-Xlint:adapted-args", // warn if an argument list is modified to match the receiver
        "-Xlint:nullary-unit", // warn when nullary methods return Unit
        "-Xlint:inaccessible", // warn about inaccessible types in method signatures
        "-Xlint:nullary-override", // warn when non-nullary `def f()' overrides nullary `def f'
        "-Xlint:infer-any", // warn when a type argument is inferred to be `Any`
        "-Xlint:missing-interpolator", // a string literal appears to be missing an interpolator id
        "-Xlint:doc-detached", // a ScalaDoc comment appears to be detached from its element
        "-Xlint:private-shadow", // a private field (or class parameter) shadows a superclass field
        "-Xlint:type-parameter-shadow", // a local type parameter shadows a type already in scope
        "-Xlint:poly-implicit-overload", // parameterized overloaded implicit methods are not visible as view bounds
        "-Xlint:option-implicit", // Option.apply used implicit view
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit
        "-Xlint:by-name-right-associative", // By-name parameter of right associative operator
        "-Xlint:package-object-classes", // Class or object defined in package object
        "-Xlint:unsound-match" // Pattern match may not be typesafe
      )
    case _ =>
      Seq.empty
  }),

  // ScalaDoc settings
  autoAPIMappings := true,
  scalacOptions in ThisBuild ++= Seq(
    // Note, this is used by the doc-source-url feature to determine the
    // relative path of a given source file. If it's not a prefix of a the
    // absolute path of the source file, the absolute path of that file
    // will be put into the FILE_SOURCE variable, which is
    // definitely not what we want.
    "-sourcepath", file(".").getAbsolutePath.replaceAll("[.]$", "")
  ),

  // Warn on unused imports, disabled for Scala 2.10, or
  // for playing in the console:
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 10)) =>
        Seq()
      case Some((2, n)) if n >= 11 =>
        Seq("-Ywarn-unused-import")
    }
  },
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},

  // -- Settings meant for deployment on oss.sonatype.org

  useGpg := true,
  useGpgAgent := true,
  usePgpKeyHex("F985F688"),

  publishMavenStyle := true,
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },

  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false }, // removes optional dependencies

  // For evicting Scoverage out of the generated POM
  // See: https://github.com/scoverage/sbt-scoverage/issues/153
  pomPostProcess := { (node: xml.Node) =>
    new RuleTransformer(new RewriteRule {
      override def transform(node: xml.Node): Seq[xml.Node] = node match {
        case e: Elem
          if e.label == "dependency" && e.child.exists(child => child.label == "groupId" && child.text == "org.scoverage") => Nil
        case _ => Seq(node)
      }
    }).transform(node).head
  },

  pomExtra :=
    <url>https://monix.io/</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>https://www.apache.org/licenses/LICENSE-2.0</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:monix/monix.git</url>
        <connection>scm:git:git@github.com:monix/monix.git</connection>
      </scm>
      <developers>
        <developer>
          <id>alexelcu</id>
          <name>Alexandru Nedelcu</name>
          <url>https://alexn.org</url>
        </developer>
      </developers>
)

lazy val doNotPublishArtifact = Seq(
  publishArtifact := false,
  publishArtifact in (Compile, packageDoc) := false,
  publishArtifact in (Compile, packageSrc) := false,
  publishArtifact in (Compile, packageBin) := false
)

lazy val unidocSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(coreJVM),

  scalacOptions in (ScalaUnidoc, unidoc) +=
    "-Xfatal-warnings",
  scalacOptions in (ScalaUnidoc, unidoc) -=
    "-Ywarn-unused-import",
  scalacOptions in (ScalaUnidoc, unidoc) ++=
    Opts.doc.title(s"Schrodinger"),
  scalacOptions in (ScalaUnidoc, unidoc) ++=
    Opts.doc.sourceUrl(s"https://github.com/effects4s/effects4s/tree/v${version.value}€{FILE_PATH}.scala"),
  scalacOptions in (ScalaUnidoc, unidoc) ++=
    Seq("-doc-root-content", file("rootdoc.txt").getAbsolutePath),
  scalacOptions in (ScalaUnidoc, unidoc) ++=
    Opts.doc.version(s"${version.value}")
)

lazy val effects4s = project.in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(coreJVM, coreJS, lawsJVM, lawsJS)
  .settings(sharedSettings)
  .settings(doNotPublishArtifact)
  .settings(unidocSettings)

lazy val coreCommon = sharedSettings ++ Seq(
  name := "effects4s-core"
)

lazy val coreJVM = project.in(file("./.jvm"))
  .settings(coreCommon)

lazy val coreJS = project.in(file("./.js"))
  .enablePlugins(ScalaJSPlugin)
  .settings(coreCommon)

lazy val lawsCommon = sharedSettings ++ Seq(
  name := "effects4s-laws",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "discipline" % disciplineVersion,
    // For TestScheduler
    "io.monix" %%% "monix-execution" % "2.2.4" % Test,
    "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
  )
)

lazy val lawsJVM = project.in(file("./laws/jvm"))
  .settings(lawsCommon)
  .dependsOn(coreJVM)

lazy val lawsJS = project.in(file("./laws/js"))
  .enablePlugins(ScalaJSPlugin)
  .settings(lawsCommon)
  .dependsOn(coreJS)