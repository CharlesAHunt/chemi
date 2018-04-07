import scala.io.Source

lazy val buildSettings = Seq(
  name := "chemi",
  organization := "com.cornfluence",
  scalaVersion := "2.12.5",
  description := "Computational Chemistry",
  version := "0.1.0"
)

parallelExecution in Test := false

lazy val chemi = project.in(file("."))
  .settings(moduleName := "root")
  .settings(chemiSettings)
  .settings(publishArtifact := false)
  .settings(commonJvmSettings)
  .aggregate(core, server)
  .dependsOn(core, server)

lazy val core = project.in(file("core"))
  .settings(moduleName := "chemi-core")
  .settings(libraryDependencies ++= deps)
  .settings(chemiSettings ++ commonJvmSettings:_*)

lazy val server = project.in(file("server"))
  .settings(moduleName := "chemi-server")
  .settings(libraryDependencies ++= deps)
  .settings(chemiSettings ++ commonJvmSettings:_*)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
) ++ prompt

def disableTests = Seq(
  test := {},
  testQuick := {},
  testOnly := {}
)

lazy val commonJvmSettings = Seq(
  fork in Test := true,
  cancelable in Global := true,
  (scalacOptions in Test) ~= (_.filterNot(_ == "-Xfatal-warnings"))
) ++ Seq(scalacOptions in Test ++= Seq("-Yrangepos"))

lazy val chemiSettings =
  buildSettings ++ commonSettings ++ publishSettings

lazy val publishSettings =
  Seq(
  homepage := Some(url("https://github.com/CharlesAHunt/chemi")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(ScmInfo(url("https://github.com/CharlesAHunt/chemi"), "scm:git@github.com:CharlesAHunt/chemi.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://cornfluence.com/chemi/api/")),
  pomExtra := (
      <developers>
        <developer>
          <id>CharlesAHunt</id>
          <name>Charles A Hunt</name>
          <url>http://www.cornfluence.com</url>
        </developer>
      </developers>
    )
) ++ credentialSettings ++ sharedPublishSettings

lazy val commonScalacOptions = Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xfuture",                          // Turn on future language features.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",              // Pattern match may not be typesafe.
  "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",             // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

lazy val sharedPublishSettings = Seq(
  // releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  credentials := Seq(Credentials(Path.userHome / ".sbt" / ".credentials")),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
)

lazy val credentialSettings = Seq(
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val prompt = shellPrompt in ThisBuild := { state =>
  scala.Console.YELLOW + "[" + scala.Console.CYAN + Project.extract(state).currentProject.id + scala.Console.YELLOW + "]" + scala.Console.RED + " $ " + scala.Console.RESET
}

lazy val commonResolvers = Seq(
  Resolver.sonatypeRepo("releases")
  , Resolver.typesafeRepo("releases")
  , Resolver.sonatypeRepo("snapshots")
)

lazy val deps = Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.typelevel" %% "mouse" % "0.16",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "com.typesafe" % "config" % "1.3.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "io.verizon.quiver" %% "core" % "7.0.18",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
