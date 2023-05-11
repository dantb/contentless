// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.1" // your current series x.y

ThisBuild / organization := "io.dantb"
ThisBuild / organizationName := "Daniel Tattan-Birch"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("dantb", "Daniel Tattan-Birch")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

val Scala213 = "2.13.10"
val Scala3 = "3.2.2"
ThisBuild / crossScalaVersions := Seq(Scala213, Scala3)
ThisBuild / scalaVersion := Scala3 // the default Scala

val Cats = "2.9.0"
val CatsEffect = "3.4.10"
val Circe = "0.14.1"
val Grackle = "0.11.0"
val Http4s = "0.23.13"
val Literally = "1.1.0"
val MUnit = "0.7.29"
val MUnitCE = "1.0.7"

lazy val root = tlCrossRootProject.aggregate(core, graphql)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "contentless",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"  % Cats,
      "io.circe"      %% "circe-core" % Circe,
      "org.scalameta" %% "munit"      % MUnit % Test,
    )
  )

lazy val graphql = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("graphql"))
  .settings(
    name := "contentless",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"      % CatsEffect,
      "edu.gemini"    %% "gsp-graphql-core" % Grackle,
      "org.typelevel" %% "literally"        % Literally,
      "org.http4s"    %% "http4s-client"    % Http4s,
      "org.http4s"    %% "http4s-circe"     % Http4s,
      "org.typelevel" %% "munit-cats-effect-3" % MUnitCE % Test
    )
  ).dependsOn(core)

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
