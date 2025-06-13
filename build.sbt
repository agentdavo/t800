//------------------------------------------------------------
// build.sbt  —  works with published jars OR git submodule
//------------------------------------------------------------

// --- 0 ▪ Mode & Scala version ---------------------------------------------
val sourceBuild    = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "0") == "1"
val spinalSrcPath  = sys.env.getOrElse("SPINALHDL_PATH", "./ext/SpinalHDL")

ThisBuild / organization := "org.example"
ThisBuild / version      := "1.0.0"
ThisBuild / scalaVersion := (if (sourceBuild) "2.12.18" else "2.13.14")

// --- 1 ▪ Published dependencies -------------------------------------------
val spinalVersion   = if (sourceBuild) "dev" else "1.12.2"
val spinalCoreDep   = "com.github.spinalhdl" %% "spinalhdl-core"  % spinalVersion
val spinalLibDep    = "com.github.spinalhdl" %% "spinalhdl-lib"   % spinalVersion
val spinalPlugDep   = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val spinalDbDep     = "com.github.spinalhdl" %% "spinalhdl-database" % spinalVersion

// Add Sonatype snapshot resolver if needed
resolvers ++= {
  if (!sourceBuild && spinalVersion == "dev")
    Seq("Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/public")
  else Nil
}

// --- 2 ▪ Submodule deps if source-building -------------------------------
val extraProjects =
  if (sourceBuild)
    Seq("core", "lib", "sim", "idslplugin").map(p => ProjectRef(file(spinalSrcPath), p))
  else Nil

// --- 3 ▪ Root project ----------------------------------------------------
lazy val t800 = (project in file("."))
  .settings(
    name := "t800",
    Compile / scalaSource := baseDirectory.value / "src" / "main" / "scala",
    Test    / scalaSource := baseDirectory.value / "src" / "test" / "scala",

    libraryDependencies ++=
      Seq("org.scalatest" %% "scalatest" % "3.2.17" % Test) ++
      (if (sourceBuild) Nil else Seq(spinalCoreDep, spinalLibDep, spinalPlugDep, spinalDbDep)),

    scalacOptions ++= Seq("-language:reflectiveCalls"),

    scalacOptions ++= Def.taskDyn {
      if (sourceBuild) {
        val idsl = ProjectRef(file(spinalSrcPath), "idslplugin")
        Def.task {
          val jar = (idsl / Compile / packageBin).value
          Seq("-Xplugin-require:idsl-plugin", s"-Xplugin:${jar.getAbsolutePath}")
        }
      } else Def.task(Seq("-Xplugin-require:idsl-plugin"))
    }.value,

    fork := true
  )
  .dependsOn(extraProjects.map(_ % "compile->compile"): _*)

// --- 4 ▪ Aliases ---------------------------------------------------------
addCommandAlias("coreVerilog", ";clean; runMain t800.TopVerilog")
