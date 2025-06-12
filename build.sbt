//---------------------------------------------
// build.sbt  —  Transputer-T800 core project
//---------------------------------------------

// ------------------------------------------------------------------
// 1.  Global versions
// ------------------------------------------------------------------
val spinalVersion   = "1.9.4"                 // or a pinned tag (e.g. "1.9.4")
val scalaVer        = "2.13.14"             // first compiler in Spinal matrix
val junitVersion    = "3.2.17"              // ScalaTest

// ------------------------------------------------------------------
// 2.  Optional: build SpinalHDL from source
// ------------------------------------------------------------------
// Use pre-built SpinalHDL by default. Set SPINALHDL_FROM_SOURCE=1 to override.
val buildSpinalFromSource = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "0") == "1"
val spinalSrcPathEnabled  = sys.env.contains("SPINALHDL_PATH")
val spinalSrcPath         = sys.env.getOrElse("SPINALHDL_PATH", "./ext/SpinalHDL")

// ------------------------------------------------------------------
// 3.  Root project definition
// ------------------------------------------------------------------
lazy val root = (project in file("."))
  .settings(
    name                := "transputer-t800",
    organization        := "org.transputer",
    version             := "0.1.0-SNAPSHOT",
    scalaVersion        := scalaVer,
    crossScalaVersions  := Seq(scalaVer),
    fork                := true,              // runMain needs its own JVM
    // ----------------------- compiler plugin -----------------------
    scalacOptions ++=
      Seq("-language:reflectiveCalls") ++
      (if (buildSpinalFromSource)
         Seq(s"-Xplugin:${pluginPath.value}", "-Xplugin-require:idsl-plugin")
       else Seq.empty),
    // ----------------------- dependencies --------------------------
    libraryDependencies ++=
      (if (buildSpinalFromSource) Nil
       else Seq(
         "com.github.spinalhdl" %% "spinalhdl-core"        % spinalVersion,
         "com.github.spinalhdl" %% "spinalhdl-lib"         % spinalVersion,
         compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
       )) ++ Seq(
        "org.scalatest" %% "scalatest" % junitVersion % Test
       ),
    // ----------------------- test config ---------------------------
    Test / parallelExecution := false
  )
  .dependsOn(
    (if (buildSpinalFromSource) Seq(
      spinalIdslPlugin, spinalCore, spinalLib, spinalSim
    ).map(_ % "compile->compile") else Seq.empty): _*
  )

// ------------------------------------------------------------------
// 4.  Helper: compute plugin jar path when built from source
// ------------------------------------------------------------------
def pluginPath = Def.setting {
  val base      = if (spinalSrcPathEnabled) file(spinalSrcPath) else baseDirectory.value / "ext" / "SpinalHDL"
  val binDir    = base / "idslplugin" / "target" / s"scala-${scalaVersion.value.split('.').init.mkString(".")}"
  val jarName   = s"spinalhdl-idsl-plugin_${scalaVersion.value.split('.').init.mkString(".")}-$spinalVersion.jar"
  (binDir / jarName).getAbsolutePath
}

// ------------------------------------------------------------------
// 5.  Spinal sub-projects (only used when building from source)
// ------------------------------------------------------------------
lazy val spinalIdslPlugin = ProjectRef(file(spinalSrcPath), "idslplugin")
lazy val spinalCore       = ProjectRef(file(spinalSrcPath), "core")
lazy val spinalLib        = ProjectRef(file(spinalSrcPath), "lib")
lazy val spinalSim        = ProjectRef(file(spinalSrcPath), "sim")