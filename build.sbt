//------------------------------------------------------------
// build.sbt  —  works with published jars OR git submodule
//------------------------------------------------------------

// --- 0 ▪ Mode & Scala version ---------------------------------------------
val sourceBuild = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "0") == "1"
val spinalSrcPath = sys.env.getOrElse("SPINALHDL_PATH", "./ext/SpinalHDL")

ThisBuild / organization := "org.example"
ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := (if (sourceBuild) "2.12.18" else "2.13.14")

// --- 1 ▪ Published dependencies -------------------------------------------
val spinalVersion = if (sourceBuild) "dev" else "1.12.2"
val spinalCoreDep = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLibDep = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalPlugDep = compilerPlugin(
  "com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion
)

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
    Test / scalaSource := baseDirectory.value / "src" / "test" / "scala",
    // Limit sources for the minimal unit build
    Compile / unmanagedSources := {
      val keep = Set(
        "Global.scala",
        "Opcode.scala",
        "T800.scala",
        "Param.scala",
        "Generate.scala",
        "SystemBusSrv.scala",
        "plugins/Service.scala",
        "pipeline/Service.scala",
        "plugins/registers/Service.scala",
        "plugins/registers/RegFilePlugin.scala",
        "PipelinePlugin.scala",
        "PipelineBuilderPlugin.scala",
        "transputer/TransputerPlugin.scala",
      )
      val srcDir = (Compile / scalaSource).value
      val selected = (srcDir ** "*.scala").get.filter(f =>
        f.getPath.contains("spinal/lib") || keep.exists(k => f.getPath.endsWith(k))
      )
      selected
    },
    Test / unmanagedSources := {
      val srcDir = (Test / scalaSource).value
      val keep = Seq("InitTransputerSpec.scala", "FpuAdderSpec.scala")
      keep.flatMap(n => (srcDir ** n).get)
    },
    libraryDependencies ++=
      Seq(
        "org.scalatest" %% "scalatest" % "3.2.17" % Test,
        "com.github.scopt" %% "scopt" % "4.1.0"
      ) ++ (if (sourceBuild) Nil else Seq(spinalCoreDep, spinalLibDep, spinalPlugDep)),
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
