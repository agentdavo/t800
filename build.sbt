//------------------------------------------------------------
// build.sbt  —  lean & dual-mode
//------------------------------------------------------------
ThisBuild / organization := "org.example"
ThisBuild / version      := "1.0.0"
ThisBuild / scalaVersion := "2.13.14"

//------------------------------------------------------------
// 1 ▪ SpinalHDL version & binary deps
//------------------------------------------------------------
val spinalVersion = "1.12.2"         // change to "dev" only when you build from source

lazy val spinalCoreDep  = "com.github.spinalhdl" %% "spinalhdl-core"  % spinalVersion
lazy val spinalLibDep   = "com.github.spinalhdl" %% "spinalhdl-lib"   % spinalVersion
lazy val spinalPlugDep  = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

//------------------------------------------------------------
// 2 ▪ Source-build switch
//------------------------------------------------------------
val buildFromSrc  = sys.env.getOrElse("SPINALHDL_FROM_SOURCE", "0") == "1"
val spinalSrcPath = sys.env.getOrElse("SPINALHDL_PATH", "./ext/SpinalHDL")

//------------------------------------------------------------
// 3 ▪ Root project
//------------------------------------------------------------
lazy val t800 = (project in file("."))
  .settings(
    name := "t800",

    Compile / scalaSource := baseDirectory.value / "src" / "main" / "scala",
    Test    / scalaSource := baseDirectory.value / "src" / "test" / "scala",

    //--------------------------------------------------------
    // libraries
    //--------------------------------------------------------
    libraryDependencies ++=
      Seq("org.scalatest" %% "scalatest" % "3.2.17" % Test) ++
      (if (buildFromSrc) Nil else Seq(spinalCoreDep, spinalLibDep, spinalPlugDep)),

    //--------------------------------------------------------
    // scalac flags
    //--------------------------------------------------------
    scalacOptions ++= {
      val base = Seq("-language:reflectiveCalls")
      if (buildFromSrc) {
        // locate freshly-built idsl-plugin jar on the class-path
        val jar = (Compile / fullClasspath).value
          .files
          .find(_.getName.startsWith("spinalhdl-idsl-plugin"))
          .getOrElse(sys.error("idsl-plugin jar not found (did sub-module build?)"))
        base ++ Seq(s"-Xplugin:${jar.getAbsolutePath}", "-Xplugin-require:idsl-plugin")
      } else {
        base :+ "-Xplugin-require:idsl-plugin"
      }
    },

    fork := true                               // own JVM for runMain / tests
  )
  //----------------------------------------------------------
  // 4 ▪ Attach local SpinalHDL sub-projects when requested
  //----------------------------------------------------------
  .dependsOn(
    (if (buildFromSrc)
       Seq(
         ProjectRef(file(spinalSrcPath), "core")       % "compile->compile",
         ProjectRef(file(spinalSrcPath), "lib")        % "compile->compile",
         ProjectRef(file(spinalSrcPath), "sim")        % "compile->compile",
         ProjectRef(file(spinalSrcPath), "idslplugin") % "compile->compile"
       )
     else Seq.empty): _*
  )

//------------------------------------------------------------
// 5 ▪ One-liner to emit Verilog
//------------------------------------------------------------
addCommandAlias("coreVerilog", ";clean; runMain t800.TopVerilog")
