// Simplified SpinalHDL build configuration  
ThisBuild / organization := "org.example"
ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := "2.13.14"

// Use published SpinalHDL jars (stable and reliable)
val spinalVersion = "1.12.2"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion),
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)

// Simplified settings
Compile / scalaSource := baseDirectory.value / "src" / "main" / "scala"
Test / scalaSource := baseDirectory.value / "src" / "test" / "scala"

// Exclude disabled files from compilation
Compile / unmanagedSources / excludeFilter := "disabled" || HiddenFileFilter

// Fork JVM to avoid memory issues
Compile / run / fork := true
Test / fork := true