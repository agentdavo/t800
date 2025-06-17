[info] welcome to sbt 1.10.0 (Ubuntu Java 21.0.7)
[info] loading settings for project t800-build from plugins.sbt ...
[info] loading project definition from /workspace/t800/project
[info] loading settings for project t800 from build.sbt ...
[info] set current project to t800 (in build file:/workspace/t800/)
[info] scalafmt: Formatting 1 Scala sources (/workspace/t800)...
[success] Total time: 4 s, completed Jun 17, 2025, 1:11:12 AM
[info] welcome to sbt 1.10.0 (Ubuntu Java 21.0.7)
[info] loading settings for project t800-build from plugins.sbt ...
[info] loading project definition from /workspace/t800/project
[info] loading settings for project t800 from build.sbt ...
[info] set current project to t800 (in build file:/workspace/t800/)
[info] compiling 1 Scala source to /workspace/t800/target/scala-2.13/classes ...
[info] done compiling
[info] FpuAdderSpec:
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:34
[Progress] at 0.000 : Elaborate components
[Progress] at 0.361 : Checks and transforms
[Progress] at 0.685 : Generate Verilog to ./simWorkspace/tmp/job_1
[Done] at 0.817
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 653.189 ms
[Progress] Start FpuAdderDut test simulation with seed 428104347
[Done] Simulation done in 22.165 ms
[info] - positive operands
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:36
[Progress] at 1.572 : Elaborate components
[Progress] at 1.601 : Checks and transforms
[Progress] at 1.663 : Generate Verilog to ./simWorkspace/tmp/job_2
[Done] at 1.703
[Info] Workspace 'FpuAdderDut' was reallocated as 'FpuAdderDut_1' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut_1
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 229.150 ms
[Progress] Start FpuAdderDut test simulation with seed 1337401411
[Done] Simulation done in 6.680 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:36
[Progress] at 1.949 : Elaborate components
[info] - negative operand
[Progress] at 1.973 : Checks and transforms
[Progress] at 2.016 : Generate Verilog to ./simWorkspace/tmp/job_3
[Done] at 2.050
[Info] Workspace 'FpuAdderDut' was reallocated as 'FpuAdderDut_2' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut_2
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 229.702 ms
[Progress] Start FpuAdderDut test simulation with seed 624141655
[Done] Simulation done in 6.440 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:36
[Progress] at 2.296 : Elaborate components
[info] - large exponent gap
[Progress] at 2.327 : Checks and transforms
[Progress] at 2.368 : Generate Verilog to ./simWorkspace/tmp/job_4
[Done] at 2.391
[Info] Workspace 'FpuAdderDut' was reallocated as 'FpuAdderDut_3' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut_3
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 225.980 ms
[Progress] Start FpuAdderDut test simulation with seed 513640363
[Done] Simulation done in 5.177 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[info] - denormals *** FAILED ***
[Progress] at 2.664 : Elaborate components
[info]   3.193344495255552E293 did not equal 9.9E-324 (FpuAdderSpec.scala:60)
[Progress] at 2.690 : Checks and transforms
[Progress] at 2.735 : Generate Verilog to ./simWorkspace/tmp/job_5
[Done] at 2.769
[Info] Workspace 'FpuAdderDut' was reallocated as 'FpuAdderDut_4' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut_4
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 196.841 ms
[Progress] Start FpuAdderDut test simulation with seed 1992663423
[Done] Simulation done in 12.560 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[info] - opposite signs
[Progress] at 2.990 : Elaborate components
[Progress] at 3.011 : Checks and transforms
[Progress] at 3.047 : Generate Verilog to ./simWorkspace/tmp/job_6
[Done] at 3.078
[Info] Workspace 'FpuAdderDut' was reallocated as 'FpuAdderDut_5' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/FpuAdderDut_5
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 153.121 ms
[Progress] Start FpuAdderDut test simulation with seed 394578279
[Done] Simulation done in 3.360 ms
[info] - rounding to minus
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[Progress] at 3.265 : Elaborate components
[info] FpuPluginSpec:
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.330 : Elaborate components
[info] - FPADD *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[Progress] at 3.355 : Elaborate components
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[info]   at t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[info]   ...
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.372 : Elaborate components
[info] - FPSUB *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[Progress] at 3.385 : Elaborate components
[info]   at t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[info]   ...
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.397 : Elaborate components
[info] - FPMUL *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[info]   at t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[Runtime] Current date : 2025.06.17 01:11:37
[info]   ...
[Progress] at 3.412 : Elaborate components
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.421 : Elaborate components
[info] - FPDIV *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[info]   at t800.FpuPluginSpec.$anonfun$run$1(FpuPluginSpec.scala:42)
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[info]   ...
[Progress] at 3.431 : Elaborate components
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$new$9(FpuPluginSpec.scala:61)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.441 : Elaborate components
[info] - rounding mode register *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[info]   at t800.FpuPluginSpec.$anonfun$new$9(FpuPluginSpec.scala:61)
[Progress] at 3.453 : Elaborate components
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[info]   ...
spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
t800.FpuDut.<init>(FpuPluginSpec.scala:27)
t800.FpuPluginSpec.$anonfun$new$12(FpuPluginSpec.scala:75)
spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
spinal.sim.JvmThread.run(SimManager.scala:51)

**********************************************************************************************
[Warning] Elaboration failed (0 error).
          Spinal will restart with scala trace to help you to find the problem.
**********************************************************************************************

[Progress] at 3.462 : Elaborate components
[info] - error flag handling *** FAILED ***
[info]   java.lang.Exception: Can't find the service t800.plugins.fpu.FpuSrv
[info]   at spinal.lib.misc.plugin.PluginHost.apply(Host.scala:54)
[info]   at t800.FpuDut.<init>(FpuPluginSpec.scala:27)
[info]   at t800.FpuPluginSpec.$anonfun$new$12(FpuPluginSpec.scala:75)
[info]   at spinal.core.internals.PhaseCreateComponent.$anonfun$impl$319(Phase.scala:2946)
[info]   at spinal.core.fiber.Engine$.$anonfun$create$1(AsyncCtrl.scala:173)
[info]   at spinal.core.fiber.AsyncThread.$anonfun$jvmThread$1(AsyncThread.scala:60)
[info]   at spinal.core.fiber.EngineContext.$anonfun$newJvmThread$1(AsyncCtrl.scala:39)
[info]   at spinal.sim.JvmThread.run(SimManager.scala:51)
[info]   ...
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:37
[Progress] at 3.486 : Elaborate components
[info] InitTransputerSpec:
[???] setup start
[???] setup end
[???] setup start
[???] setup end
[???] build start
[???] build end
[???] build start
[???] build end
[Progress] at 3.512 : Checks and transforms
[Progress] at 3.520 : Generate Verilog to .
[Warning] 17 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 3.532
[info] - TransputerPlugin sets default configuration
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[info] FpuBusySpec:
[Runtime] Current date : 2025.06.17 01:11:37
[Progress] at 3.543 : Elaborate components
[Progress] at 3.562 : Checks and transforms
[Progress] at 3.565 : Generate Verilog to ./simWorkspace/tmp/job_13
[Done] at 3.574
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/BusyDut
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 178.122 ms
[Progress] Start BusyDut test simulation with seed 1632897876
[Error] Simulation failed at time=170
[info] - busy clears after operations *** FAILED ***
[info]   spinal.core.sim.`package`.SimBoolPimper(dut.io.busy).toBoolean was false (FpuBusySpec.scala:37)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:38
[Progress] at 3.777 : Elaborate components
[info] FpuVCUSpec:
[Progress] at 3.795 : Checks and transforms
[Progress] at 3.829 : Generate Verilog to ./simWorkspace/tmp/job_14
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 3.846
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 147.466 ms
[Progress] Start VcuDut test simulation with seed 1558634054
[Done] Simulation done in 3.500 ms
[info] - detect NaN and canonical result
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:38
[Progress] at 4.004 : Elaborate components
[Progress] at 4.016 : Checks and transforms
[Progress] at 4.042 : Generate Verilog to ./simWorkspace/tmp/job_15
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 4.056
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_1' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_1
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 154.602 ms
[Progress] Start VcuDut test simulation with seed 103570629
[Done] Simulation done in 19.874 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:38
[Progress] at 4.267 : Elaborate components
[info] - detect NaN in operand B
[Progress] at 4.278 : Checks and transforms
[Progress] at 4.307 : Generate Verilog to ./simWorkspace/tmp/job_16
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 4.348
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_2' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_2
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 197.702 ms
[Progress] Start VcuDut test simulation with seed 1622488256
[Done] Simulation done in 8.835 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:39
[Progress] at 4.566 : Elaborate components
[Progress] at 4.571 : Checks and transforms
[Progress] at 4.597 : Generate Verilog to ./simWorkspace/tmp/job_17
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 4.611
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_3' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_3
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 186.576 ms
[Progress] Start VcuDut test simulation with seed 1196974012
[Done] Simulation done in 4.023 ms
[info] - detect infinities and sign propagate
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:39
[Progress] at 4.827 : Elaborate components
[Progress] at 4.842 : Checks and transforms
[Progress] at 4.918 : Generate Verilog to ./simWorkspace/tmp/job_18
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 4.983
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_4' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_4
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 165.016 ms
[Progress] Start VcuDut test simulation with seed 1935467785
[Done] Simulation done in 6.795 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:39
[Progress] at 5.202 : Elaborate components
[Progress] at 5.218 : Checks and transforms
[Progress] at 5.257 : Generate Verilog to ./simWorkspace/tmp/job_19
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 5.274
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_5' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_5
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 199.367 ms
[Progress] Start VcuDut test simulation with seed 1840731189
[Done] Simulation done in 7.219 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:39
[Progress] at 5.489 : Elaborate components
[info] - normalize denormals
[Progress] at 5.495 : Checks and transforms
[Progress] at 5.534 : Generate Verilog to ./simWorkspace/tmp/job_20
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 5.552
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_6' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_6
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 140.602 ms
[Progress] Start VcuDut test simulation with seed 1351532826
[Done] Simulation done in 6.895 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:40
[Progress] at 5.719 : Elaborate components
[Progress] at 5.732 : Checks and transforms
[Progress] at 5.755 : Generate Verilog to ./simWorkspace/tmp/job_21
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 5.771
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_7' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_7
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 162.563 ms
[Progress] Start VcuDut test simulation with seed 1566127420
[Done] Simulation done in 5.574 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:40
[Progress] at 5.947 : Elaborate components
[Progress] at 5.953 : Checks and transforms
[Progress] at 5.980 : Generate Verilog to ./simWorkspace/tmp/job_22
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 5.996
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_8' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_8
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 144.186 ms
[Progress] Start VcuDut test simulation with seed 912539093
[Done] Simulation done in 12.562 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:40
[Progress] at 6.159 : Elaborate components
[info] - propagate zero sign
[Progress] at 6.166 : Checks and transforms
[Progress] at 6.244 : Generate Verilog to ./simWorkspace/tmp/job_23
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 6.265
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_9' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_9
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 153.644 ms
[Progress] Start VcuDut test simulation with seed 1037974850
[Done] Simulation done in 7.317 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:40
[Progress] at 6.433 : Elaborate components
[Progress] at 6.438 : Checks and transforms
[Progress] at 6.464 : Generate Verilog to ./simWorkspace/tmp/job_24
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 6.480
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_10' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_10
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 118.162 ms
[Progress] Start VcuDut test simulation with seed 1761898757
[Done] Simulation done in 2.737 ms
[info] - fpmul zero bypass
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 6.606 : Elaborate components
[Progress] at 6.611 : Checks and transforms
[Progress] at 6.647 : Generate Verilog to ./simWorkspace/tmp/job_25
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 6.662
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_11' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_11
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 139.755 ms
[Progress] Start VcuDut test simulation with seed 406664224
[Done] Simulation done in 19.520 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 6.828 : Elaborate components
[Progress] at 6.844 : Checks and transforms
[Progress] at 6.865 : Generate Verilog to ./simWorkspace/tmp/job_26
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 6.877
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_12' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_12
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 118.268 ms
[Progress] Start VcuDut test simulation with seed 2098352034
[Done] Simulation done in 2.728 ms
[info] - fpmul infinity interactions
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 7.004 : Elaborate components
[Progress] at 7.013 : Checks and transforms
[Progress] at 7.035 : Generate Verilog to ./simWorkspace/tmp/job_27
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.044
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_13' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_13
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 111.564 ms
[Progress] Start VcuDut test simulation with seed 1346623610
[Done] Simulation done in 10.541 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 7.170 : Elaborate components
[Progress] at 7.207 : Checks and transforms
[Progress] at 7.228 : Generate Verilog to ./simWorkspace/tmp/job_28
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.241
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_14' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_14
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 117.735 ms
[Progress] Start VcuDut test simulation with seed 1645797468
[Done] Simulation done in 8.870 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 7.373 : Elaborate components
[Progress] at 7.378 : Checks and transforms
[Progress] at 7.399 : Generate Verilog to ./simWorkspace/tmp/job_29
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.409
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_15' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_15
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 112.858 ms
[Progress] Start VcuDut test simulation with seed 1521328661
[Done] Simulation done in 14.797 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:41
[Progress] at 7.541 : Elaborate components
[Progress] at 7.550 : Checks and transforms
[Progress] at 7.572 : Generate Verilog to ./simWorkspace/tmp/job_30
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.581
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_16' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_16
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 126.233 ms
[Progress] Start VcuDut test simulation with seed 1142644754
[Done] Simulation done in 2.682 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:42
[Progress] at 7.715 : Elaborate components
[Progress] at 7.727 : Checks and transforms
[Progress] at 7.749 : Generate Verilog to ./simWorkspace/tmp/job_31
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.777
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_17' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_17
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 116.934 ms
[Progress] Start VcuDut test simulation with seed 940520472
[Done] Simulation done in 2.466 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:42
[Progress] at 7.901 : Elaborate components
[Progress] at 7.911 : Checks and transforms
[Progress] at 7.932 : Generate Verilog to ./simWorkspace/tmp/job_32
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 7.941
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_18' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_18
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 118.517 ms
[Progress] Start VcuDut test simulation with seed 742836339
[Done] Simulation done in 2.985 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:42
[Progress] at 8.067 : Elaborate components
[Progress] at 8.091 : Checks and transforms
[Progress] at 8.113 : Generate Verilog to ./simWorkspace/tmp/job_33
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 8.127
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_19' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_19
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 118.889 ms
[Progress] Start VcuDut test simulation with seed 621569033
[Done] Simulation done in 2.526 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:42
[Progress] at 8.253 : Elaborate components
[Progress] at 8.261 : Checks and transforms
[Progress] at 8.282 : Generate Verilog to ./simWorkspace/tmp/job_34
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 8.291
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_20' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_20
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 134.369 ms
[Progress] Start VcuDut test simulation with seed 1469738163
[Done] Simulation done in 11.125 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:42
[Progress] at 8.441 : Elaborate components
[Progress] at 8.454 : Checks and transforms
[Progress] at 8.524 : Generate Verilog to ./simWorkspace/tmp/job_35
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 8.542
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_21' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_21
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 120.039 ms
[Progress] Start VcuDut test simulation with seed 648365495
[Done] Simulation done in 2.321 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:43
[Progress] at 8.671 : Elaborate components
[Progress] at 8.683 : Checks and transforms
[Progress] at 8.704 : Generate Verilog to ./simWorkspace/tmp/job_36
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 8.712
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_22' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_22
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 138.569 ms
[Progress] Start VcuDut test simulation with seed 531979484
[Done] Simulation done in 3.948 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:43
[Progress] at 8.862 : Elaborate components
[Progress] at 8.883 : Checks and transforms
[Progress] at 8.914 : Generate Verilog to ./simWorkspace/tmp/job_37
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 8.926
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_23' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_23
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 126.028 ms
[Progress] Start VcuDut test simulation with seed 1884617775
[Done] Simulation done in 10.418 ms
[info] - comparison opcodes
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:43
[Progress] at 9.069 : Elaborate components
[Progress] at 9.082 : Checks and transforms
[Progress] at 9.119 : Generate Verilog to ./simWorkspace/tmp/job_38
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 9.137
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_24' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_24
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 124.567 ms
[Progress] Start VcuDut test simulation with seed 250289618
[Done] Simulation done in 2.583 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:43
[Progress] at 9.284 : Elaborate components
[info] - FPEQ returns false on NaN
[Progress] at 9.288 : Checks and transforms
[Progress] at 9.309 : Generate Verilog to ./simWorkspace/tmp/job_39
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 9.318
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_25' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_25
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 121.843 ms
[Progress] Start VcuDut test simulation with seed 614280481
[Done] Simulation done in 2.260 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:43
[Progress] at 9.446 : Elaborate components
[Progress] at 9.451 : Checks and transforms
[Progress] at 9.473 : Generate Verilog to ./simWorkspace/tmp/job_40
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 9.483
[Info] Workspace 'VcuDut' was reallocated as 'VcuDut_26' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/VcuDut_26
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 120.647 ms
[Progress] Start VcuDut test simulation with seed 1708747478
[Done] Simulation done in 2.391 ms
[info] - trapEnable on fpchkerr
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 9.614 : Elaborate components
[info] RangeReducerSpec:
[Progress] at 9.627 : Checks and transforms
[Progress] at 9.629 : Generate Verilog to ./simWorkspace/tmp/job_41
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 9.634
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/RangeReducerDut
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 163.165 ms
[Progress] Start RangeReducerDut test simulation with seed 1269311006
[Done] Simulation done in 3.316 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 9.807 : Elaborate components
[Progress] at 9.810 : Checks and transforms
[info] - pi reduction
[Progress] at 9.811 : Generate Verilog to ./simWorkspace/tmp/job_42
[Warning] 1 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 9.818
[Info] Workspace 'RangeReducerDut' was reallocated as 'RangeReducerDut_1' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/RangeReducerDut_1
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 138.569 ms
[Progress] Start RangeReducerDut test simulation with seed 985255158
[Done] Simulation done in 3.914 ms
[info] - 3pi/2 reduction
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 9.972 : Elaborate components
[info] DivRootSpec:
[Progress] at 9.996 : Checks and transforms
[Progress] at 10.001 : Generate Verilog to ./simWorkspace/tmp/job_43
[Warning] 6 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 10.018
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/DivRootDut
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 110.761 ms
[Progress] Start DivRootDut test simulation with seed 417108550
[info] - simple divide
[Done] Simulation done in 3.619 ms
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 10.137 : Elaborate components
[Progress] at 10.141 : Checks and transforms
[Progress] at 10.145 : Generate Verilog to ./simWorkspace/tmp/job_44
[Warning] 6 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 10.151
[Info] Workspace 'DivRootDut' was reallocated as 'DivRootDut_1' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/DivRootDut_1
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 111.077 ms
[Progress] Start DivRootDut test simulation with seed 1946731859
[Error] Simulation failed at time=320
[info] - simple sqrt *** FAILED ***
[info]   4611686018829942783 did not equal 4613937818241073152 (DivRootSpec.scala:65)
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 10.271 : Elaborate components
[Progress] at 10.281 : Checks and transforms
[Progress] at 10.284 : Generate Verilog to ./simWorkspace/tmp/job_45
[Warning] 6 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 10.290
[Info] Workspace 'DivRootDut' was reallocated as 'DivRootDut_2' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/DivRootDut_2
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 142.296 ms
[Progress] Start DivRootDut test simulation with seed 1163514649
[Done] Simulation done in 3.950 ms
[info] - simple remainder
[Runtime] SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
[Runtime] JVM max memory : 2544.0MiB
[Runtime] Current date : 2025.06.17 01:11:44
[Progress] at 10.451 : Elaborate components
[Progress] at 10.455 : Checks and transforms
[Progress] at 10.459 : Generate Verilog to ./simWorkspace/tmp/job_46
[Warning] 6 signals were pruned. You can call printPruned on the backend report to get more informations.
[Done] at 10.466
[Info] Workspace 'DivRootDut' was reallocated as 'DivRootDut_3' to avoid collision
[Progress] Simulation workspace in /workspace/t800/./simWorkspace/DivRootDut_3
[Progress] Verilator compilation started
[info] Found cached verilator binaries
[Progress] Verilator compilation done in 179.152 ms
[Progress] Start DivRootDut test simulation with seed 963852544
[Done] Simulation done in 90.598 ms
[info] - divide negative sign
[info] Real32ToReal64Spec:
[info] - convert several floats
[info] Run completed in 12 seconds, 47 milliseconds.
[info] Total number of tests run: 31
[info] Suites: completed 8, aborted 0
[info] Tests: succeeded 22, failed 9, canceled 0, ignored 0, pending 0
[info] *** 9 TESTS FAILED ***
[error] Failed tests:
[error] 	t800.FpuAdderSpec
[error] 	t800.FpuPluginSpec
[error] 	t800.FpuBusySpec
[error] 	t800.DivRootSpec
[error] (Test / test) sbt.TestsFailedException: Tests unsuccessful
[error] Total time: 21 s, completed Jun 17, 2025, 1:11:45 AM
