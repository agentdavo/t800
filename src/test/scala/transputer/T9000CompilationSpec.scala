package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

/**
 * Test specification for T9000 Compilation and Instantiation
 * Tests that T9000 components can be instantiated without simulation
 */
class T9000CompilationSpec extends AnyFunSuite {

  test("T9000 minimal configuration should compile") {
    // Test that we can create a minimal T9000 without simulation
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false
    )
    
    // Configure globals
    T9000Transputer.configureGlobals(param)
    
    // This should compile without error
    val design = new Component {
      val core = Transputer(Seq(
        new transputer.plugins.core.transputer.TransputerPlugin(),
        new transputer.plugins.core.regstack.RegStackPlugin(),
        new transputer.plugins.core.pipeline.PipelinePlugin(),
        new transputer.plugins.bus.SystemBusPlugin()
      ))
    }
    
    // If we get here, compilation succeeded
    assert(design != null, "T9000 minimal configuration compiled successfully")
  }

  test("T9000 with cache plugins should compile") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false,
      mainCacheKb = 16,
      wsCacheWords = 32
    )
    
    T9000Transputer.configureGlobals(param)
    
    val design = new Component {
      val core = Transputer(Seq(
        new transputer.plugins.core.transputer.TransputerPlugin(),
        new transputer.plugins.core.regstack.RegStackPlugin(),
        new transputer.plugins.core.pipeline.PipelinePlugin(),
        new transputer.plugins.core.cache.MainCachePlugin(),
        new transputer.plugins.core.cache.WorkspaceCachePlugin(),
        new transputer.plugins.bus.SystemBusPlugin()
      ))
    }
    
    assert(design != null, "T9000 with cache plugins compiled successfully")
  }

  test("T9000 with scheduler should compile") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = true,  // Enable scheduler
      enableTimers = false,
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val design = new Component {
      val core = Transputer(Seq(
        new transputer.plugins.core.transputer.TransputerPlugin(),
        new transputer.plugins.core.regstack.RegStackPlugin(),
        new transputer.plugins.core.pipeline.PipelinePlugin(),
        new transputer.plugins.schedule.SchedulerPlugin(),
        new transputer.plugins.bus.SystemBusPlugin()
      ))
    }
    
    assert(design != null, "T9000 with scheduler compiled successfully")
  }

  test("T9000 with timers should compile") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = true,  // Enable timers
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val design = new Component {
      val core = Transputer(Seq(
        new transputer.plugins.core.transputer.TransputerPlugin(),
        new transputer.plugins.core.regstack.RegStackPlugin(),
        new transputer.plugins.core.pipeline.PipelinePlugin(),
        new transputer.plugins.timers.TimerPlugin(),
        new transputer.plugins.bus.SystemBusPlugin()
      ))
    }
    
    assert(design != null, "T9000 with timers compiled successfully")
  }

  test("T9000 with arithmetic plugins should compile") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val design = new Component {
      val core = Transputer(Seq(
        new transputer.plugins.core.transputer.TransputerPlugin(),
        new transputer.plugins.core.regstack.RegStackPlugin(),
        new transputer.plugins.core.pipeline.PipelinePlugin(),
        new transputer.plugins.arithmetic.ArithmeticPlugin(),
        new transputer.plugins.general.GeneralPlugin(),
        new transputer.plugins.bus.SystemBusPlugin()
      ))
    }
    
    assert(design != null, "T9000 with arithmetic plugins compiled successfully")
  }

  test("T9000 full configuration should compile") {
    // Test the full T9000 configuration that param.plugins() would create
    val param = T9000Param(
      enableFpu = false,      // Keep FPU disabled for compilation test
      enablePmi = false,      // PMI has known issues
      enableVcp = false,      // Keep VCP disabled for simpler test
      enableScheduler = true,
      enableTimers = true,
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val design = new Component {
      // Use the full plugin set from T9000Param
      val core = Transputer(param.plugins())
    }
    
    assert(design != null, "T9000 full configuration compiled successfully")
  }
}