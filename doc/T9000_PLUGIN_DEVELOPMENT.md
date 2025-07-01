# Plugin Development Guide

This guide provides comprehensive instructions for developing plugins in the T9000 Transputer SpinalHDL implementation. It covers the FiberPlugin architecture, service design patterns, testing strategies, and best practices.

## Table of Contents

- [Overview](#overview)
- [FiberPlugin Fundamentals](#fiberplugin-fundamentals)
- [Service Architecture](#service-architecture)
- [Plugin Development Workflow](#plugin-development-workflow)
- [Testing Strategies](#testing-strategies)
- [Common Patterns](#common-patterns)
- [Debugging and Troubleshooting](#debugging-and-troubleshooting)
- [Best Practices](#best-practices)

## Overview

The T9000 Transputer uses SpinalHDL's **FiberPlugin** architecture to create a modular, service-oriented hardware design. Each major subsystem is implemented as an independent plugin that communicates with other plugins through well-defined service interfaces.

### Key Concepts

- **FiberPlugin**: A hardware component that can be hot-swapped and has lifecycle management
- **Service**: An interface contract between plugins for communication
- **Database**: Typed configuration storage shared across all plugins
- **Fiber**: Concurrent elaboration mechanism allowing out-of-order plugin dependencies
- **Pipeline Integration**: Seamless integration with SpinalHDL's Pipeline DSL

### Benefits of Plugin Architecture

- **Modularity**: Each plugin can be developed, tested, and maintained independently
- **Reusability**: Plugins can be reused across different processor configurations
- **Testability**: Individual plugins can be tested in isolation
- **Configurability**: System composition can be changed without modifying existing plugins
- **Maintainability**: Clear separation of concerns and well-defined interfaces

## FiberPlugin Fundamentals

### Plugin Lifecycle

Every FiberPlugin follows a two-phase lifecycle:

#### Phase 1: Setup
- Service registration and discovery
- Dependency declaration
- Configuration parameter validation
- Resource allocation

#### Phase 2: Build  
- Hardware generation
- Signal interconnection
- Pipeline stage creation
- Service implementation

### Basic Plugin Structure

```scala
package transputer.plugins.myfeature

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.plugins.registers.RegfileService

class MyFeaturePlugin extends FiberPlugin {
  val version = "MyFeaturePlugin v1.0"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    
    // Validate configuration
    require(param.isValid, "Invalid configuration parameters")
    
    // Register services this plugin provides
    addService(new MyFeatureService {
      override def operation(input: UInt): UInt = operationImpl(input)
      override def statusSignal: Bool = statusImpl
    })
    
    // Declare dependencies (don't access yet!)
    val regfile = Plugin[RegfileService] // Declaration only
    
    // Hold setup phase until ready
    retain()
    
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    
    // Wait for all setup phases to complete
    retain.await()
    
    // Now safe to access other services
    implicit val host = this.host
    val regfile = Plugin[RegfileService]
    val systemBus = Plugin[SystemBusService]
    
    // Generate hardware logic
    val myLogic = new MyFeatureLogic(regfile, systemBus)
    
    // Implementation of service methods
    def operationImpl(input: UInt): UInt = {
      myLogic.processInput(input)
    }
    
    def statusImpl: Bool = myLogic.status
    
    println(s"[${this.getDisplayName()}] build complete")
  }
}
```

### Service Definition

Services define the contract between plugins:

```scala
package transputer.plugins.myfeature

import spinal.core._

trait MyFeatureService {
  // Method interfaces for complex operations
  def operation(input: UInt): UInt
  def configure(config: MyFeatureConfig): Unit
  
  // Direct signal access for performance-critical paths
  def statusSignal: Bool
  def dataOutput: UInt
  def interruptSignal: Bool
}

// Configuration bundle for the service
case class MyFeatureConfig(
  enableFeature: Boolean,
  operationMode: Int,
  bufferSize: Int
) {
  def validate(): Unit = {
    require(operationMode >= 0 && operationMode <= 3, "Invalid operation mode")
    require(bufferSize > 0 && bufferSize <= 1024, "Invalid buffer size")
  }
}
```

## Service Architecture

### Service Design Principles

#### 1. Interface Segregation
Keep service interfaces focused and minimal:

```scala
// Good: Focused interface
trait StackService {
  def push(value: UInt): Unit
  def pop(): UInt
  def A: UInt  // Direct register access
  def B: UInt
  def C: UInt
}

// Bad: Overly broad interface  
trait ProcessorService {
  def executeInstruction(instr: Bits): Unit
  def manageMemory(addr: UInt): UInt
  def handleInterrupts(): Unit
  def scheduleProcesses(): Unit
}
```

#### 2. Performance Considerations
Use direct signal access for performance-critical operations:

```scala
trait StackService {
  // Direct access for performance (single cycle)
  def A: UInt
  def B: UInt  
  def C: UInt
  
  // Method interface for complex operations (multi-cycle)
  def push(value: UInt): Unit
  def pop(): UInt
}
```

#### 3. Signal Ownership
Services must clearly own their signals to avoid hierarchy violations:

```scala
class StackPlugin extends FiberPlugin {
  during build new Area {
    // Plugin owns these signals
    val regA = Reg(UInt(32 bits))
    val regB = Reg(UInt(32 bits))
    val regC = Reg(UInt(32 bits))
    
    addService(new StackService {
      // Service provides controlled access
      override def A: UInt = regA
      override def B: UInt = regB
      override def C: UInt = regC
      
      override def push(value: UInt): Unit = {
        regC := regB
        regB := regA
        regA := value
      }
    })
  }
}
```

### Database Integration

Use the database system for configuration sharing:

```scala
object MyFeatureGlobal {
  val ENABLE_FEATURE = Database.blocking[Boolean]
  val BUFFER_SIZE = Database.blocking[Int]
  val OPERATION_MODE = Database.blocking[Int]
}

class MyFeaturePlugin extends FiberPlugin {
  during setup new Area {
    // Read configuration from database
    val enabled = MyFeatureGlobal.ENABLE_FEATURE.get
    val bufferSize = MyFeatureGlobal.BUFFER_SIZE.get
    
    // Only register service if enabled
    if (enabled) {
      addService(new MyFeatureService { ... })
    }
  }
}
```

## Plugin Development Workflow

### Step 1: Define Requirements

Before starting implementation, clearly define:

1. **Functionality**: What does the plugin do?
2. **Interface**: How do other plugins interact with it?
3. **Dependencies**: What services does it require?
4. **Configuration**: What parameters are configurable?
5. **Performance**: What are the timing requirements?

### Step 2: Design Service Interface

```scala
// Start with the service interface
trait MyFeatureService {
  // Core operations
  def enable(): Unit
  def disable(): Unit
  def process(data: UInt): UInt
  
  // Status and control
  def isEnabled: Bool
  def isBusy: Bool
  def errorFlag: Bool
  
  // Configuration
  def configure(params: MyFeatureParams): Unit
}

case class MyFeatureParams(
  mode: MyFeatureMode.Type,
  bufferSize: Int,
  enableInterrupts: Boolean
)

object MyFeatureMode extends SpinalEnum {
  val BASIC, ADVANCED, CUSTOM = newElement()
}
```

### Step 3: Implement Plugin Structure

```scala
class MyFeaturePlugin(param: MyFeatureParam = MyFeatureParam()) extends FiberPlugin {
  val version = "MyFeaturePlugin v1.0"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    
    // Validate parameters
    param.validate()
    
    // Register service
    addService(new MyFeatureService {
      // Implementation will be filled in build phase
      override def enable(): Unit = enableImpl()
      override def process(data: UInt): UInt = processImpl(data)
      override def isEnabled: Bool = enabledFlag
    })
    
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    
    // Access dependencies
    val regfile = Plugin[RegfileService]
    val systemBus = Plugin[SystemBusService]
    
    // Create hardware logic
    val logic = new MyFeatureLogic(param, regfile, systemBus)
    
    // Implement service methods
    def enableImpl(): Unit = logic.enable()
    def processImpl(data: UInt): UInt = logic.process(data)
    val enabledFlag: Bool = logic.enabled
    
    println(s"[${this.getDisplayName()}] build complete")
  }
}
```

### Step 4: Implement Hardware Logic

```scala
class MyFeatureLogic(
  param: MyFeatureParam,
  regfile: RegfileService,
  systemBus: Bmb
) extends Area {
  
  // Control registers
  val enabled = Reg(Bool()) init False
  val busy = Reg(Bool()) init False
  val mode = Reg(MyFeatureMode()) init MyFeatureMode.BASIC
  
  // Data processing logic
  val inputBuffer = Mem(UInt(32 bits), param.bufferSize)
  val outputBuffer = Mem(UInt(32 bits), param.bufferSize)
  
  // State machine
  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val PROCESSING = new State
    val DONE = new State
    
    IDLE.whenIsActive {
      when(enabled && inputReady) {
        goto(PROCESSING)
      }
    }
    
    PROCESSING.whenIsActive {
      busy := True
      // Processing logic here
      when(processingComplete) {
        goto(DONE)
      }
    }
    
    DONE.whenIsActive {
      busy := False
      goto(IDLE)
    }
  }
  
  // Service implementation
  def enable(): Unit = enabled := True
  def disable(): Unit = enabled := False
  def process(data: UInt): UInt = {
    // Process input data
    val result = processData(data)
    result
  }
}
```

### Step 5: Integration with T9000Param

Add your plugin to the T9000 configuration:

```scala
// In T9000Param.scala
case class T9000Param(
  // ... existing parameters
  enableMyFeature: Boolean = false,
  myFeatureBufferSize: Int = 64,
  myFeatureMode: MyFeatureMode.Type = MyFeatureMode.BASIC
) {
  
  def pluginsArea(hartId: Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()
    
    // ... existing plugins
    
    // Add your plugin
    if (enableMyFeature) {
      plugins += new transputer.plugins.myfeature.MyFeaturePlugin(
        MyFeatureParam(
          bufferSize = myFeatureBufferSize,
          mode = myFeatureMode
        )
      )
    }
  }
}
```

## Testing Strategies

### Unit Testing

Create focused unit tests for individual plugins:

```scala
package transputer.plugins.myfeature

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.{Transputer, T9000Transputer, T9000Param}

class MyFeatureSpec extends AnyFunSuite {
  
  // Test DUT with minimal plugin set
  class MyFeatureTestDut extends Component {
    val db = T9000Transputer.configureDatabase(T9000Param(enableMyFeature = true))
    
    val testPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.registers.RegFilePlugin(),
      new transputer.plugins.myfeature.MyFeaturePlugin()
    )
    
    val core = Database(db).on(Transputer(testPlugins))
    val myFeatureService = core.host[MyFeatureService]
    
    // Test interface
    val io = new Bundle {
      val enable = in Bool()
      val inputData = in UInt(32 bits)
      val outputData = out UInt(32 bits)
      val busy = out Bool()
    }
    
    // Connect to service
    when(io.enable) {
      myFeatureService.enable()
    }
    io.outputData := myFeatureService.process(io.inputData)
    io.busy := myFeatureService.isBusy
  }
  
  test("MyFeature basic functionality") {
    SimConfig.withWave.compile(new MyFeatureTestDut).doSim { dut =>
      SimTimeout(1000)
      dut.clockDomain.forkStimulus(10)
      
      // Test enable/disable
      dut.io.enable #= false
      dut.clockDomain.waitRisingEdge()
      assert(!dut.io.busy.toBoolean)
      
      dut.io.enable #= true
      dut.clockDomain.waitRisingEdge()
      
      // Test data processing
      dut.io.inputData #= 0x12345678
      dut.clockDomain.waitRisingEdge()
      
      // Wait for processing to complete
      while (dut.io.busy.toBoolean) {
        dut.clockDomain.waitRisingEdge()
      }
      
      // Verify output
      val expectedOutput = 0x12345678 // Adjust based on expected behavior
      assert(dut.io.outputData.toInt == expectedOutput)
    }
  }
  
  test("MyFeature error handling") {
    SimConfig.withWave.compile(new MyFeatureTestDut).doSim { dut =>
      // Test error conditions
      // Verify error flags
      // Test recovery behavior
    }
  }
}
```

### Integration Testing

Test plugin interactions with the full system:

```scala
class MyFeatureIntegrationSpec extends AnyFunSuite {
  
  class FullSystemTestDut extends Component {
    val param = T9000Param(enableMyFeature = true)
    val db = T9000Transputer.configureDatabase(param)
    val core = Database(db).on(T9000Transputer(param))
    
    // Full system interface
    val io = new Bundle {
      val cpuReset = in Bool()
      val cpuClock = in Bool()
      val testInput = in UInt(32 bits)
      val testOutput = out UInt(32 bits)
    }
  }
  
  test("MyFeature integration with full T9000") {
    SimConfig.withWave.compile(new FullSystemTestDut).doSim { dut =>
      // Test plugin within full system context
      // Verify interactions with other plugins
      // Test under load conditions
    }
  }
}
```

### Performance Testing

Measure plugin performance characteristics:

```scala
test("MyFeature performance metrics") {
  SimConfig.compile(new MyFeatureTestDut).doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    
    // Measure latency
    val startTime = simTime()
    dut.io.inputData #= 0x12345678
    dut.io.enable #= true
    
    while (dut.io.busy.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    
    val latency = simTime() - startTime
    println(s"Processing latency: ${latency} cycles")
    
    // Measure throughput
    val iterations = 1000
    val throughputStart = simTime()
    
    for (i <- 0 until iterations) {
      dut.io.inputData #= i
      while (dut.io.busy.toBoolean) {
        dut.clockDomain.waitRisingEdge()
      }
    }
    
    val throughputTime = simTime() - throughputStart
    val throughput = iterations.toDouble / (throughputTime / 10.0) // 10ns clock period
    println(s"Throughput: ${throughput} operations/second")
  }
}
```

## Common Patterns

### BMB Bus Integration

```scala
class MyFeatureBusLogic(systemBus: Bmb) extends Area {
  
  // Create BMB interface for plugin
  val pluginBus = Bmb(BmbParameter(
    addressWidth = 32,
    dataWidth = 64,  // Downsize from 128-bit system bus
    sourceWidth = 2,
    contextWidth = 4,
    lengthWidth = 4
  ))
  
  // Connect via downsizer
  val downsizer = BmbDownSizerBridge(
    inputParameter = systemBus.p,
    outputParameter = pluginBus.p
  )
  
  systemBus >> downsizer.io.input
  downsizer.io.output >> pluginBus
  
  // Use plugin bus for memory operations
  def readMemory(address: UInt): UInt = {
    pluginBus.cmd.valid := True
    pluginBus.cmd.opcode := 0  // Read
    pluginBus.cmd.address := address
    pluginBus.cmd.length := 0  // Single word
    
    pluginBus.rsp.data.asUInt
  }
}
```

### Register File Integration

```scala
class MyFeatureRegisterLogic(regfile: RegfileService) extends Area {
  
  // Define plugin-specific register names
  object MyFeatureRegs {
    val CONTROL = 0x40
    val STATUS = 0x41
    val DATA_IN = 0x42
    val DATA_OUT = 0x43
  }
  
  // Register access helpers
  def readControlReg(): UInt = regfile.read(MyFeatureRegs.CONTROL, 0)
  def writeControlReg(value: UInt): Unit = regfile.write(MyFeatureRegs.CONTROL, value, 0)
  
  def readStatusReg(): UInt = regfile.read(MyFeatureRegs.STATUS, 0)
  def writeStatusReg(value: UInt): Unit = regfile.write(MyFeatureRegs.STATUS, value, 0)
}
```

### Pipeline Integration

```scala
class MyFeaturePipelineLogic(pipeline: PipelineStageService) extends Area {
  
  // Get pipeline stage handles
  val executeStage = pipeline.execute
  val memoryStage = pipeline.memory
  
  // Add plugin logic to execute stage
  executeStage plug new Area {
    val myFeatureOperation = executeStage(MyFeaturePayload())
    
    when(myFeatureOperation.valid) {
      // Perform plugin operation
      val result = processData(myFeatureOperation.data)
      myFeatureOperation.result := result
      
      // Stall pipeline if needed
      when(operationBusy) {
        executeStage.haltWhen(True)
      }
    }
  }
}
```

### State Machine Pattern

```scala
class MyFeatureStateMachine extends Area {
  
  object State extends SpinalEnum {
    val IDLE, SETUP, PROCESSING, CLEANUP, ERROR = newElement()
  }
  
  val currentState = Reg(State()) init State.IDLE
  val nextState = State()
  
  // State transition logic
  switch(currentState) {
    is(State.IDLE) {
      when(startOperation) {
        nextState := State.SETUP
      } otherwise {
        nextState := State.IDLE
      }
    }
    
    is(State.SETUP) {
      when(setupComplete) {
        nextState := State.PROCESSING
      } otherwise {
        nextState := State.SETUP
      }
    }
    
    is(State.PROCESSING) {
      when(processingError) {
        nextState := State.ERROR
      } elsewhen(processingComplete) {
        nextState := State.CLEANUP
      } otherwise {
        nextState := State.PROCESSING
      }
    }
    
    is(State.CLEANUP) {
      when(cleanupComplete) {
        nextState := State.IDLE
      } otherwise {
        nextState := State.CLEANUP
      }
    }
    
    is(State.ERROR) {
      when(errorCleared) {
        nextState := State.IDLE
      } otherwise {
        nextState := State.ERROR
      }
    }
  }
  
  // Update state register
  currentState := nextState
  
  // State-dependent outputs
  val busy = currentState =/= State.IDLE
  val error = currentState === State.ERROR
}
```

## Debugging and Troubleshooting

### Common Issues

#### 1. Service Not Found
```
Error: Can't find the service MyFeatureService
```

**Solution:**
- Ensure plugin is included in plugin list
- Verify service is registered in setup phase
- Check service interface spelling and package

#### 2. Hierarchy Violations
```
Error: You can't read/write a signal that is not yours
```

**Solution:**
- Use service interfaces instead of direct signal access
- Ensure proper signal ownership in plugins
- Follow SpinalHDL hierarchy rules

#### 3. Database Access Errors
```
Error: Database key not found: MY_FEATURE_PARAM
```

**Solution:**
- Ensure database key is defined in Global object
- Set database value in TransputerPlugin setup
- Use correct key naming convention

#### 4. Pipeline Stalls
```
Issue: Pipeline performance degradation
```

**Solution:**
- Check haltWhen() conditions
- Verify ready/valid signals
- Optimize critical paths

### Debug Techniques

#### Plugin Tracing
```scala
class MyFeaturePlugin extends FiberPlugin {
  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start - dependencies: ${dependencies}")
    // ... setup logic
    println(s"[${this.getDisplayName()}] setup end - services: ${services}")
  }
  
  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    // ... build logic  
    println(s"[${this.getDisplayName()}] build complete")
  }
}
```

#### Waveform Analysis
```scala
test("Debug MyFeature with waveforms") {
  SimConfig
    .withWave // Generate VCD
    .withConfig(SpinalConfig(
      defaultClockDomainFrequency = FixedFrequency(100 MHz)
    ))
    .compile(new MyFeatureTestDut)
    .doSim { dut =>
      // Test with waveform capture
    }
}
```

#### Signal Naming
```scala
class MyFeatureLogic extends Area {
  val control = Reg(UInt(8 bits))
  val status = Reg(UInt(8 bits))
  val data = Reg(UInt(32 bits))
  
  // Add descriptive names for debugging
  control.setName("myfeature_control")
  status.setName("myfeature_status")  
  data.setName("myfeature_data")
}
```

## Best Practices

### Design Guidelines

#### 1. Keep Plugins Focused
- Single responsibility principle
- Clear, well-defined interfaces
- Minimal external dependencies

#### 2. Use Proper Error Handling
```scala
class MyFeaturePlugin extends FiberPlugin {
  during setup new Area {
    try {
      validateParameters()
      registerServices()
    } catch {
      case e: Exception =>
        println(s"Plugin setup failed: ${e.getMessage}")
        throw e
    }
  }
}
```

#### 3. Document Service Interfaces
```scala
/**
 * Service interface for MyFeature plugin.
 * 
 * Provides data processing capabilities with configurable modes.
 * 
 * @example
 * {{{
 * val service = Plugin[MyFeatureService]
 * service.configure(MyFeatureParams(mode = ADVANCED))
 * val result = service.process(inputData)
 * }}}
 */
trait MyFeatureService {
  /**
   * Process input data according to configured mode.
   * 
   * @param data Input data to process
   * @return Processed result
   * @note This operation may take multiple cycles
   */
  def process(data: UInt): UInt
}
```

#### 4. Use Configuration Parameters
```scala
case class MyFeatureParam(
  bufferSize: Int = 64,
  enableFeature: Boolean = true,
  operationMode: MyFeatureMode.Type = MyFeatureMode.BASIC
) {
  def validate(): Unit = {
    require(bufferSize > 0 && bufferSize <= 1024, s"Invalid buffer size: $bufferSize")
    require(List(MyFeatureMode.BASIC, MyFeatureMode.ADVANCED).contains(operationMode), 
            s"Invalid operation mode: $operationMode")
  }
}
```

### Performance Guidelines

#### 1. Optimize Critical Paths
- Use direct signal access for performance-critical operations
- Minimize combinational logic depth
- Pipeline complex operations

#### 2. Memory Access Optimization
- Use appropriate bus widths
- Implement caching where beneficial
- Batch memory operations

#### 3. Resource Sharing
- Share expensive resources (multipliers, memories) between plugins
- Use time-multiplexed designs for non-critical operations

### Testing Guidelines

#### 1. Comprehensive Coverage
- Test all service interface methods
- Verify error conditions and recovery
- Test boundary conditions and edge cases

#### 2. Realistic Test Scenarios
- Use representative data patterns
- Test under various load conditions
- Verify timing requirements

#### 3. Automated Testing
- Integrate tests into CI/CD pipeline
- Use property-based testing where appropriate
- Generate test reports and coverage metrics

---

*This plugin development guide provides the foundation for creating robust, maintainable plugins in the T9000 Transputer implementation. Follow these patterns and guidelines to ensure your plugins integrate seamlessly with the overall system architecture.*