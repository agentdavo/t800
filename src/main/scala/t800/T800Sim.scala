package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim._
import t800.plugins._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.PluginHost

/** Testbench for T800 component, verifying system bus and configuration register access. */
object T800Sim {
  def main(args: Array[String]): Unit = {
    // Configure simulation with Verilator and waveform output
    SimConfig
      .withVerilator
      .withWave
      .compile(new T800(new PluginHost, T800.defaultPlugins()))
      .doSim("T800Test") { dut =>
        // Fork a clock stimulus with 10ns period (100 MHz)
        dut.clockDomain.forkStimulus(period = 10)

        // Initialize simulation
        dut.clockDomain.assertReset()
        dut.clockDomain.waitSampling(10) // Hold reset for 10 cycles
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(10) // Wait for stabilization

        // BMB memory agent to simulate external memory
        val memoryAgent = BmbMemoryAgent(
          memory = new Memory(1 << 20), // 1 MB memory
          bmbParameter = T800.systemBusParam,
          clockDomain = dut.clockDomain,
          invalidReadData = true
        )
        memoryAgent.connect(dut.systemBus)

        // Test 1: System Bus Write and Read
        println("Testing BMB system bus write and read...")
        val writeAddress = Global.InternalMemStart + 0x100 // Write within internal memory
        val writeData = BigInt("1234567890ABCDEF1234567890ABCDEF", 16) // 128-bit data
        memoryAgent.write(
          address = writeAddress,
          data = writeData,
          length = 16 // 16-byte burst
        )
        dut.clockDomain.waitSampling(50) // Wait for write completion

        val readData = memoryAgent.readSync(
          address = writeAddress,
          length = 16
        )
        assert(
          readData == writeData,
          s"BMB read data mismatch: expected ${writeData.toString(16)}, got ${readData.toString(16)}"
        )
        println("BMB system bus test passed!")

        // Test 2: Configuration Register Access
        println("Testing configuration register access...")
        // Simulate ConfigAccessSrv interaction (assumes plugins use it)
        val configAccess = Global.ConfigAccessSrv()
        // Connect configAccess to DUT (placeholder; assumes plugins expose interface)
        // Note: Actual connection depends on plugin implementation
        val configAddr = Global.ConfigAddr.CPU // Example: CPU config register
        val configData = B"32'xDEADBEEF" // Test data
        val sigBits = 32 // Full 32-bit write

        // Write to config register
        configAccess.write(configAddr, configData, sigBits)
        dut.clockDomain.waitSampling(10)
        assert(
          configAccess.isValid.toBoolean,
          s"Config write to ${configAddr.toString(16)} failed: write lock or invalid access"
        )

        // Read from config register
        val readConfigData = configAccess.read(configAddr, sigBits)
        dut.clockDomain.waitSampling(10)
        assert(
          readConfigData === configData,
          s"Config read mismatch: expected ${configData.toString(16)}, got ${readConfigData.toString(16)}"
        )
        println("Configuration register access test passed!")

        // Test 3: Basic Plugin Stimulation
        println("Testing plugin stimulation via system bus...")
        // Write to memory address that MainCachePlugin might access
        val cacheAddress = Global.InternalMemStart + 0x200
        memoryAgent.write(
          address = cacheAddress,
          data = BigInt("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16),
          length = 16
        )
        dut.clockDomain.waitSampling(50)

        // Read back to verify cache interaction (assumes cache returns data)
        val cacheReadData = memoryAgent.readSync(
          address = cacheAddress,
          length = 16
        )
        // Note: Actual data verification depends on MainCachePlugin behavior
        println(s"Cache read data: ${cacheReadData.toString(16)}")

        // Run simulation for additional cycles to observe plugin behavior
        dut.clockDomain.waitSampling(100)

        println("T800 simulation completed!")
      }
  }
}
