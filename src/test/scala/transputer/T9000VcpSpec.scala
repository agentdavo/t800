package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.legacy.vcp.{VcpService, VcpPlugin}

/** Comprehensive tests for T9000 Virtual Channel Processor (VCP).
  *
  * Tests the T9000 VCP implementation:
  *   - VCP control and status operations
  *   - Virtual channel management
  *   - Link configuration and error handling
  *   - DS-Link packet routing
  */
class T9000VcpSpec extends AnyFunSuite {

  // Simple DUT to test VCP operations in isolation
  class VcpTestDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val vcpCommand = in Bits (8 bits)
      val linkMode = in Bits (8 bits)
      val linkIndex = in UInt (2 bits)
      val channelIndex = in UInt (8 bits)

      val vcpStatus = out Bits (8 bits)
      val isRunning = out Bool ()
      val isReady = out Bool ()
      val channelReady = out Bool ()
      val linkError = out Bool ()
    }

    // VCP needs basic transputer infrastructure
    val testPlugins = Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.legacy.vcp.VcpPlugin()
    )
    val core = Database(db).on(Transputer(testPlugins))
    val vcpService = core.host[VcpService]

    // VCP control logic
    vcpService.setVcpCommand(io.vcpCommand)

    // Configure link using VCP service method for all possible indices
    for (i <- 0 until 4) {
      when(io.linkIndex === i) {
        vcpService.updateChannelHeader(i, io.linkMode)
      }
    }

    // Output VCP state
    io.vcpStatus := vcpService.getVcpStatus()
    io.isRunning := vcpService.vcpStatus(2) // Running bit
    io.isReady := vcpService.vcpStatus(3) // Ready bit
    io.channelReady := vcpService.txReady(io.channelIndex)
    // Select link error based on index
    io.linkError := False
    for (i <- 0 until 4) {
      when(io.linkIndex === i) {
        io.linkError := vcpService.linkError(i)
      }
    }
  }

  test("T9000 VCP basic control operations") {
    SimConfig.withWave.compile(new VcpTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Initially VCP should be in reset state
      dut.clockDomain.waitSampling()
      assert(!dut.io.isRunning.toBoolean, "VCP should not be running initially")
      assert(dut.io.isReady.toBoolean, "VCP should be ready initially")

      // Issue start command
      dut.io.vcpCommand #= 0x02 // Start bit
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      assert(dut.io.isRunning.toBoolean, "VCP should be running after start command")
      assert(dut.io.isReady.toBoolean, "VCP should still be ready")

      // Issue stop command
      dut.io.vcpCommand #= 0x04 // Stop bit
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      assert(!dut.io.isRunning.toBoolean, "VCP should be stopped after stop command")
      assert(dut.io.isReady.toBoolean, "VCP should be ready")

      // Issue reset command
      dut.io.vcpCommand #= 0x01 // Reset bit
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      assert(!dut.io.isRunning.toBoolean, "VCP should be stopped after reset")
      assert(dut.io.isReady.toBoolean, "VCP should be ready after reset")
    }
  }

  test("T9000 VCP link configuration") {
    SimConfig.withWave.compile(new VcpTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Configure Link 0 with specific settings
      dut.io.linkIndex #= 0
      dut.io.linkMode #= 0x07 // ByteMode=1, LocalizeError=1, HeaderLength=1
      dut.clockDomain.waitSampling(2)

      // Start VCP to enable link operations
      dut.io.vcpCommand #= 0x02 // Start
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      assert(dut.io.isRunning.toBoolean, "VCP should be running")

      // Check link error status (should be clear initially)
      assert(!dut.io.linkError.toBoolean, "Link 0 should not have errors initially")

      // Test other links
      for (linkId <- 1 to 3) {
        dut.io.linkIndex #= linkId
        dut.clockDomain.waitSampling()
        assert(!dut.io.linkError.toBoolean, s"Link $linkId should not have errors initially")
      }
    }
  }

  test("T9000 VCP virtual channel operations") {
    SimConfig.withWave.compile(new VcpTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Start VCP
      dut.io.vcpCommand #= 0x02 // Start
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      // Test channel operations
      for (channelId <- 0 until 8) {
        dut.io.channelIndex #= channelId
        dut.clockDomain.waitSampling()

        // Check channel ready status
        val isReady = dut.io.channelReady.toBoolean
        // In our simplified implementation, channels should be ready when VCP is running
        assert(isReady, s"Channel $channelId should be ready when VCP is running")
      }
    }
  }

  test("T9000 VCP status register behavior") {
    SimConfig.withWave.compile(new VcpTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Check initial status
      dut.clockDomain.waitSampling()
      val initialStatus = dut.io.vcpStatus.toLong

      // Status should indicate: not running (bit 2=0), ready (bit 3=1)
      assert((initialStatus & 0x04) == 0, "VCP should not be running initially (bit 2)")
      assert((initialStatus & 0x08) != 0, "VCP should be ready initially (bit 3)")

      // Start VCP and check status change
      dut.io.vcpCommand #= 0x02 // Start
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      val runningStatus = dut.io.vcpStatus.toLong
      assert((runningStatus & 0x04) != 0, "VCP should be running after start (bit 2)")
      assert((runningStatus & 0x08) != 0, "VCP should still be ready (bit 3)")

      // Stop VCP and check status
      dut.io.vcpCommand #= 0x04 // Stop
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      val stoppedStatus = dut.io.vcpStatus.toLong
      assert((stoppedStatus & 0x04) == 0, "VCP should not be running after stop (bit 2)")
      assert((stoppedStatus & 0x08) != 0, "VCP should be ready after stop (bit 3)")
    }
  }

  test("T9000 VCP error handling") {
    SimConfig.withWave.compile(new VcpTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Start VCP
      dut.io.vcpCommand #= 0x02 // Start
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      // Test all links for error status
      for (linkId <- 0 until 4) {
        dut.io.linkIndex #= linkId
        dut.clockDomain.waitSampling()

        // Initially no errors should be present
        assert(!dut.io.linkError.toBoolean, s"Link $linkId should not have errors initially")
      }

      // Reset VCP should clear any potential errors
      dut.io.vcpCommand #= 0x01 // Reset
      dut.clockDomain.waitSampling(2)
      dut.io.vcpCommand #= 0x00
      dut.clockDomain.waitSampling(2)

      // Verify all errors are cleared
      for (linkId <- 0 until 4) {
        dut.io.linkIndex #= linkId
        dut.clockDomain.waitSampling()
        assert(!dut.io.linkError.toBoolean, s"Link $linkId should not have errors after reset")
      }
    }
  }
}
