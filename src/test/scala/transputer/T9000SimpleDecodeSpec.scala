package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

/** Simplified tests for T9000 instruction decode operations (M-2 milestone).
  *
  * Tests individual decode logic without complex plugin dependencies.
  */
class T9000SimpleDecodeSpec extends AnyFunSuite {

  // Simple test component with minimal dependencies
  class SimpleDecodeTestDut extends Component {
    val io = new Bundle {
      val instruction = in Bits (8 bits)
      val execute = in Bool ()
      val reset = in Bool ()

      val executed = out Bool ()
      val operand = out UInt (32 bits)
      val stackA = out UInt (32 bits)
      val stackDepth = out UInt (3 bits)
    }

    // Simple operand register for prefix operations
    val operand = Reg(UInt(32 bits)) init 0

    // Simple stack registers
    val stackA = Reg(UInt(32 bits)) init 0
    val stackDepth = Reg(UInt(3 bits)) init 0

    // Decode logic (simplified version of SimpleDecodeLogic)
    val executed = Reg(Bool()) init False
    executed := False

    when(io.reset) {
      operand := 0
      stackA := 0
      stackDepth := 0
    }.elsewhen(io.execute) {
      // Extract function and data from 8-bit instruction
      val function = io.instruction(7 downto 4)
      val data = io.instruction(3 downto 0).asUInt

      switch(function) {
        // LDC: Load Constant - push (operand | data) onto stack
        is(Opcode.Primary.LDC(7 downto 4)) {
          val value = operand | data.resized
          stackA := value
          operand := 0 // Clear operand after use
          when(stackDepth < 3) {
            stackDepth := stackDepth + 1
          }
          executed := True
        }

        // PFIX: Positive Prefix - accumulate operand
        is(Opcode.Primary.PFIX(7 downto 4)) {
          operand := (operand |<< 4) | data.resized
          executed := True
        }

        // NFIX: Negative Prefix - accumulate negative operand
        is(Opcode.Primary.NFIX(7 downto 4)) {
          operand := ~((~operand |<< 4) | data.resized)
          executed := True
        }

        // ADC: Add Constant - add (operand | data) to top of stack
        is(Opcode.Primary.ADC(7 downto 4)) {
          val value = operand | data.resized
          stackA := stackA + value
          operand := 0 // Clear operand after use
          executed := True
        }

        // EQC: Equal Constant - compare top of stack with constant
        is(Opcode.Primary.EQC(7 downto 4)) {
          val value = operand | data.resized
          stackA := (stackA === value) ? U(1) | U(0)
          operand := 0 // Clear operand after use
          executed := True
        }
      }
    }

    // Connect outputs
    io.executed := executed
    io.operand := operand
    io.stackA := stackA
    io.stackDepth := stackDepth
  }

  test("T9000 simple decode compilation") {
    // Test that the simple decode compiles and elaborates
    SimConfig.compile(new SimpleDecodeTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      // Basic test - verify it doesn't crash
      dut.io.instruction #= Opcode.Primary.LDC.toInt
      dut.io.execute #= false
      dut.io.reset #= false
      dut.clockDomain.waitRisingEdge()

      // Test completed successfully if we get here
      assert(true, "Simple decode compiled and elaborated successfully")
    }
  }

  test("T9000 PFIX operation standalone") {
    SimConfig.withWave.compile(new SimpleDecodeTestDut).doSim { dut =>
      SimTimeout(100)
      dut.clockDomain.forkStimulus(10)

      // Reset first
      dut.io.reset #= true
      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      dut.io.reset #= false
      dut.clockDomain.waitRisingEdge()

      // Test PFIX: accumulate operand
      // PFIX 0x5 should set operand to 0x5
      val pfixInstr = (Opcode.Primary.PFIX.toInt & 0xf0) | 0x5
      dut.io.instruction #= pfixInstr
      dut.io.execute #= true
      dut.clockDomain.waitRisingEdge()

      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      println(s"PFIX executed: ${dut.io.executed.toBoolean}")
      println(s"Operand after PFIX 0x5: 0x${dut.io.operand.toInt.toHexString}")

      // Verify PFIX operation
      assert(dut.io.executed.toBoolean, "PFIX should have executed")
      assert(
        dut.io.operand.toInt == 0x5,
        s"Operand should be 0x5, got 0x${dut.io.operand.toInt.toHexString}"
      )
    }
  }

  test("T9000 LDC operation standalone") {
    SimConfig.withWave.compile(new SimpleDecodeTestDut).doSim { dut =>
      SimTimeout(100)
      dut.clockDomain.forkStimulus(10)

      // Reset first
      dut.io.reset #= true
      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      dut.io.reset #= false
      dut.clockDomain.waitRisingEdge()

      // Test LDC: load constant
      // LDC 0xA should push 0xA onto stack
      val ldcInstr = (Opcode.Primary.LDC.toInt & 0xf0) | 0xa
      dut.io.instruction #= ldcInstr
      dut.io.execute #= true
      dut.clockDomain.waitRisingEdge()

      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      println(s"LDC executed: ${dut.io.executed.toBoolean}")
      println(s"Stack A after LDC 0xA: 0x${dut.io.stackA.toInt.toHexString}")
      println(s"Stack depth: ${dut.io.stackDepth.toInt}")

      // Verify LDC operation
      assert(dut.io.executed.toBoolean, "LDC should have executed")
      assert(
        dut.io.stackA.toInt == 0xa,
        s"Stack A should be 0xA, got 0x${dut.io.stackA.toInt.toHexString}"
      )
      assert(
        dut.io.stackDepth.toInt == 1,
        s"Stack depth should be 1, got ${dut.io.stackDepth.toInt}"
      )
    }
  }

  test("T9000 PFIX + LDC combination") {
    SimConfig.withWave.compile(new SimpleDecodeTestDut).doSim { dut =>
      SimTimeout(100)
      dut.clockDomain.forkStimulus(10)

      // Reset first
      dut.io.reset #= true
      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      dut.io.reset #= false
      dut.clockDomain.waitRisingEdge()

      // Step 1: PFIX 0x1 (should set operand to 0x1)
      val pfixInstr = (Opcode.Primary.PFIX.toInt & 0xf0) | 0x1
      dut.io.instruction #= pfixInstr
      dut.io.execute #= true
      dut.clockDomain.waitRisingEdge()

      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      println(s"After PFIX 0x1: operand = 0x${dut.io.operand.toInt.toHexString}")
      assert(dut.io.operand.toInt == 0x1, "Operand should be 0x1 after PFIX")

      // Step 2: LDC 0x5 (should push 0x15 onto stack)
      val ldcInstr = (Opcode.Primary.LDC.toInt & 0xf0) | 0x5
      dut.io.instruction #= ldcInstr
      dut.io.execute #= true
      dut.clockDomain.waitRisingEdge()

      dut.io.execute #= false
      dut.clockDomain.waitRisingEdge()

      println(s"After LDC 0x5: stack A = 0x${dut.io.stackA.toInt.toHexString}")
      println(s"After LDC: operand = 0x${dut.io.operand.toInt.toHexString}")

      // Verify combined operation
      assert(
        dut.io.stackA.toInt == 0x15,
        s"Stack A should be 0x15 (0x1<<4 | 0x5), got 0x${dut.io.stackA.toInt.toHexString}"
      )
      assert(dut.io.operand.toInt == 0x0, "Operand should be cleared after LDC")
    }
  }
}
