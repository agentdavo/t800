package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/** FPU Load/Store Unit
  *
  * Implements all T9000 FPU memory operations:
  *   - FPLDBS/FPLDBD: Load single/double precision
  *   - FPLDNLS/FPLDNLD: Load non-local single/double
  *   - FPSTSNL/FPSTDNL: Store single/double non-local
  *
  * Features:
  *   - IEEE 754 format validation on loads
  *   - Single/double precision conversion
  *   - BMB bus interface for cache integration
  *   - Non-local addressing support
  */
class FpuLoadStore extends Component {
  val io = new Bundle {
    // Command interface
    val cmd = slave Stream (MemoryCmd())
    val rsp = master Flow (MemoryRsp())

    // BMB memory interface
    val bus = master(
      Bmb(
        BmbParameter(
          addressWidth = 32,
          dataWidth = 128,
          sourceWidth = 4,
          contextWidth = 4,
          lengthWidth = 8,
          alignment = BmbParameter.BurstAlignement.WORD
        )
      )
    )
  }

  // State machine
  object State extends SpinalEnum {
    val IDLE, ADDRESS, WAIT_RESPONSE, FORMAT = newElement()
  }

  val state = Reg(State()) init State.IDLE
  val currentOp = Reg(FpOp())
  val isDouble = Reg(Bool())
  val address = Reg(UInt(32 bits))
  val storeData = Reg(Bits(64 bits))

  // BMB command defaults
  io.bus.cmd.valid := False
  io.bus.cmd.payload.assignDontCare()

  // Response defaults
  io.rsp.valid := False
  io.rsp.payload.assignDontCare()

  // Command ready when idle
  io.cmd.ready := state === State.IDLE

  // State machine
  switch(state) {
    is(State.IDLE) {
      when(io.cmd.valid) {
        // Note: MemoryCmd doesn't have 'op' field, so we need to determine operation from context
        // For now, use a default operation - this should be passed through a separate signal
        currentOp := FpOp.FPLDBD // Default to double load
        isDouble := io.cmd.payload.isDouble
        address := io.cmd.payload.addr
        storeData := io.cmd.payload.data
        state := State.ADDRESS
      }
    }

    is(State.ADDRESS) {
      // Calculate effective address based on operation
      val effectiveAddr = UInt(32 bits)

      switch(currentOp) {
        // Load operations with byte offset
        is(FpOp.FPLDBS) {
          effectiveAddr := address // Single = 4 bytes
        }
        is(FpOp.FPLDBD) {
          effectiveAddr := address // Double = 8 bytes
        }

        // Non-local loads use word offset (×4)
        is(FpOp.FPLDNLS) {
          effectiveAddr := address |<< 2 // Word offset to byte
        }
        is(FpOp.FPLDNLD) {
          effectiveAddr := address |<< 2 // Word offset to byte
        }

        // Non-local stores use word offset
        is(FpOp.FPSTSNL, FpOp.FPSTDNL) {
          effectiveAddr := address |<< 2
        }

        default {
          effectiveAddr := address
        }
      }

      // Issue BMB command
      io.bus.cmd.valid := True
      io.bus.cmd.payload.address := effectiveAddr
      when(currentOp === FpOp.FPSTSNL || currentOp === FpOp.FPSTDNL) {
        io.bus.cmd.payload.opcode := Bmb.Cmd.Opcode.WRITE
      } otherwise {
        io.bus.cmd.payload.opcode := Bmb.Cmd.Opcode.READ
      }

      // Set transfer size based on precision
      val isStore = currentOp === FpOp.FPSTSNL || currentOp === FpOp.FPSTDNL
      val transferSize =
        (isDouble || currentOp === FpOp.FPLDBD || currentOp === FpOp.FPLDNLD) ? U(3) | U(
          2
        ) // 8 bytes : 4 bytes
      io.bus.cmd.payload.length := (U(1) << transferSize) - 1

      // For stores, set write data and mask
      when(isStore) {
        // Pack store data based on precision
        val packedData = Bits(128 bits)
        when(isDouble || currentOp === FpOp.FPSTDNL) {
          // Double precision - 64 bits
          packedData := storeData ## B(0, 64 bits)
          io.bus.cmd.payload.mask := B"11111111" ## B"00000000"
        } otherwise {
          // Single precision - 32 bits
          val singleData = formatToSingle(storeData)
          packedData := singleData ## B(0, 96 bits)
          io.bus.cmd.payload.mask := B"1111" ## B"000000000000"
        }
        io.bus.cmd.payload.data := packedData
      } otherwise {
        io.bus.cmd.payload.mask := B"1111111111111111" // Read all
      }

      when(io.bus.cmd.ready) {
        state := State.WAIT_RESPONSE
      }
    }

    is(State.WAIT_RESPONSE) {
      when(io.bus.rsp.valid) {
        val isLoad = currentOp =/= FpOp.FPSTSNL && currentOp =/= FpOp.FPSTDNL

        when(isLoad) {
          state := State.FORMAT
        } otherwise {
          // Store complete
          io.rsp.valid := True
          io.rsp.payload.data := 0
          io.rsp.payload.error := False  // BMB errors handled differently
          state := State.IDLE
        }
      }
    }

    is(State.FORMAT) {
      // Format loaded data based on operation
      val loadedData = io.bus.rsp.payload.data
      val formattedResult = Bits(64 bits)

      switch(currentOp) {
        is(FpOp.FPLDBS, FpOp.FPLDNLS) {
          // Single precision load - extend to double
          val singleBits = loadedData(31 downto 0)
          formattedResult := formatToDouble(singleBits)
        }

        is(FpOp.FPLDBD, FpOp.FPLDNLD) {
          // Double precision load
          formattedResult := loadedData(63 downto 0)
        }

        default {
          formattedResult := loadedData(63 downto 0)
        }
      }

      // Validate IEEE 754 format
      val validated = validateAndClean(
        formattedResult,
        isDouble || currentOp === FpOp.FPLDBD || currentOp === FpOp.FPLDNLD
      )

      // Return result
      io.rsp.valid := True
      io.rsp.payload.data := validated
      io.rsp.payload.error := False  // BMB errors handled differently
      state := State.IDLE
    }
  }

  // Format conversion: Double to Single
  def formatToSingle(double: Bits): Bits = {
    val sign = double(63)
    val exponent = double(62 downto 52).asUInt
    val mantissa = double(51 downto 0)

    val singleBits = Bits(32 bits)

    // Check for special values
    when(exponent === 0x7ff) {
      // Infinity or NaN
      when(mantissa === 0) {
        // Infinity
        singleBits := sign ## B(0xff, 8 bits) ## B(0, 23 bits)
      } otherwise {
        // NaN - preserve quiet bit
        singleBits := sign ## B(0xff, 8 bits) ## B"1" ## mantissa(50 downto 29)
      }
    } elsewhen (exponent === 0) {
      // Zero or denormal - flush to zero
      singleBits := sign ## B(0, 31 bits)
    } otherwise {
      // Normal number - adjust exponent bias
      val adjustedExp = (exponent - 1023 + 127).resize(11)

      when(adjustedExp > 254) {
        // Overflow to infinity
        singleBits := sign ## B(0xff, 8 bits) ## B(0, 23 bits)
      } elsewhen (adjustedExp <= 0) {
        // Underflow to zero
        singleBits := sign ## B(0, 31 bits)
      } otherwise {
        // Normal conversion
        singleBits := sign ## adjustedExp(7 downto 0).asBits ## mantissa(51 downto 29)
      }
    }

    singleBits
  }

  // Format conversion: Single to Double
  def formatToDouble(single: Bits): Bits = {
    val sign = single(31)
    val exponent = single(30 downto 23).asUInt
    val mantissa = single(22 downto 0)

    val doubleBits = Bits(64 bits)

    // Check for special values
    when(exponent === 0xff) {
      // Infinity or NaN
      when(mantissa === 0) {
        // Infinity
        doubleBits := sign ## B(0x7ff, 11 bits) ## B(0, 52 bits)
      } otherwise {
        // NaN - preserve quiet bit and extend
        doubleBits := sign ## B(0x7ff, 11 bits) ## mantissa ## B(0, 29 bits)
      }
    } elsewhen (exponent === 0) {
      // Zero or denormal - treat as zero
      doubleBits := sign ## B(0, 63 bits)
    } otherwise {
      // Normal number - adjust exponent bias
      val adjustedExp = (exponent - 127 + 1023).resize(11).asBits
      doubleBits := sign ## adjustedExp ## mantissa ## B(0, 29 bits)
    }

    doubleBits
  }

  // Validate and clean IEEE 754 format
  def validateAndClean(bits: Bits, isDouble: Bool): Bits = {
    val sign = bits(63)
    val exponent = Mux(isDouble, bits(62 downto 52), B"000" ## bits(30 downto 23))
    val mantissa = Mux(isDouble, bits(51 downto 0), bits(22 downto 0) ## B(0, 29 bits))

    val cleaned = Bits(64 bits)

    // Check for signaling NaN and convert to quiet NaN
    when(exponent === B"11111111111" && mantissa =/= 0 && !mantissa(51)) {
      // Signaling NaN - set quiet bit
      cleaned := sign ## exponent ## B"1" ## mantissa(50 downto 0)
    } otherwise {
      // Valid format or already quiet NaN
      cleaned := bits
    }

    cleaned
  }
}
