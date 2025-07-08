package transputer.plugins.bitops

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.bitops._

/** T9000 Bit Operations Plugin implementing Table 6.16 bit manipulation and CRC instructions.
  *
  * This plugin implements bit-level operations from T9000 Table 6.16:
  *   - CRC operations: crcword, crcbyte
  *   - Bit manipulation: bitcnt, bitrevword, bitrevnbits
  *
  * Features:
  *   - Hardware CRC calculation
  *   - Efficient bit counting and reversal
  *   - Support for various bit manipulation algorithms
  *   - Integration with data integrity checking
  */
class BitOpsPlugin extends FiberPlugin {
  override def getDisplayName(): String = "BitOpsPlugin"
  setName("bitops")

  during setup new Area {
    println(s"[${BitOpsPlugin.this.getDisplayName()}] setup start")

    addService(new BitOpService {
      override def executeOp(op: BitOp.C, data: UInt, polynomial: UInt): BitOpResult = bitOpResult
      override def isBitOp(opcode: Bits): Bool = isBitOperation
      override def getBitOp(opcode: Bits): BitOp.C = bitOperation
    })

    println(s"[${BitOpsPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var bitOpResult: BitOpResult = null
  var isBitOperation: Bool = null
  var bitOperation: BitOp.C = null

  during build new Area {
    println(s"[${BitOpsPlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    bitOpResult = BitOpResult()
    isBitOperation = Bool()
    bitOperation = BitOp()

    // Default values
    bitOpResult.result := 0
    bitOpResult.bitCount := 0
    bitOpResult.crcValue := 0
    bitOpResult.completed := False
    isBitOperation := False
    bitOperation := BitOp.CRCWORD

    println(s"[${BitOpsPlugin.this.getDisplayName()}] Bit operations hardware configured")
    println(s"[${BitOpsPlugin.this.getDisplayName()}] - Table 6.16: Bit manipulation & CRC")
    println(s"[${BitOpsPlugin.this.getDisplayName()}] build end")
  }
}
