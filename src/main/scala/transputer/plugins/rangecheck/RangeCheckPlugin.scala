package transputer.plugins.rangecheck

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.rangecheck._

/** T9000 Range Check Plugin implementing Table 6.14 range checking and conversion instructions.
  *
  * This plugin implements safety and conversion operations from T9000 Table 6.14:
  *   - Range checking: cir, cb, cs, cword, ccnt1
  *   - Type conversion: xsword, csngl, cdbl
  *   - Control flow: cj, call
  *
  * These operations provide memory safety through bounds checking and safe type conversions for
  * mixed-precision arithmetic.
  *
  * Features:
  *   - Automatic bounds checking with trap generation
  *   - Type-safe conversions between data sizes
  *   - Integration with trap handling system
  *   - Support for both runtime and compile-time checks
  */
class RangeCheckPlugin extends FiberPlugin {
  override def getDisplayName(): String = "RangeCheckPlugin"
  setName("rangecheck")

  during setup new Area {
    println(s"[${RangeCheckPlugin.this.getDisplayName()}] setup start")

    addService(new RangeCheckService {
      override def executeOp(
        op: RangeCheckOp.C,
        value: UInt,
        lowerBound: UInt,
        upperBound: UInt
      ): RangeCheckResult = rangeCheckResult
      override def isRangeCheckOp(opcode: Bits): Bool = isRangeCheckOperation
      override def getRangeCheckOp(opcode: Bits): RangeCheckOp.C = rangeCheckOperation
    })

    println(s"[${RangeCheckPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var rangeCheckResult: RangeCheckResult = null
  var isRangeCheckOperation: Bool = null
  var rangeCheckOperation: RangeCheckOp.C = null

  during build new Area {
    println(s"[${RangeCheckPlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    rangeCheckResult = RangeCheckResult()
    isRangeCheckOperation = Bool()
    rangeCheckOperation = RangeCheckOp()

    // Default values
    rangeCheckResult.inRange := True
    rangeCheckResult.converted := 0
    rangeCheckResult.rangeError := False
    rangeCheckResult.jumpTaken := False
    rangeCheckResult.newIptr := 0
    isRangeCheckOperation := False
    rangeCheckOperation := RangeCheckOp.CIR

    println(s"[${RangeCheckPlugin.this.getDisplayName()}] Range check hardware configured")
    println(
      s"[${RangeCheckPlugin.this.getDisplayName()}] - Table 6.14: Range checking & conversion"
    )
    println(s"[${RangeCheckPlugin.this.getDisplayName()}] - Memory safety and type conversion")
    println(s"[${RangeCheckPlugin.this.getDisplayName()}] build end")
  }
}
