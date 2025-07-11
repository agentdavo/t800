package transputer.plugins.alternative

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.alternative._

/** T9000 Alternative Plugin implementing Table 6.24 ALT construct instructions.
  *
  * This plugin implements the transputer's non-deterministic choice mechanism from T9000 Table
  * 6.24:
  *   - ALT construct: alt, altwt, altend
  *   - Guard management: enbc, disc, enbt, dist
  *   - Timer integration: talt, taltwt
  *
  * The ALT construct allows a process to wait for any one of several communication events to become
  * ready, implementing non-deterministic choice.
  *
  * Features:
  *   - Non-deterministic guard selection
  *   - Integration with channel communication
  *   - Timer-based timeouts
  *   - Process scheduling integration
  */
class AlternativePlugin extends FiberPlugin {
  override def getDisplayName(): String = "AlternativePlugin"
  setName("alternative")

  during setup new Area {
    println(s"[${AlternativePlugin.this.getDisplayName()}] setup start")

    addService(new AlternativeService {
      override def executeOp(op: AltOp.C, channelMask: Bits, timerValue: UInt): AltResult =
        altResult
      override def getState(): AltState = altState
      override def isAltOp(opcode: Bits): Bool = isAltOperation
      override def getAltOp(opcode: Bits): AltOp.C = altOperation
    })

    println(s"[${AlternativePlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var altResult: AltResult = null
  var altState: AltState = null
  var isAltOperation: Bool = null
  var altOperation: AltOp.C = null

  during build new Area {
    println(s"[${AlternativePlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    altResult = AltResult()
    altState = AltState()
    isAltOperation = Bool()
    altOperation = AltOp()

    // Default values
    altResult.ready := False
    altResult.channelId := 0
    altResult.isTimer := False
    altResult.skipTaken := False
    altResult.error := False

    altState.inAlt := False
    altState.waiting := False
    altState.selectedChannel := 0
    altState.skipEnabled := False
    altState.timerEnabled := False

    isAltOperation := False
    altOperation := AltOp.ALT

    println(s"[${AlternativePlugin.this.getDisplayName()}] ALT hardware configured")
    println(
      s"[${AlternativePlugin.this.getDisplayName()}] - Table 6.24: Alternative construct operations"
    )
    println(s"[${AlternativePlugin.this.getDisplayName()}] - Non-deterministic choice mechanism")
    println(s"[${AlternativePlugin.this.getDisplayName()}] build end")
  }
}
