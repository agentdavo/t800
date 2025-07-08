package transputer.plugins.core.grouper

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.plugins.core.fetch.InstrFetchService

/** T9000 Hardware Instruction Grouper implementing superscalar instruction grouping.
  *
  * Based on T9000 specification:
  *   - Scans instruction stream and automatically groups instructions for concurrent execution
  *   - Groups instructions based on pipeline resource availability and dependencies
  *   - Optimizes pipeline loading without programmer intervention
  *   - Handles primary instructions (0-12), PFIX/NFIX prefixes, and secondary instructions (OPR)
  *
  * T9000 Primary Instructions (function codes 0-12):
  *   - 0: j (jump), 1: ldlp (load local pointer), 2: pfix (prefix)
  *   - 3: ldnl (load non-local), 4: ldc (load constant), 5: ldnlp (load non-local pointer)
  *   - 6: nfix (negative prefix), 7: ldl (load local), 8: adc (add constant)
  *   - 9: call (call), 10: cj (conditional jump), 11: ajw (adjust workspace)
  *   - 12: eqc (equals constant), 13: stl (store local), 14: stnl (store non-local)
  *   - 15: opr (operate - secondary instructions)
  */
class InstrGrouperPlugin extends FiberPlugin {
  override def getDisplayName(): String = "InstrGrouperPlugin"
  setName("grouper")

  private var groupFlow: Stream[GroupedInstructions] = null

  // T9000 instruction grouping state
  private var instrDataValue: UInt = null
  private var groupBuffer: Vec[UInt] = null
  private var groupCount: UInt = null
  private var prefixActive: Bool = null

  during setup new Area {
    groupFlow = Stream(GroupedInstructions())
    groupFlow.setIdle()
    addService(new GroupedInstrService {
      override def groups: Stream[GroupedInstructions] = groupFlow
    })
  }

  during build new Area {
    val fetchService = Plugin[InstrFetchService]

    // T9000 Hardware Instruction Grouper Logic
    val grouperLogic = new Area {
      val instrStream = fetchService.instrStream

      // Grouper state registers
      instrDataValue = Reg(UInt(32 bits)) init 0
      groupBuffer = Vec(Reg(UInt(8 bits)) init 0, 8) // Max 8 instructions per group
      groupCount = Reg(UInt(4 bits)) init 0
      prefixActive = Reg(Bool()) init False

      // Instruction decoding
      val currentInstr = instrStream.payload.asUInt
      val function = currentInstr(7 downto 4)
      val data = currentInstr(3 downto 0)

      // Primary instruction types
      val isJump = function === 0 || function === 10 // j or cj
      val isLoad =
        function === 1 || function === 3 || function === 4 || function === 5 || function === 7 // ldlp, ldnl, ldc, ldnlp, ldl
      val isStore = function === 13 || function === 14 // stl, stnl
      val isArithmetic = function === 8 || function === 12 // adc, eqc
      val isWorkspace = function === 9 || function === 11 // call, ajw
      val isPrefix = function === 2 || function === 6 // pfix, nfix
      val isOperate = function === 15 // opr (secondary instructions)

      // Group termination conditions (based on T9000 pipeline constraints)
      val terminateGroup = isJump || isOperate || (groupCount === 7)

      // Simplified instruction grouping logic (state machine to be enhanced later)
      val groupingLogic = new Area {
        when(instrStream.valid) {
          when(isPrefix) {
            // Handle PFIX/NFIX prefix instructions
            when(function === 2) { // PFIX
              instrDataValue := (instrDataValue |<< 4) | data.resize(32)
            } otherwise { // NFIX
              instrDataValue := ~((instrDataValue |<< 4) | data.resize(32))
            }
            prefixActive := True
            instrStream.ready := True
            groupFlow.valid := False
          } otherwise {
            // Simple pass-through for now
            groupFlow.valid := True
            groupFlow.payload.instructions(0) := currentInstr.asBits
            groupFlow.payload.count := 1
            for (i <- 1 until 8) {
              groupFlow.payload.instructions(i) := 0
            }
            instrStream.ready := groupFlow.ready
            prefixActive := False
            instrDataValue := 0
          }
        } otherwise {
          groupFlow.valid := False
          instrStream.ready := False
        }
      }
    }
  }
}
