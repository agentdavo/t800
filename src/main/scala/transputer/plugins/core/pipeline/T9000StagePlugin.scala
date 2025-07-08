package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.lib.bus.bmb._
import transputer.{Global, Opcode}
import transputer.plugins.core.regstack.{RegStackService, RegName}
import transputer.plugins.core.cache.{WorkspaceCacheService, MainCacheService, CacheCmd}

/** T9000 Stage Assignment Plugin
  *
  * This plugin properly assigns instructions to their correct pipeline stages according to the
  * T9000 architecture:
  *
  * Stage 1 (Fetch): Instruction fetch + LDL (local variable loads) - workspace cache Stage 2
  * (Decode): Address calculations (WSUB, ADD for addressing) Stage 3 (Execute): LDNL (non-local
  * loads) - main cache access Stage 4 (Memory): ALU/FPU operations Stage 5 (WriteBack): STL/STNL
  * (stores) and CJ/J (branches) - cache writeback
  */
class T9000StagePlugin extends FiberPlugin {
  override def getDisplayName(): String = "T9000StagePlugin"
  setName("t9000Stage")

  // Service interface for stage-specific operations
  trait T9000StageService {
    def isLocalLoad(opcode: Bits): Bool
    def isAddressCalc(opcode: Bits): Bool
    def isNonLocalLoad(opcode: Bits): Bool
    def isAluFpuOp(opcode: Bits): Bool
    def isStoreOrBranch(opcode: Bits): Bool
  }

  during setup new Area {
    println(s"[${T9000StagePlugin.this.getDisplayName()}] setup start")

    addService(new T9000StageService {
      override def isLocalLoad(opcode: Bits): Bool = checkLocalLoad(opcode)
      override def isAddressCalc(opcode: Bits): Bool = checkAddressCalc(opcode)
      override def isNonLocalLoad(opcode: Bits): Bool = checkNonLocalLoad(opcode)
      override def isAluFpuOp(opcode: Bits): Bool = checkAluFpuOp(opcode)
      override def isStoreOrBranch(opcode: Bits): Bool = checkStoreOrBranch(opcode)
    })

    println(s"[${T9000StagePlugin.this.getDisplayName()}] setup end")
  }

  // Stage check functions
  var checkLocalLoad: Bits => Bool = null
  var checkAddressCalc: Bits => Bool = null
  var checkNonLocalLoad: Bits => Bool = null
  var checkAluFpuOp: Bits => Bool = null
  var checkStoreOrBranch: Bits => Bool = null

  during build new Area {
    println(s"[${T9000StagePlugin.this.getDisplayName()}] build start")

    // Get pipeline stages
    val pipe = host[PipelineStageService]
    val regStack = host[RegStackService]
    val wsCache = host.get[WorkspaceCacheService]
    val mainCache = host.get[MainCacheService]

    // Stage 1 (Fetch): Local loads with workspace cache access
    val stage1 = new Area {
      // LDL operations access workspace cache
      val isLdl =
        pipe.fetch(Global.OPCODE)(7 downto 4) === Opcode.PrimaryOpcode.LDL.asBits.resize(4)

      when(isLdl) {
        val offset = pipe.fetch(Global.OPCODE)(3 downto 0).asUInt
        val workspace = regStack.readReg(RegName.WdescReg)
        val addr = workspace + (offset << 2) // Word addressing

        // Access workspace cache if available
        if (wsCache.isDefined) {
          val data = wsCache.get.readA(addr)
          // Result will be available in next stage
          pipe.fetch.down(Global.MEM_DATA) := data
        } else {
          // Fallback without workspace cache
          pipe.fetch.down(Global.MEM_DATA) := B(0, 32 bits)
        }
      }
    }

    // Stage 2 (Decode): Address calculations
    val stage2 = new Area {
      val opcode = pipe.decode(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      when(isOpr) {
        // WSUB - workspace subtract for address calculation
        when(oprFunc === Opcode.SecondaryOpcode.WSUB.asBits.resize(4)) {
          val workspace = regStack.readReg(RegName.WdescReg)
          val areg = regStack.readReg(RegName.Areg)
          val result = workspace - areg
          regStack.writeReg(RegName.Areg, result)
        }

        // ADD used for address calculation
        when(oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4)) {
          val areg = regStack.readReg(RegName.Areg)
          val breg = regStack.readReg(RegName.Breg)
          val creg = regStack.readReg(RegName.Creg)
          val result = breg + areg
          regStack.writeReg(RegName.Areg, result)
          regStack.writeReg(RegName.Breg, creg)
        }
      }
    }

    // Stage 3 (Execute): Non-local loads with main cache access
    val stage3 = new Area {
      val isLdnl =
        pipe.execute(Global.OPCODE)(7 downto 4) === Opcode.PrimaryOpcode.LDNL.asBits.resize(4)

      when(isLdnl) {
        val offset = pipe.execute(Global.OPCODE)(3 downto 0).asUInt
        val areg = regStack.readReg(RegName.Areg)
        val addr = areg + (offset << 2)

        // Access main cache through Load A port if available
        if (mainCache.isDefined) {
          val cmd = CacheCmd()
          cmd.address := addr
          cmd.write := False
          cmd.byteEnable := B"1111"
          cmd.writeData := B(0, 32 bits)

          mainCache.get.cpuLoadA.valid := True
          mainCache.get.cpuLoadA.payload := cmd
        }

        // Result propagates to next stage
        pipe.execute.down(Global.MEM_DATA) := B(0, 32 bits) // Placeholder - actual data from cache
      }
    }

    // Stage 4 (Memory): ALU/FPU operations
    val stage4 = new Area {
      val opcode = pipe.memory(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      when(isOpr) {
        // Simple ALU operations implementation
        // TODO: Connect to actual AluPlugin when service interface is properly exposed
        when(oprFunc === Opcode.SecondaryOpcode.REV.asBits.resize(4)) {
          val areg = regStack.readReg(RegName.Areg)
          val breg = regStack.readReg(RegName.Breg)
          regStack.writeReg(RegName.Areg, breg)
          regStack.writeReg(RegName.Breg, areg)
        }

        when(oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4)) {
          val areg = regStack.readReg(RegName.Areg)
          val breg = regStack.readReg(RegName.Breg)
          val creg = regStack.readReg(RegName.Creg)
          val result = breg + areg
          regStack.writeReg(RegName.Areg, result)
          regStack.writeReg(RegName.Breg, creg)
        }

        when(oprFunc === Opcode.SecondaryOpcode.SUB.asBits.resize(4)) {
          val areg = regStack.readReg(RegName.Areg)
          val breg = regStack.readReg(RegName.Breg)
          val creg = regStack.readReg(RegName.Creg)
          val result = breg - areg
          regStack.writeReg(RegName.Areg, result)
          regStack.writeReg(RegName.Breg, creg)
        }
      }
    }

    // Stage 5 (WriteBack): Stores and branches
    val stage5 = new Area {
      val opcode = pipe.writeBack(Global.OPCODE)
      val primaryOp = opcode(7 downto 4)

      // STL - Store local
      when(primaryOp === Opcode.PrimaryOpcode.STL.asBits.resize(4)) {
        val offset = opcode(3 downto 0).asUInt
        val workspace = regStack.readReg(RegName.WdescReg)
        val addr = workspace + (offset << 2)
        val areg = regStack.readReg(RegName.Areg)

        // Write to workspace cache if available
        if (wsCache.isDefined) {
          wsCache.get.write(addr, areg.asBits)
        }

        // Pop stack
        val breg = regStack.readReg(RegName.Breg)
        val creg = regStack.readReg(RegName.Creg)
        regStack.writeReg(RegName.Areg, breg)
        regStack.writeReg(RegName.Breg, creg)
      }

      // STNL - Store non-local
      when(primaryOp === Opcode.PrimaryOpcode.STNL.asBits.resize(4)) {
        val offset = opcode(3 downto 0).asUInt
        val areg = regStack.readReg(RegName.Areg)
        val addr = areg + (offset << 2)
        val breg = regStack.readReg(RegName.Breg)

        // Write through main cache Store port if available
        if (mainCache.isDefined) {
          val cmd = CacheCmd()
          cmd.address := addr
          cmd.write := True
          cmd.writeData := breg.asBits
          cmd.byteEnable := B"1111"

          mainCache.get.cpuStore.valid := True
          mainCache.get.cpuStore.payload := cmd
        }

        // Pop stack twice
        val creg = regStack.readReg(RegName.Creg)
        regStack.writeReg(RegName.Areg, creg)
      }

      // CJ - Conditional jump
      when(primaryOp === Opcode.PrimaryOpcode.CJ.asBits.resize(4)) {
        val offset = opcode(3 downto 0).asSInt
        val areg = regStack.readReg(RegName.Areg)
        when(areg === 0) {
          pipe.writeBack(Global.IPTR) := (pipe.writeBack(Global.IPTR).asSInt + offset.resize(
            32
          )).asUInt
        }
        val breg = regStack.readReg(RegName.Breg)
        val creg = regStack.readReg(RegName.Creg)
        regStack.writeReg(RegName.Areg, breg)
        regStack.writeReg(RegName.Breg, creg)
      }

      // J - Jump
      when(primaryOp === Opcode.PrimaryOpcode.J.asBits.resize(4)) {
        val offset = opcode(3 downto 0).asSInt
        pipe
          .writeBack(Global.IPTR) := (pipe.writeBack(Global.IPTR).asSInt + offset.resize(32)).asUInt
      }
    }

    // Implement stage check functions
    checkLocalLoad = (opcode: Bits) => {
      opcode(7 downto 4) === Opcode.PrimaryOpcode.LDL.asBits.resize(4)
    }

    checkAddressCalc = (opcode: Bits) => {
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val func = opcode(3 downto 0)
      isOpr && (func === Opcode.SecondaryOpcode.WSUB.asBits.resize(4) ||
        func === Opcode.SecondaryOpcode.ADD.asBits.resize(4))
    }

    checkNonLocalLoad = (opcode: Bits) => {
      opcode(7 downto 4) === Opcode.PrimaryOpcode.LDNL.asBits.resize(4)
    }

    checkAluFpuOp = (opcode: Bits) => {
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val func = opcode(3 downto 0)

      // Check if it's an ALU or FPU operation (excluding address calc)
      isOpr && func =/= Opcode.SecondaryOpcode.WSUB.asBits.resize(4) &&
      func =/= Opcode.SecondaryOpcode.ADD.asBits.resize(4)
    }

    checkStoreOrBranch = (opcode: Bits) => {
      val primaryOp = opcode(7 downto 4)
      primaryOp === Opcode.PrimaryOpcode.STL.asBits.resize(4) ||
      primaryOp === Opcode.PrimaryOpcode.STNL.asBits.resize(4) ||
      primaryOp === Opcode.PrimaryOpcode.CJ.asBits.resize(4) ||
      primaryOp === Opcode.PrimaryOpcode.J.asBits.resize(4)
    }

    println(s"[${T9000StagePlugin.this.getDisplayName()}] Stage assignment configured")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] - Stage 1: Fetch + LDL (workspace cache)")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] - Stage 2: Address calculations")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] - Stage 3: LDNL (main cache)")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] - Stage 4: ALU/FPU operations")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] - Stage 5: Stores and branches")
    println(s"[${T9000StagePlugin.this.getDisplayName()}] build end")
  }
}
