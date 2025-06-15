package t800.plugins.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbDownSizerBridge, BmbUnburstify}
import t800.{Global, Opcodes, T800}
import t800.plugins.{RegfileService, Fetch, GrouperPlugin, GroupedInstrSrv}
import t800.plugins.registers.RegName
import t800.plugins.pipeline.{PipelineService, PipelineSrv}

/** Implements primary instruction decoding and execution, receiving opcodes from FetchPlugin or GrouperPlugin, and accessing 128-bit system bus via BMB. */
class PrimaryInstrPlugin extends FiberPlugin with PipelineService {
  val version = "PrimaryInstrPlugin v0.6"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    val regfile = Plugin[RegfileService]
    val pipe = Plugin[PipelineSrv]
    val systemBus = Plugin[SystemBusSrv].bus // 128-bit BMB system bus
    val grouper = try Plugin[GroupedInstrSrv] catch { case _: Exception => null } // Optional GrouperPlugin

    // Define 32-bit BMB parameters for memory access
    val memParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 32,
        lengthWidth = 0, // Single-beat transactions
        sourceWidth = 1,
        contextWidth = 0
      )
    )

    // BMB pipeline for memory operations
    val memBmb = Bmb(memParam)
    val unburstify = BmbUnburstify(memParam)
    val downSizer = BmbDownSizerBridge(
      inputParameter = T800.systemBusParam,
      outputParameter = memParam
    )
    memBmb >> unburstify.io.input
    unburstify.io.output >> downSizer.io.input
    downSizer.io.output >> systemBus

    // Select input: grouped or single opcode
    val inst = if (grouper != null) {
      pipe.execute(GrouperPlugin.GROUP_INSTR)(0) // First opcode from group
    } else {
      pipe.execute(Global.OPCODE) // Single opcode from FetchPlugin
    }
    val groupCount = if (grouper != null) pipe.execute(GrouperPlugin.GROUP_COUNT) else U(1)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = Reg(UInt(32 bits)) init 0 // Temporary operand accumulator
    val primary = Opcodes.PrimaryEnum()
    primary.assignFromBits(inst(7 downto 4))

    // Memory command/response handling
    memBmb.cmd.valid := False
    memBmb.cmd.opcode := 0 // Read by default
    memBmb.cmd.address := U(0)
    memBmb.cmd.length := 0 // Single-beat
    memBmb.cmd.data := B(0, 32 bits)
    memBmb.cmd.mask := B"1111" // Full 32-bit write
    memBmb.cmd.source := 0
    memBmb.cmd.context := 0

    // Instruction execution (single-issue for now, group processing TBD)
    switch(primary) {
      is(Opcodes.PrimaryEnum.PFIX) {
        accumulated := (accumulated | nibble.resized) << 4
      }
      is(Opcodes.PrimaryEnum.NFIX) {
        accumulated := ((~accumulated) | nibble.resized) << 4
      }
      is(Opcodes.PrimaryEnum.LDC) {
        val operand = accumulated.asSInt
        regfile.write(RegName.Creg, regfile.read(RegName.Breg, 0), 0, shadow = false)
        regfile.write(RegName.Breg, regfile.read(RegName.Areg, 0), 0, shadow = false)
        regfile.write(RegName.Areg, operand.asUInt, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.LDL) {
        val operand = accumulated.asSInt
        val addr = regfile.read(RegName.WdescReg, 0).asUInt + operand
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 0 // Read
        memBmb.cmd.address := addr.resized
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(RegName.Creg, regfile.read(RegName.Breg, 0), 0, shadow = false)
          regfile.write(RegName.Breg, regfile.read(RegName.Areg, 0), 0, shadow = false)
          regfile.write(RegName.Areg, memBmb.rsp.data.asUInt, 0, shadow = false)
          accumulated := 0
        }
      }
      is(Opcodes.PrimaryEnum.STL) {
        val operand = accumulated.asSInt
        val addr = regfile.read(RegName.WdescReg, 0).asUInt + operand
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := regfile.read(RegName.Areg, 0).asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(RegName.Areg, regfile.read(RegName.Breg, 0), 0, shadow = false)
          regfile.write(RegName.Breg, regfile.read(RegName.Creg, 0), 0, shadow = false)
          accumulated := 0
        }
      }
      is(Opcodes.PrimaryEnum.LDLP) {
        val operand = accumulated
        regfile.write(RegName.Creg, regfile.read(RegName.Breg, 0), 0, shadow = false)
        regfile.write(RegName.Breg, regfile.read(RegName.Areg, 0), 0, shadow = false)
        regfile.write(RegName.Areg, regfile.read(RegName.WdescReg, 0).asUInt + operand, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.LDNLP) {
        regfile.write(RegName.Areg, regfile.read(RegName.Areg, 0).asUInt + (accumulated |<< 2), 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.ADC) {
        val operand = accumulated
        regfile.write(RegName.Areg, regfile.read(RegName.Areg, 0).asUInt + operand, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.EQC) {
        val operand = accumulated
        regfile.write(RegName.Areg, (regfile.read(RegName.Areg, 0) === operand).asUInt.resized, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.J) {
        val operand = accumulated.asSInt
        regfile.write(RegName.IptrReg, (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.CJ) {
        val operand = accumulated.asSInt
        when(regfile.read(RegName.Areg, 0) === 0) {
          regfile.write(RegName.IptrReg, (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt, 0, shadow = false)
        } otherwise {
          regfile.write(RegName.Areg, regfile.read(RegName.Breg, 0), 0, shadow = false)
          regfile.write(RegName.Breg, regfile.read(RegName.Creg, 0), 0, shadow = false)
        }
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.LDNL) {
        val addr = regfile.read(RegName.Areg, 0).asUInt + accumulated
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 0 // Read
        memBmb.cmd.address := addr.resized
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(RegName.Areg, memBmb.rsp.data.asUInt, 0, shadow = false)
          accumulated := 0
        }
      }
      is(Opcodes.PrimaryEnum.STNL) {
        val addr = regfile.read(RegName.Areg, 0).asUInt + accumulated
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := regfile.read(RegName.Breg, 0).asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(RegName.Areg, regfile.read(RegName.Creg, 0), 0, shadow = false)
          accumulated := 0
        }
      }
      is(Opcodes.PrimaryEnum.CALL) {
        val operand = accumulated.asSInt
        val addr = regfile.read(RegName.WdescReg, 0).asUInt + S(-4)
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := (regfile.read(RegName.IptrReg, 0) + 1).asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(RegName.WdescReg, (regfile.read(RegName.WdescReg, 0).asSInt - 4).asUInt, 0, shadow = false)
          regfile.write(RegName.Areg, regfile.read(RegName.IptrReg, 0) + 1, 0, shadow = false)
          regfile.write(RegName.IptrReg, (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt, 0, shadow = false)
          accumulated := 0
        }
      }
      is(Opcodes.PrimaryEnum.AJW) {
        val operand = accumulated.asSInt
        regfile.write(RegName.WdescReg, (regfile.read(RegName.WdescReg, 0).asSInt + operand).asUInt, 0, shadow = false)
        accumulated := 0
      }
      is(Opcodes.PrimaryEnum.OPR) {
        // Defer to SecondaryInstrPlugin
        accumulated := 0
      }
      default {
        accumulated := 0
      }
    }

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()

  trait SystemBusSrv {
    def bus: Bmb
  }
}