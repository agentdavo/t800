package transputer.plugins.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbDownSizerBridge, BmbUnburstify}
import transputer.{Global, Opcode, Transputer}
import transputer.plugins.registers.RegfileSrv
import transputer.plugins.Fetch
import transputer.plugins.grouper.GroupedInstrSrv
import transputer.plugins.SystemBusSrv
import transputer.plugins.registers.RegName
import transputer.plugins.pipeline.{PipelineSrv, PipelineStageSrv}

/** Implements primary instruction decoding and execution, receiving opcodes from FetchPlugin or
  * GrouperPlugin, and accessing 128-bit system bus via BMB.
  */
class PrimaryInstrPlugin extends FiberPlugin with PipelineSrv {
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
    val regfile = Plugin[RegfileSrv]
    val pipe = Plugin[PipelineStageSrv]
    val systemBus = Plugin[SystemBusSrv].bus // 128-bit BMB system bus
    val grouper =
      try Plugin[GroupedInstrSrv]
      catch { case _: Exception => null } // Optional GrouperPlugin

    // Define 32-bit BMB parameters for memory access
    val memParam = BmbParameter(
      addressWidth = Global.ADDR_BITS,
      dataWidth = 32,
      sourceWidth = 1,
      contextWidth = 0,
      lengthWidth = 0
    )

    // BMB pipeline for memory operations
    val memBmb = Bmb(memParam)
    val unburstify = BmbUnburstify(memParam)
    val downSizer = BmbDownSizerBridge(
      inputParameter = Transputer.systemBusParam,
      outputParameter = memParam
    )
    memBmb >> unburstify.io.input
    unburstify.io.output >> downSizer.io.input
    downSizer.io.output >> systemBus

    // Select input: grouped or single opcode
    val inst = if (grouper != null) {
      grouper.groups.payload.instructions(0)
    } else {
      pipe.execute(Global.OPCODE)
    }
    val groupCount = if (grouper != null) grouper.groups.payload.count else U(1)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = Reg(UInt(32 bits)) init 0 // Temporary operand accumulator
    val primary = Opcode.PrimaryOpcode()
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
      is(Opcode.PrimaryOpcode.PFIX) {
        accumulated := (accumulated | nibble.resized) << 4
      }
      is(Opcode.PrimaryOpcode.NFIX) {
        accumulated := ((~accumulated) | nibble.resized) << 4
      }
      is(Opcode.PrimaryOpcode.LDC) {
        val operand = accumulated.asSInt
        regfile.write(RegName.Creg, regfile.read(RegName.Breg, 0), 0, shadow = false)
        regfile.write(RegName.Breg, regfile.read(RegName.Areg, 0), 0, shadow = false)
        regfile.write(RegName.Areg, operand.asUInt, 0, shadow = false)
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDL) {
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
      is(Opcode.PrimaryOpcode.STL) {
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
      is(Opcode.PrimaryOpcode.LDLP) {
        val operand = accumulated
        regfile.write(RegName.Creg, regfile.read(RegName.Breg, 0), 0, shadow = false)
        regfile.write(RegName.Breg, regfile.read(RegName.Areg, 0), 0, shadow = false)
        regfile.write(
          RegName.Areg,
          regfile.read(RegName.WdescReg, 0).asUInt + operand,
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDNLP) {
        regfile.write(
          RegName.Areg,
          regfile.read(RegName.Areg, 0).asUInt + (accumulated |<< 2),
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.ADC) {
        val operand = accumulated
        regfile.write(
          RegName.Areg,
          regfile.read(RegName.Areg, 0).asUInt + operand,
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.EQC) {
        val operand = accumulated
        regfile.write(
          RegName.Areg,
          (regfile.read(RegName.Areg, 0) === operand).asUInt.resized,
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.J) {
        val operand = accumulated.asSInt
        regfile.write(
          RegName.IptrReg,
          (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt,
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.CJ) {
        val operand = accumulated.asSInt
        when(regfile.read(RegName.Areg, 0) === 0) {
          regfile.write(
            RegName.IptrReg,
            (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt,
            0,
            shadow = false
          )
        } otherwise {
          regfile.write(RegName.Areg, regfile.read(RegName.Breg, 0), 0, shadow = false)
          regfile.write(RegName.Breg, regfile.read(RegName.Creg, 0), 0, shadow = false)
        }
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDNL) {
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
      is(Opcode.PrimaryOpcode.STNL) {
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
      is(Opcode.PrimaryOpcode.CALL) {
        val operand = accumulated.asSInt
        val addr = regfile.read(RegName.WdescReg, 0).asUInt + S(-4)
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := (regfile.read(RegName.IptrReg, 0) + 1).asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regfile.write(
            RegName.WdescReg,
            (regfile.read(RegName.WdescReg, 0).asSInt - 4).asUInt,
            0,
            shadow = false
          )
          regfile.write(RegName.Areg, regfile.read(RegName.IptrReg, 0) + 1, 0, shadow = false)
          regfile.write(
            RegName.IptrReg,
            (regfile.read(RegName.IptrReg, 0).asSInt + operand).asUInt,
            0,
            shadow = false
          )
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.AJW) {
        val operand = accumulated.asSInt
        regfile.write(
          RegName.WdescReg,
          (regfile.read(RegName.WdescReg, 0).asSInt + operand).asUInt,
          0,
          shadow = false
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.OPR) {
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
}
