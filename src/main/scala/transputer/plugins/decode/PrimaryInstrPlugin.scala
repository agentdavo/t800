package transputer.plugins.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbDownSizerBridge, BmbUpSizerBridge, BmbUnburstify}
import transputer.Global
import transputer.{Opcode, Transputer}
import transputer.plugins.regstack.{RegStackService, RegName}
import transputer.plugins.fetch.InstrFetchService
import transputer.plugins.grouper.GroupedInstrService
import transputer.plugins.SystemBusService
import transputer.plugins.bus.BusMasterService
import transputer.plugins.pipeline.{PipelineService, PipelineStageService}

/** Implements primary instruction decoding and execution, receiving opcodes from FetchPlugin or
  * GrouperPlugin, and accessing 128-bit system bus via BMB.
  */
class PrimaryInstrPlugin extends FiberPlugin with PipelineService {
  val version = "PrimaryInstrPlugin v0.6"

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    val regStack = Plugin[RegStackService]
    val pipe = Plugin[PipelineStageService]
    val systemBus = Plugin[SystemBusService].bus // 128-bit BMB system bus
    val grouper =
      try Plugin[GroupedInstrService]
      catch { case _: Exception => null } // Optional GrouperPlugin

    // Define 32-bit BMB parameters for memory access
    import spinal.lib.bus.bmb.BmbSourceParameter
    
    // Create memory bus parameters for 32-bit instruction access
    val memParam = BmbParameter(
      addressWidth = Global.ADDR_BITS,
      dataWidth = 32,       // 32-bit data for instructions
      sourceWidth = 1,      // Single source for decode stage  
      contextWidth = 0,     // Base context width (will be extended by upsizer)
      lengthWidth = 2       // Support up to 4-byte transfers
    )

    // Create dedicated decode bus and upsize to system bus
    val memBmb = Bmb(memParam)
    
    // Default assignments to prevent undriven signals
    memBmb.cmd.valid := False
    memBmb.cmd.opcode := 0
    memBmb.cmd.address := 0
    memBmb.cmd.length := 0
    memBmb.cmd.data := 0
    memBmb.cmd.mask := 0
    memBmb.cmd.last := True
    memBmb.cmd.source := 0
    memBmb.cmd.context := 0
    memBmb.rsp.ready := True
    val outputParam = BmbUpSizerBridge.outputParameterFrom(
      memParam.access,
      systemBus.p.access.dataWidth
    )
    val upSizer = BmbUpSizerBridge(
      inputParameter = memParam,
      outputParameter = memParam.copy(access = outputParam)
    )
    memBmb >> upSizer.io.input
    
    // Register with bus master service for arbitration
    val busMasterService = host.get[BusMasterService]
    if (busMasterService.isDefined) {
      busMasterService.get.addMaster("primaryInstr", upSizer.io.output)
    } else {
      // Fallback: temporarily disable to diagnose issue
      // upSizer.io.output >> systemBus
      upSizer.io.output.cmd.ready := True
      upSizer.io.output.rsp.valid := False
      upSizer.io.output.rsp.payload.data := 0
      upSizer.io.output.rsp.payload.last := True
      upSizer.io.output.rsp.payload.source := 0
    }

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

    // Memory command/response handling - 32-bit decode bus
    // Note: Default assignments already done above

    // Instruction execution (single-issue for now, group processing TBD)
    switch(primary) {
      is(Opcode.PrimaryOpcode.PFIX) {
        accumulated := ((accumulated | nibble.resized) << 4).resize(32)
      }
      is(Opcode.PrimaryOpcode.NFIX) {
        accumulated := (((~accumulated) | nibble.resized) << 4).resize(32)
      }
      is(Opcode.PrimaryOpcode.LDC) {
        val operand = accumulated.asSInt
        regStack.stackPush(operand.asUInt)
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDL) {
        val operand = accumulated.asSInt
        val addr = regStack.readReg(RegName.WdescReg) + operand.asUInt
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 0 // Read
        memBmb.cmd.address := addr.resized
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regStack.write(
            RegName.Creg,
            regStack.readReg(RegName.Breg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
          regStack.write(
            RegName.Breg,
            regStack.readReg(RegName.Areg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
          regStack.write(RegName.Areg, memBmb.rsp.data, U((1L << 32) - 1, 32 bits))
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.STL) {
        val operand = accumulated.asSInt
        val addr = regStack.readReg(RegName.WdescReg) + operand.asUInt
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := regStack.readReg(RegName.Areg).asBits.asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regStack.write(
            RegName.Areg,
            regStack.readReg(RegName.Breg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
          regStack.write(
            RegName.Breg,
            regStack.readReg(RegName.Creg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.LDLP) {
        val operand = accumulated
        regStack.write(
          RegName.Creg,
          regStack.readReg(RegName.Breg).asBits,
          U((1L << 32) - 1, 32 bits)
        )
        regStack.write(
          RegName.Breg,
          regStack.readReg(RegName.Areg).asBits,
          U((1L << 32) - 1, 32 bits)
        )
        regStack.write(
          RegName.Areg,
          (regStack.readReg(RegName.WdescReg) + operand).asBits,
          U((1L << 32) - 1, 32 bits)
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDNLP) {
        regStack.write(
          RegName.Areg,
          (regStack.readReg(RegName.Areg) + (accumulated |<< 2)).asBits,
          U((1L << 32) - 1, 32 bits)
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.ADC) {
        val operand = accumulated
        regStack.addToReg(RegName.Areg, operand)
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.EQC) {
        val operand = accumulated
        regStack.compareReg(RegName.Areg, operand)
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.J) {
        val operand = accumulated.asSInt
        regStack.addToRegSigned(RegName.IptrReg, operand)
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.CJ) {
        val operand = accumulated.asSInt
        when(regStack.readReg(RegName.Areg).asBits === 0) {
          regStack.write(
            RegName.IptrReg,
            (regStack.read(RegName.IptrReg, U(0)).asSInt + operand).asBits,
            U(0)
          )
        } otherwise {
          regStack.write(
            RegName.Areg,
            regStack.readReg(RegName.Breg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
          regStack.write(
            RegName.Breg,
            regStack.readReg(RegName.Creg).asBits,
            U((1L << 32) - 1, 32 bits)
          )
        }
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.LDNL) {
        val addr = regStack.readReg(RegName.Areg) + accumulated
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 0 // Read
        memBmb.cmd.address := addr.resized
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regStack.write(RegName.Areg, memBmb.rsp.data, U((1L << 32) - 1, 32 bits))
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.STNL) {
        val addr = regStack.readReg(RegName.Areg) + accumulated
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := regStack.readReg(RegName.Breg).asBits.asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regStack.write(RegName.Areg, regStack.readReg(RegName.Creg).asBits, 0)
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.CALL) {
        val operand = accumulated.asSInt
        val addr = (regStack.readReg(RegName.WdescReg).asSInt + S(-4)).asUInt
        memBmb.cmd.valid := True
        memBmb.cmd.opcode := 1 // Write
        memBmb.cmd.address := addr.resized
        memBmb.cmd.data := (regStack.readReg(RegName.IptrReg) + 1).asBits
        pipe.execute.haltWhen(!memBmb.rsp.valid)
        when(pipe.execute.down.isFiring) {
          regStack.write(
            RegName.WdescReg,
            (regStack.read(RegName.WdescReg, U(0)).asSInt - 4).asBits,
            U(0)
          )
          regStack.write(
            RegName.Areg,
            (regStack.read(RegName.IptrReg, U(0)).asUInt + 1).asBits,
            U(0)
          )
          regStack.write(
            RegName.IptrReg,
            (regStack.read(RegName.IptrReg, U(0)).asSInt + operand).asBits,
            U(0)
          )
          accumulated := 0
        }
      }
      is(Opcode.PrimaryOpcode.AJW) {
        val operand = accumulated.asSInt
        regStack.write(
          RegName.WdescReg,
          (regStack.read(RegName.WdescReg, U((1L << 32) - 1, 32 bits)).asSInt + operand).asBits,
          U(0)
        )
        accumulated := 0
      }
      is(Opcode.PrimaryOpcode.OPR) {
        // Defer to SecondaryInstrPlugin
        accumulated := 0
      }
    }

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()
}
