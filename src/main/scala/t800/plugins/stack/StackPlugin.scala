package t800.plugins.stack

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbDownSizerBridge, BmbUnburstify}
import t800.{Global, T800}
import t800.plugins.{SystemBusSrv, RegfileService}
import t800.plugins.registers.RegName

/** Manages workspace memory access for local variable operations (LDL, STL, CALL) in the T800 pipeline. */

class StackPlugin extends FiberPlugin {
  val version = "StackPlugin v0.2"
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
    implicit val h: PluginHost = host
    val regfile = Plugin[RegfileService]
    val systemBus = Plugin[SystemBusSrv].bus // 128-bit BMB system bus
    val workspaceCache = try Plugin[WorkspaceCacheSrv] catch { case _: Exception => null } // Optional WorkspaceCachePlugin

    // Define 32-bit BMB parameters for workspace access
    val memParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 32,
        lengthWidth = 0, // Single-beat transactions
        sourceWidth = 1,
        contextWidth = 0
      )
    )

    // Workspace memory: fallback Mem if no WorkspaceCachePlugin
    val workspace = workspaceCache == null generate Mem(UInt(Global.WORD_BITS bits), Global.RAM_WORDS)
    val memBmb = workspaceCache != null generate Bmb(memParam)

    // Connect to WorkspaceCachePlugin or system bus
    if (workspaceCache != null) {
      val unburstify = BmbUnburstify(memParam)
      val downSizer = BmbDownSizerBridge(
        inputParameter = T800.systemBusParam,
        outputParameter = memParam
      )
      memBmb >> unburstify.io.input
      unburstify.io.output >> downSizer.io.input
      downSizer.io.output >> systemBus
    }

    addService(new StackSrv {
      override def read(offset: SInt): UInt = {
        val addr = (regfile.read(RegName.WdescReg, 0).asSInt + offset).asUInt.resize(log2Up(Global.RAM_WORDS) bits)
        if (workspaceCache != null) {
          memBmb.cmd.valid := True
          memBmb.cmd.opcode := 0 // Read
          memBmb.cmd.address := addr
          memBmb.cmd.length := 0
          memBmb.cmd.source := 0
          memBmb.cmd.context := 0
          memBmb.rsp.valid ? memBmb.rsp.data.asUInt | U(0)
        } else {
          workspace.readSync(addr)
        }
      }

      override def write(offset: SInt, data: UInt): Unit = {
        val addr = (regfile.read(RegName.WdescReg, 0).asSInt + offset).asUInt.resize(log2Up(Global.RAM_WORDS) bits)
        if (workspaceCache != null) {
          memBmb.cmd.valid := True
          memBmb.cmd.opcode := 1 // Write
          memBmb.cmd.address := addr
          memBmb.cmd.length := 0
          memBmb.cmd.data := data.asBits
          memBmb.cmd.mask := B"1111"
          memBmb.cmd.source := 0
          memBmb.cmd.context := 0
        } else {
          workspace.write(addr, data)
        }
      }
    })

    println(s"[${this.getDisplayName()}] build end")
  }
}

trait WorkspaceCacheSrv {
  def bus: Bmb // BMB interface for workspace cache
}
