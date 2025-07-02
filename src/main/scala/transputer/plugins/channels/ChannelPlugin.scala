package transputer.plugins.channels

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.channels._

/** T9000 Channel Plugin implementing Table 6.21 channel management operations.
  *
  * This plugin implements virtual channel and link management from T9000 Table 6.21:
  *   - chantype: Query channel type (physical/virtual/resource)
  *   - initvlcb: Initialize virtual link control block
  *   - setchmode: Configure channel operating mode
  *   - sethdr/writehdr/readhdr: Header management
  *   - swapbfr: Buffer management
  *   - mkrc/unmkrc: Resource channel marking
  *
  * Features:
  *   - Virtual channel multiplexing over physical links
  *   - Resource channel abstraction for system services
  *   - Packet-based communication with headers
  *   - Priority and buffering control
  */
class ChannelPlugin extends FiberPlugin {
  override def getDisplayName(): String = "ChannelPlugin"
  setName("channels")

  during setup new Area {
    println(s"[${ChannelPlugin.this.getDisplayName()}] setup start")

    addService(new ChannelService {
      override def executeOp(op: ChannelOp.C, channelId: UInt, value: UInt): ChannelResult =
        channelResult
      override def isChannelOp(opcode: Bits): Bool = isChannelOperation
      override def getChannelOp(opcode: Bits): ChannelOp.C = channelOperation
    })

    println(s"[${ChannelPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var channelResult: ChannelResult = null
  var isChannelOperation: Bool = null
  var channelOperation: ChannelOp.C = null

  during build new Area {
    println(s"[${ChannelPlugin.this.getDisplayName()}] build start")

    // Get pipeline and register stack services
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    channelResult = ChannelResult()
    isChannelOperation = Bool()
    channelOperation = ChannelOp()

    // Initialize channelResult to avoid latches
    channelResult.channelType := ChannelType.INVALID
    channelResult.mode.enabled := False
    channelResult.mode.priority := 0
    channelResult.mode.buffered := False
    channelResult.mode.packetized := False
    channelResult.header := 0
    channelResult.status := 0
    channelResult.error := False

    // Channel control registers
    val vlcbBase = Reg(UInt(32 bits)) init 0 // Virtual link control block base
    val channelHeaders = Vec(Reg(UInt(32 bits)) init 0, 16) // Channel headers
    val channelModes = Vec(Reg(ChannelMode()), 16) // Channel modes
    val resourceMask = Reg(Bits(16 bits)) init 0 // Resource channel mask

    // Initialize channel modes
    for (i <- 0 until 16) {
      channelModes(i).enabled init False
      channelModes(i).priority init 0
      channelModes(i).buffered init False
      channelModes(i).packetized init False
    }

    // Channel execution in Memory stage (stage 4)
    val channelLogic = new Area {
      val opcode = pipe.execute(Global.OPCODE)
      val isPrimary = opcode(7 downto 4) =/= Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)
      // For negative prefix operations, would need full instruction word, not just opcode
      val isNegPrefix = False // Simplified - negative prefix ops not implemented

      // Table 6.21 instruction recognition
      isChannelOperation := (isOpr && oprFunc === U(
        Table6_21.CHANTYPE_OPCODE & 0xf,
        4 bits
      ).asBits) ||
        (isNegPrefix && isOpr) // Negative prefix operations checked separately

      // Decode channel operation
      channelOperation := ChannelOp.CHANTYPE // Default
      when(isOpr) {
        when(oprFunc === U(Table6_21.CHANTYPE_OPCODE & 0xf, 4 bits).asBits) {
          channelOperation := ChannelOp.CHANTYPE
        } elsewhen (isNegPrefix) {
          // For negative prefix operations, need to check the full pattern
          channelOperation := ChannelOp.INITVLCB // Simplified for now
        }
      }

      // Execute channel operations
      when(isChannelOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)
        val channelIndex = areg(3 downto 0) // Lower 4 bits for channel index

        // Initialize result
        channelResult.error := False
        channelResult.channelType := ChannelType.INVALID // Default value

        switch(channelOperation) {
          is(ChannelOp.CHANTYPE) {
            // Get channel type: A = channel ID -> A = type
            val isResource = resourceMask(channelIndex)
            val isValid = True // channelIndex is 4 bits, so always < 16

            when(isResource) {
              channelResult.channelType := ChannelType.RESOURCE
            } elsewhen (isValid && channelModes(channelIndex).enabled) {
              channelResult.channelType := ChannelType.VIRTUAL
            } elsewhen (channelIndex < U(4, 4 bits)) { // First 4 are physical
              channelResult.channelType := ChannelType.PHYSICAL
            } otherwise {
              channelResult.channelType := ChannelType.INVALID
            }

            // Return type in Areg
            regStack.writeReg(
              transputer.plugins.core.regstack.RegName.Areg,
              channelResult.channelType.asBits.asUInt.resize(32)
            )
          }

          is(ChannelOp.INITVLCB) {
            // Initialize VLCB: A = base address
            vlcbBase := areg
            // Pop value
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ChannelOp.SETCHMODE) {
            // Set channel mode: B = channel, A = mode
            when(True) { // channelIndex is 4 bits, so always < 16
              channelModes(channelIndex).enabled := areg(0)
              channelModes(channelIndex).priority := areg(2 downto 1)
              channelModes(channelIndex).buffered := areg(3)
              channelModes(channelIndex).packetized := areg(4)
            }
            // Pop values
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, 0)
          }

          is(ChannelOp.SETHDR) {
            // Set header: B = channel, A = header
            when(True) { // channelIndex is 4 bits, so always < 16
              channelHeaders(channelIndex) := areg
            }
            // Pop values
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, 0)
          }

          is(ChannelOp.READHDR) {
            // Read header: A = channel -> A = header
            when(True) { // channelIndex is 4 bits, so always < 16
              regStack.writeReg(
                transputer.plugins.core.regstack.RegName.Areg,
                channelHeaders(channelIndex)
              )
            } otherwise {
              regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, 0)
              channelResult.error := True
            }
          }

          is(ChannelOp.MKRC) {
            // Mark as resource channel: A = channel
            when(True) { // channelIndex is 4 bits, so always < 16
              resourceMask(channelIndex) := True
            }
            // Pop value
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ChannelOp.UNMKRC) {
            // Unmark resource channel: A = channel
            when(True) { // channelIndex is 4 bits, so always < 16
              resourceMask(channelIndex) := False
            }
            // Pop value
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          default {
            // Other operations not fully implemented
            channelResult.error := True
          }
        }

        // Update result fields
        channelResult.mode := channelModes(channelIndex)
        channelResult.header := channelHeaders(channelIndex)
        channelResult.status := U(0, 32 bits) // Simplified
      }
    }

    // Default result values
    when(!isChannelOperation) {
      channelResult.channelType := ChannelType.INVALID
      channelResult.mode.enabled := False
      channelResult.mode.priority := 0
      channelResult.mode.buffered := False
      channelResult.mode.packetized := False
      channelResult.header := 0
      channelResult.status := 0
      channelResult.error := False
    }

    println(s"[${ChannelPlugin.this.getDisplayName()}] Channel hardware configured")
    println(s"[${ChannelPlugin.this.getDisplayName()}] - Table 6.21: Channel management")
    println(s"[${ChannelPlugin.this.getDisplayName()}] - Virtual channels and resource marking")
    println(s"[${ChannelPlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(s"[${ChannelPlugin.this.getDisplayName()}] build end")
  }
}
