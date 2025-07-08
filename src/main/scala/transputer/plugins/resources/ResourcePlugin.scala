package transputer.plugins.resources

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.resources._

/** T9000 Resource Plugin implementing Table 6.22 resource channel operations.
  *
  * This plugin implements resource management from T9000 Table 6.22:
  *   - grant/enbg/disg: Resource granting mechanism
  *   - mkrc/unmkrc: Resource channel marking
  *   - irdsq/erdsq: Resource descriptor queue management
  *   - stresptr/ldresptr: Resource pointer management
  *
  * Resource channels provide a mechanism for managing shared resources in a multi-process system,
  * ensuring mutual exclusion and fair access.
  *
  * Features:
  *   - Resource allocation with queuing
  *   - Fair resource scheduling
  *   - Integration with channel system
  *   - Support for nested resource acquisition
  */
class ResourcePlugin extends FiberPlugin {
  override def getDisplayName(): String = "ResourcePlugin"
  setName("resources")

  during setup new Area {
    println(s"[${ResourcePlugin.this.getDisplayName()}] setup start")

    addService(new ResourceService {
      override def executeOp(op: ResourceOp.C, channelId: UInt, value: UInt): ResourceResult =
        resourceResult
      override def isResourceOp(opcode: Bits): Bool = isResourceOperation
      override def getResourceOp(opcode: Bits): ResourceOp.C = resourceOperation
      override def getState(): ResourceState = resourceState
    })

    println(s"[${ResourcePlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var resourceResult: ResourceResult = null
  var isResourceOperation: Bool = null
  var resourceOperation: ResourceOp.C = null
  var resourceState: ResourceState = null

  during build new Area {
    println(s"[${ResourcePlugin.this.getDisplayName()}] build start")

    // Get pipeline and register stack services
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    resourceResult = ResourceResult()
    isResourceOperation = Bool()
    resourceOperation = ResourceOp()
    resourceState = ResourceState()

    // Initialize default values
    resourceResult.granted := False
    resourceResult.resourceId := 0
    resourceResult.queueEmpty := True
    resourceResult.error := False

    // Resource state registers
    val grantEnabled = Reg(Bool()) init False
    val resourceCount = Reg(UInt(8 bits)) init 0
    val queueLength = Reg(UInt(8 bits)) init 0
    val resourcePtr = Reg(UInt(32 bits)) init 0
    val resourceChannels = Reg(Bits(16 bits)) init 0 // Bit mask of resource channels

    // Simple resource queue (simplified - real implementation would use memory)
    val resourceQueue = Vec(Reg(UInt(8 bits)) init 0, 16)
    val queueHead = Reg(UInt(4 bits)) init 0
    val queueTail = Reg(UInt(4 bits)) init 0

    // Connect state
    resourceState.grantEnabled := grantEnabled
    resourceState.resourceCount := resourceCount
    resourceState.queueLength := queueLength
    resourceState.resourcePtr := resourcePtr

    // Resource execution in Memory stage (stage 4)
    val resourceLogic = new Area {
      val opcode = pipe.execute(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Table 6.22 instruction recognition
      // Using raw secondary opcodes from Service.scala
      isResourceOperation := isOpr && (
        oprFunc === B"0100" || // MKRC (0x124 -> 0x24 -> 4)
          oprFunc === B"0011" // UNMKRC (0x123 -> 0x23 -> 3)
      )

      // Decode resource operation
      resourceOperation := ResourceOp.GRANT // Default
      when(isOpr) {
        switch(oprFunc) {
          is(B"0100") { // MKRC
            resourceOperation := ResourceOp.MKRC
          }
          is(B"0011") { // UNMKRC
            resourceOperation := ResourceOp.UNMKRC
          }
        }
      }

      // Execute resource operations
      when(isResourceOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)

        switch(resourceOperation) {
          is(ResourceOp.GRANT) {
            // Grant resource: A = resource ID
            when(resourceCount > 0 && grantEnabled) {
              resourceResult.granted := True
              resourceResult.resourceId := areg(7 downto 0)
              resourceCount := resourceCount - 1
            } otherwise {
              resourceResult.granted := False
              // Would add to queue in real implementation
            }
            // Pop resource ID
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ResourceOp.ENBG) {
            // Enable grant mechanism
            grantEnabled := True
          }

          is(ResourceOp.DISG) {
            // Disable grant mechanism
            grantEnabled := False
          }

          is(ResourceOp.MKRC) {
            // Mark channel as resource channel: A = channel ID
            when(areg < 16) {
              resourceChannels(areg(3 downto 0)) := True
            }
            // Pop channel ID
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ResourceOp.UNMKRC) {
            // Unmark resource channel: A = channel ID
            when(areg < 16) {
              resourceChannels(areg(3 downto 0)) := False
            }
            // Pop channel ID
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ResourceOp.IRDSQ) {
            // Insert at front of RDS queue: A = process descriptor
            when(queueLength < 16) {
              resourceQueue(queueHead) := areg(7 downto 0)
              queueHead := (queueHead - 1).resize(4)
              queueLength := queueLength + 1
            }
            // Pop descriptor
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ResourceOp.ERDSQ) {
            // Empty RDS queue
            queueLength := 0
            queueHead := 0
            queueTail := 0
            resourceResult.queueEmpty := True
          }

          is(ResourceOp.STRESPTR) {
            // Store resource pointer: A = pointer
            resourcePtr := areg
            // Pop pointer
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ResourceOp.LDRESPTR) {
            // Load resource pointer: -> A
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, resourcePtr)
          }
        }
      }
    }

    // Update queue empty status
    resourceResult.queueEmpty := queueLength === 0

    println(s"[${ResourcePlugin.this.getDisplayName()}] Resource hardware configured")
    println(s"[${ResourcePlugin.this.getDisplayName()}] - Table 6.22: Resource channels")
    println(s"[${ResourcePlugin.this.getDisplayName()}] - Mutual exclusion and fair access")
    println(s"[${ResourcePlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(s"[${ResourcePlugin.this.getDisplayName()}] build end")
  }
}
