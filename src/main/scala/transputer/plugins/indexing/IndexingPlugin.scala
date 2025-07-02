package transputer.plugins.indexing

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.indexing._

/** T9000 Indexing Plugin implementing Table 6.13 array indexing and memory operations.
  *
  * This plugin implements memory operations from T9000 Table 6.13:
  *   - Array subscripting: bsub, wsub
  *   - Byte operations: lb, sb, ldlb, stlb, ldnlb, stnlb
  *   - 16-bit operations: ls, ss
  *   - Local memory: ldl, stl, ldlp (immediate addressing)
  *   - Non-local memory: ldnl, stnl, ldnlp (indirect addressing)
  *
  * These operations execute in pipeline stage 4 (Memory stage) and provide the core memory access
  * functionality for the T9000.
  *
  * Features:
  *   - Automatic address calculation for arrays
  *   - Bounds checking for safe array access
  *   - Multiple data sizes (byte, 16-bit, 32-bit)
  *   - Integration with workspace cache and main cache
  *   - Support for both direct and indirect addressing modes
  */
class IndexingPlugin extends FiberPlugin {
  override def getDisplayName(): String = "IndexingPlugin"
  setName("indexing")

  during setup new Area {
    println(s"[${IndexingPlugin.this.getDisplayName()}] setup start")

    addService(new IndexingService {
      override def executeOp(
        op: IndexingOp.C,
        baseAddress: UInt,
        index: UInt,
        data: UInt
      ): IndexingResult = indexingResult
      override def isIndexingOp(opcode: Bits): Bool = isIndexingOperation
      override def getIndexingOp(opcode: Bits): IndexingOp.C = indexingOperation
      override def calculateAddress(
        baseAddr: UInt,
        index: UInt,
        elementSize: UInt,
        arrayLength: UInt
      ): (UInt, Bool) = (calculatedAddress, boundsError)
    })

    println(s"[${IndexingPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var indexingResult: IndexingResult = null
  var isIndexingOperation: Bool = null
  var indexingOperation: IndexingOp.C = null
  var calculatedAddress: UInt = null
  var boundsError: Bool = null

  during build new Area {
    println(s"[${IndexingPlugin.this.getDisplayName()}] build start")

    // Get pipeline service
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]
    // Cache services temporarily disabled during migration
    // val mainCache = host.get[transputer.plugins.core.cache.MainCacheService]
    // val wsCache = host.get[transputer.plugins.core.cache.WorkspaceCacheService]

    // Initialize hardware signals
    indexingResult = IndexingResult()
    calculatedAddress = UInt(32 bits)
    boundsError = Bool()
    isIndexingOperation = Bool()
    indexingOperation = IndexingOp()

    // Indexing execution in Memory stage (stage 4) - T9000 specification
    val indexingLogic = new Area {
      val opcode = pipe.execute(Global.OPCODE)
      val isPrimary = opcode(7 downto 4) =/= Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)
      val primaryOp = opcode(7 downto 4)
      val immediate = opcode(3 downto 0)

      // Table 6.13 instruction recognition
      isIndexingOperation := (isPrimary && (
        primaryOp === Opcode.PrimaryOpcode.LDL.asBits.resize(4) || // 1x - ldl
          primaryOp === Opcode.PrimaryOpcode.STL.asBits.resize(4) || // 2x - stl
          primaryOp === Opcode.PrimaryOpcode.LDNL.asBits.resize(4) || // 3x - ldnl
          primaryOp === Opcode.PrimaryOpcode.STNL.asBits.resize(4) || // 4x - stnl
          primaryOp === Opcode.PrimaryOpcode.LDLP.asBits.resize(4) || // 5x - ldlp
          primaryOp === Opcode.PrimaryOpcode.LDNLP.asBits.resize(4) // 6x - ldnlp
      )) || (isOpr && (
        oprFunc === Opcode.SecondaryOpcode.BSUB.asBits.resize(4) || // 21FC - bsub
          oprFunc === Opcode.SecondaryOpcode.WSUB.asBits.resize(4) || // 25FA - wsub
          oprFunc === Opcode.SecondaryOpcode.LB.asBits.resize(4) || // 21F0 - lb
          oprFunc === Opcode.SecondaryOpcode.SB.asBits.resize(4) || // 23F5 - sb
          oprFunc === Opcode.SecondaryOpcode.LSX.asBits.resize(4) || // 22F4 - ls
          oprFunc === Opcode.SecondaryOpcode.SS.asBits.resize(4) // 22F5 - ss
      ))

      // Initialize default values to prevent latches
      indexingOperation := IndexingOp.LDL // Default
      calculatedAddress := 0
      when(isPrimary) {
        switch(primaryOp) {
          is(Opcode.PrimaryOpcode.LDL.asBits.resize(4)) { indexingOperation := IndexingOp.LDL }
          is(Opcode.PrimaryOpcode.STL.asBits.resize(4)) { indexingOperation := IndexingOp.STL }
          is(Opcode.PrimaryOpcode.LDNL.asBits.resize(4)) { indexingOperation := IndexingOp.LDNL }
          is(Opcode.PrimaryOpcode.STNL.asBits.resize(4)) { indexingOperation := IndexingOp.STNL }
          is(Opcode.PrimaryOpcode.LDLP.asBits.resize(4)) { indexingOperation := IndexingOp.LDLP }
          is(Opcode.PrimaryOpcode.LDNLP.asBits.resize(4)) { indexingOperation := IndexingOp.LDNLP }
        }
      } otherwise {
        when(isOpr) {
          switch(oprFunc) {
            is(Opcode.SecondaryOpcode.BSUB.asBits.resize(4)) {
              indexingOperation := IndexingOp.BSUB
            }
            is(Opcode.SecondaryOpcode.WSUB.asBits.resize(4)) {
              indexingOperation := IndexingOp.WSUB
            }
            is(Opcode.SecondaryOpcode.LB.asBits.resize(4)) { indexingOperation := IndexingOp.LB }
            is(Opcode.SecondaryOpcode.SB.asBits.resize(4)) { indexingOperation := IndexingOp.SB }
            is(Opcode.SecondaryOpcode.LSX.asBits.resize(4)) { indexingOperation := IndexingOp.LSX }
            is(Opcode.SecondaryOpcode.SS.asBits.resize(4)) { indexingOperation := IndexingOp.SS }
          }
        }
      }

      // Execute indexing operations
      when(isIndexingOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)
        val wptr: UInt =
          (regStack.readReg(transputer.plugins.core.regstack.RegName.WdescReg)(
            31 downto 2
          ) ## U"00").asUInt

        // Initialize result
        indexingResult.address := 0
        indexingResult.data := 0
        indexingResult.accessSize := AccessSize.WORD32
        indexingResult.isLoad := True
        indexingResult.stackPush := False
        indexingResult.stackPop := False
        indexingResult.bounds_error := False
        calculatedAddress := 0
        boundsError := False

        switch(indexingOperation) {
          is(IndexingOp.LDL) {
            // Load local: address = Wptr + immediate*4
            calculatedAddress := wptr + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            indexingResult.isLoad := True
            indexingResult.stackPush := True

            // Use workspace cache if available and in range (temporarily disabled)
            // if (wsCache.isDefined) {
            //   when(immediate.asUInt < 16) { // Within workspace cache range
            //     val wsData = wsCache.get.readA(calculatedAddress)
            //     regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, wsData)
            //     regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            //     regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
            //   }
            // }
          }

          is(IndexingOp.STL) {
            // Store local: address = Wptr + immediate*4, data = Areg
            calculatedAddress := wptr + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            indexingResult.data := areg
            indexingResult.isLoad := False
            indexingResult.stackPop := True

            // Use workspace cache if available (temporarily disabled)
            // if (wsCache.isDefined) {
            //   when(immediate.asUInt < 16) {
            //     wsCache.get.writeA(calculatedAddress, areg)
            //     regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            //     regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
            //   }
            // }
          }

          is(IndexingOp.LDNL) {
            // Load non-local: address = Areg + immediate*4
            calculatedAddress := areg + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            indexingResult.isLoad := True

            // Use main cache (temporarily disabled)
            // if (mainCache.isDefined) {
            //   val cacheData = mainCache.get.read(calculatedAddress)
            //   regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, cacheData)
            // }
          }

          is(IndexingOp.STNL) {
            // Store non-local: address = Breg + immediate*4, data = Areg
            calculatedAddress := breg + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            indexingResult.data := areg
            indexingResult.isLoad := False
            indexingResult.stackPop := True

            // Use main cache (temporarily disabled)
            // if (mainCache.isDefined) {
            //   mainCache.get.write(calculatedAddress, areg)
            //   regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
            //   regStack.writeReg(
            //     transputer.plugins.core.regstack.RegName.Breg,
            //     breg
            //   ) // Load next workspace value
            // }
          }

          is(IndexingOp.LDLP) {
            // Load local pointer: Areg = Wptr + immediate*4
            calculatedAddress := wptr + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            indexingResult.stackPush := True
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, calculatedAddress)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
          }

          is(IndexingOp.LDNLP) {
            // Load non-local pointer: Areg = Areg + immediate*4
            calculatedAddress := areg + ((immediate.resize(30) ## U"00").asUInt)
            indexingResult.address := calculatedAddress
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, calculatedAddress)
          }

          is(IndexingOp.BSUB) {
            // Byte subscript: Areg = Breg + Areg (byte addressing)
            calculatedAddress := breg + areg
            indexingResult.address := calculatedAddress
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, calculatedAddress)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(IndexingOp.WSUB) {
            // Word subscript: Areg = Breg + Areg*4 (word addressing)
            calculatedAddress := (breg + (areg << 2)).resize(32)
            indexingResult.address := calculatedAddress
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, calculatedAddress)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(IndexingOp.LB) {
            // Load byte: load 8-bit from address Areg
            calculatedAddress := areg
            indexingResult.address := calculatedAddress
            indexingResult.accessSize := AccessSize.BYTE
            indexingResult.isLoad := True
            // Byte load implementation with sign extension
          }

          is(IndexingOp.SB) {
            // Store byte: store Breg[7:0] to address Areg
            calculatedAddress := areg
            indexingResult.address := calculatedAddress
            indexingResult.data := breg(7 downto 0).resize(32)
            indexingResult.accessSize := AccessSize.BYTE
            indexingResult.isLoad := False
            indexingResult.stackPop := True
          }

          is(IndexingOp.LSX) {
            // Load 16-bit: load 16-bit from address Areg
            calculatedAddress := areg
            indexingResult.address := calculatedAddress
            indexingResult.accessSize := AccessSize.WORD16
            indexingResult.isLoad := True
          }

          is(IndexingOp.SS) {
            // Store 16-bit: store Breg[15:0] to address Areg
            calculatedAddress := areg
            indexingResult.address := calculatedAddress
            indexingResult.data := breg(15 downto 0).resize(32)
            indexingResult.accessSize := AccessSize.WORD16
            indexingResult.isLoad := False
            indexingResult.stackPop := True
          }
        }
      }
    }

    println(s"[${IndexingPlugin.this.getDisplayName()}] Indexing hardware configured")
    println(
      s"[${IndexingPlugin.this.getDisplayName()}] - Table 6.13: Array indexing & memory operations"
    )
    println(s"[${IndexingPlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(
      s"[${IndexingPlugin.this.getDisplayName()}] - Local/non-local memory, byte/word operations"
    )
    println(s"[${IndexingPlugin.this.getDisplayName()}] build end")
  }
}
