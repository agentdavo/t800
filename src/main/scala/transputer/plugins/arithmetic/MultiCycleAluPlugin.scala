package transputer.plugins.arithmetic

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import transputer.{Global, Opcode}
import transputer.plugins.core.pipeline.{PipelineStageService, T9000PipelinePlugin}

/** Multi-cycle ALU plugin demonstrating CtrlLaneApi usage
  *
  * This plugin shows how to implement multi-cycle operations using the
  * SpinalHDL Pipeline API's lane mechanism. It handles:
  * - Single-cycle operations (ADD, SUB, AND, OR, XOR)
  * - Multi-cycle operations (MUL, DIV)
  * - Pipeline stalls and hazard detection
  * - Result forwarding
  */
class MultiCycleAluPlugin extends FiberPlugin {
  override def getDisplayName(): String = "MultiCycleAluPlugin"
  setName("multiCycleAlu")
  
  // ALU operation states
  object AluState extends SpinalEnum {
    val IDLE, EXECUTING, COMPLETING = newElement()
  }
  
  // Service interface for other plugins
  trait MultiCycleAluService {
    def isAluBusy: Bool
    def aluResult: UInt
    def executeAluOp(op: Bits, a: UInt, b: UInt): Unit
  }
  
  during setup new Area {
    println(s"[${getDisplayName()}] Setting up multi-cycle ALU")
    
    // Register service
    addService(new MultiCycleAluService {
      var busySignal: Bool = null
      var resultSignal: UInt = null
      
      override def isAluBusy: Bool = {
        if (busySignal == null) False else busySignal
      }
      
      override def aluResult: UInt = {
        if (resultSignal == null) U(0, 32 bits) else resultSignal
      }
      
      override def executeAluOp(op: Bits, a: UInt, b: UInt): Unit = {
        // Implementation provided in build phase
      }
    })
  }
  
  during build new Area {
    println(s"[${getDisplayName()}] Building multi-cycle ALU")
    
    // Get pipeline service
    val t9000Pipeline = host[T9000PipelinePlugin]
    val pipeline = host[PipelineStageService]
    
    // Get ALU lane from T9000 pipeline
    val aluLane = t9000Pipeline.getAluLane
    val executeStage = pipeline.execute
    
    // ALU state machine
    val state = Reg(AluState()) init(AluState.IDLE)
    val operandA = Reg(UInt(32 bits)) init(0)
    val operandB = Reg(UInt(32 bits)) init(0)
    val operation = Reg(Bits(4 bits)) init(0)
    val result = Reg(UInt(32 bits)) init(0)
    val cyclesRemaining = Reg(UInt(6 bits)) init(0)
    
    // Multiplier implementation (simplified)
    val multiplier = new Area {
      val multiplicand = Reg(UInt(32 bits))
      val multiplier = Reg(UInt(32 bits))
      val accumulator = Reg(UInt(64 bits))
      val bitCounter = Reg(UInt(5 bits))
      
      def start(a: UInt, b: UInt): Unit = {
        multiplicand := a
        multiplier := b
        accumulator := 0
        bitCounter := 31
      }
      
      def step(): Unit = {
        when(multiplier.lsb) {
          accumulator := accumulator + (multiplicand.resize(64) << bitCounter)
        }
        multiplier := multiplier >> 1
        bitCounter := bitCounter - 1
      }
      
      def isDone: Bool = bitCounter === 0
      def getResult: UInt = accumulator(31 downto 0)
    }
    
    // Divider implementation (simplified)
    val divider = new Area {
      val dividend = Reg(UInt(32 bits))
      val divisor = Reg(UInt(32 bits))
      val quotient = Reg(UInt(32 bits))
      val remainder = Reg(UInt(32 bits))
      val bitCounter = Reg(UInt(5 bits))
      
      def start(a: UInt, b: UInt): Unit = {
        dividend := a
        divisor := b
        quotient := 0
        remainder := 0
        bitCounter := 31
      }
      
      def step(): Unit = {
        remainder := (remainder << 1) | dividend.msb.asUInt
        dividend := dividend << 1
        when(remainder >= divisor) {
          remainder := remainder - divisor
          quotient := (quotient << 1) | U(1)
        } otherwise {
          quotient := quotient << 1
        }
        bitCounter := bitCounter - 1
      }
      
      def isDone: Bool = bitCounter === 0
      def getResult: UInt = quotient
    }
    
    // Check if this is an ALU operation
    val opcode = executeStage(Global.OPCODE)
    val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
    val oprFunc = opcode(3 downto 0)
    
    val isAluOp = isOpr && (
      oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SUB.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.MUL.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.DIV.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.AND.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.OR.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.XOR.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.NOT.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SHL.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SHR.asBits.resize(4)
    )
    
    // Request ALU lane when we have an ALU operation
    aluLane.enable := executeStage.isValid && isAluOp && state === AluState.IDLE
    
    // Determine operation latency
    def getOpLatency(op: Bits): UInt = {
      val latency = UInt(6 bits)
      switch(op) {
        is(Opcode.SecondaryOpcode.MUL.asBits.resize(4)) { latency := 32 }
        is(Opcode.SecondaryOpcode.DIV.asBits.resize(4)) { latency := 32 }
        default { latency := 1 }
      }
      latency
    }
    
    // State machine
    switch(state) {
      is(AluState.IDLE) {
        when(aluLane.isUsed) {
          // Capture operands and start operation
          operandA := executeStage(Global.AREG_VALUE)
          operandB := executeStage(Global.BREG_VALUE)
          operation := oprFunc
          cyclesRemaining := getOpLatency(oprFunc)
          
          // Start multi-cycle operations
          switch(oprFunc) {
            is(Opcode.SecondaryOpcode.MUL.asBits.resize(4)) {
              multiplier.start(operandA, operandB)
              state := AluState.EXECUTING
            }
            is(Opcode.SecondaryOpcode.DIV.asBits.resize(4)) {
              divider.start(operandA, operandB)
              state := AluState.EXECUTING
            }
            default {
              // Single-cycle operations complete immediately
              result := performSingleCycleOp(oprFunc, operandA, operandB)
              state := AluState.COMPLETING
            }
          }
        }
      }
      
      is(AluState.EXECUTING) {
        // Continue multi-cycle operation
        aluLane.halt := True  // Stall the pipeline
        cyclesRemaining := cyclesRemaining - 1
        
        switch(operation) {
          is(Opcode.SecondaryOpcode.MUL.asBits.resize(4)) {
            multiplier.step()
            when(multiplier.isDone) {
              result := multiplier.getResult
              state := AluState.COMPLETING
            }
          }
          is(Opcode.SecondaryOpcode.DIV.asBits.resize(4)) {
            divider.step()
            when(divider.isDone) {
              result := divider.getResult
              state := AluState.COMPLETING
            }
          }
        }
      }
      
      is(AluState.COMPLETING) {
        // Write result and return to idle
        executeStage(Global.ALU_RESULT) := result
        state := AluState.IDLE
      }
    }
    
    // Update service signals
    val service = host.getService[MultiCycleAluService]
    service.asInstanceOf[MultiCycleAluService] {
      case s: MultiCycleAluService =>
        s.busySignal = (state =/= AluState.IDLE)
        s.resultSignal = result
    }
    
    // Performance counters
    val cycleCount = Reg(UInt(32 bits)) init(0)
    val mulCount = Reg(UInt(32 bits)) init(0)
    val divCount = Reg(UInt(32 bits)) init(0)
    
    cycleCount := cycleCount + 1
    
    when(aluLane.isUsed && oprFunc === Opcode.SecondaryOpcode.MUL.asBits.resize(4)) {
      mulCount := mulCount + 1
    }
    
    when(aluLane.isUsed && oprFunc === Opcode.SecondaryOpcode.DIV.asBits.resize(4)) {
      divCount := divCount + 1
    }
    
    println(s"[${getDisplayName()}] Multi-cycle ALU build complete")
  }
  
  def performSingleCycleOp(op: Bits, a: UInt, b: UInt): UInt = {
    val result = UInt(32 bits)
    
    switch(op) {
      is(Opcode.SecondaryOpcode.ADD.asBits.resize(4)) { result := a + b }
      is(Opcode.SecondaryOpcode.SUB.asBits.resize(4)) { result := a - b }
      is(Opcode.SecondaryOpcode.AND.asBits.resize(4)) { result := a & b }
      is(Opcode.SecondaryOpcode.OR.asBits.resize(4))  { result := a | b }
      is(Opcode.SecondaryOpcode.XOR.asBits.resize(4)) { result := a ^ b }
      is(Opcode.SecondaryOpcode.NOT.asBits.resize(4)) { result := ~a }
      is(Opcode.SecondaryOpcode.SHL.asBits.resize(4)) { result := a |<< b(4 downto 0) }
      is(Opcode.SecondaryOpcode.SHR.asBits.resize(4)) { result := a |>> b(4 downto 0) }
      default { result := 0 }
    }
    
    result
  }
}