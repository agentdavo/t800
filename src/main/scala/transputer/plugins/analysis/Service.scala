package transputer.plugins.analysis

import spinal.core._
import spinal.lib._
import transputer.Global._

/** T9000 Error Detection and Analysis Service Interface Provides comprehensive error detection,
  * analysis, and debugging support
  */
trait AnalysisService {
  // Analysis Control
  def analysisEnabled: Bool // Analysis unit enabled
  def analysisMode: Bits // Analysis mode register
  def analysisPaused: Bool // Analysis is paused
  def analysisTriggered: Bool // Analysis trigger fired

  // Breakpoint Management
  def setBreakpoint(address: UInt, condition: Bits): Unit
  def clearBreakpoint(index: Int): Unit
  def breakpointHit(index: Int): Bool
  def breakpointAddress(index: Int): UInt
  def breakpointCount: UInt

  // Trace Buffer
  def traceEnabled: Bool // Instruction trace enabled
  def traceBufferFull: Bool // Trace buffer is full
  def traceBufferCount: UInt // Number of entries in trace buffer
  def getTraceEntry(index: UInt): Bits
  def clearTraceBuffer(): Unit

  // Performance Counters
  def instructionCount: UInt // Total instructions executed
  def cycleCount: UInt // Total cycles elapsed
  def cacheHitCount: UInt // Cache hit counter
  def cacheMissCount: UInt // Cache miss counter
  def branchCount: UInt // Branch instruction counter
  def branchMispredictCount: UInt // Branch misprediction counter

  // Error Detection
  def errorDetected: Bool // Any error detected
  def parityError: Bool // Parity error detected
  def timeoutError: Bool // Timeout error detected
  def protectionError: Bool // Memory protection error
  def alignmentError: Bool // Data alignment error
  def divideByZeroError: Bool // Division by zero error
  def stackOverflowError: Bool // Stack overflow error
  def stackUnderflowError: Bool // Stack underflow error

  // System State Analysis
  def currentIptr: UInt // Current instruction pointer
  def currentOpcode: Bits // Current instruction opcode
  def stackDepth: UInt // Current stack depth
  def processState: Bits // Current process state
  def linkStatus: Vec[Bits] // Status of all links

  // Analysis Methods
  def enableAnalysis(): Unit
  def disableAnalysis(): Unit
  def pauseAnalysis(): Unit
  def resumeAnalysis(): Unit
  def triggerAnalysis(): Unit
  def resetCounters(): Unit
  def updateRegisters(): Unit
}
