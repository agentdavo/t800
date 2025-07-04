package transputer

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.plugin.PluginHost
import scala.collection.mutable
import scala.io.Source
import java.io.{File, PrintWriter}

// T9000 Hardware Test Bench for simulation
class T9000HardwareTestBench(initialMemory: Map[Long, Int]) extends Component {
  val io = new Bundle {
    // Debug outputs to monitor processor state
    val iptr = out UInt(32 bits)
    val areg = out UInt(32 bits)
    val breg = out UInt(32 bits)
    val creg = out UInt(32 bits)
    val wptr = out UInt(32 bits)
    val halted = out Bool()
  }
  
  // Use the existing Transputer object with T9000 configuration
  val param = T9000Param(
    enableFpu = false,      // Disable FPU for faster simulation
    enablePmi = false,      // PMI disabled due to hierarchy issues
    enableVcp = false,      // Disable VCP for simplicity
    enableScheduler = false, // Single process for bootloader
    enableTimers = false,    // No timers needed for bootloader
    enableMmu = false       // No MMU for bootloader
  )
  
  // Configure globals for T9000
  T9000Transputer.configureGlobals(param)
  
  // Use the existing Transputer.apply() method
  val transputer = Transputer(param.plugins())
  
  // Create memory slave to respond to system bus
  val memorySize = 1 << 20  // 1MB memory
  val memory = Mem(Bits(8 bits), memorySize)
  
  // Initialize memory with boot ROM content
  val initData = Array.fill[Bits](memorySize)(B(0, 8 bits))
  for ((addr, value) <- initialMemory) {
    if (addr >= 0 && addr < memorySize) {
      initData(addr.toInt) = B(value & 0xFF, 8 bits)
    }
  }
  memory.init(initData)
  
  // BMB memory slave
  val memoryBmb = new Area {
    val bmb = slave(Bmb(transputer.systemBus.p))
    
    // Simple memory controller state machine
    val cmdReady = RegInit(True)
    val rspValid = RegInit(False)
    val rspSource = Reg(UInt(4 bits))
    val rspData = Reg(Bits(128 bits))
    val rspLast = Reg(Bool())
    
    bmb.cmd.ready := cmdReady
    bmb.rsp.valid := rspValid
    bmb.rsp.payload.fragment.source := rspSource
    bmb.rsp.payload.fragment.opcode := Bmb.Rsp.Opcode.SUCCESS
    bmb.rsp.payload.fragment.data := rspData
    bmb.rsp.payload.fragment.context := 0
    bmb.rsp.payload.last := rspLast
    
    when(bmb.cmd.valid && cmdReady) {
      cmdReady := False
      rspValid := True
      rspSource := bmb.cmd.payload.fragment.source
      rspLast := bmb.cmd.payload.last
      
      val addr = bmb.cmd.payload.fragment.address
      val isWrite = bmb.cmd.payload.fragment.opcode === Bmb.Cmd.Opcode.WRITE
      
      when(isWrite) {
        // Write operation - write each byte
        for (i <- 0 until 16) {
          when(bmb.cmd.payload.fragment.mask(i)) {
            memory((addr + i).resized) := bmb.cmd.payload.fragment.data(i*8+7 downto i*8)
          }
        }
        rspData := 0
      } otherwise {
        // Read operation - read 16 bytes
        val readData = Vec(Bits(8 bits), 16)
        for (i <- 0 until 16) {
          readData(i) := memory((addr + i).resized)
        }
        rspData := readData.asBits
      }
    }
    
    when(bmb.rsp.valid && bmb.rsp.ready) {
      rspValid := False
      cmdReady := True
    }
  }
  
  // Connect memory to system bus (memory is slave, transputer is master)
  transputer.systemBus >> memoryBmb.bmb
  
  // We'll expose signals in the simulation code, not here
  
  // For now, still provide default values
  io.iptr := 0x80000000L  // Default boot address
  io.areg := 0
  io.breg := 0  
  io.creg := 0
  io.wptr := 0x80000230L  // Default workspace
  io.halted := False
}

object BootloadTraceSim {
  
  // T9000 instruction trace format
  case class TraceEntry(
    iptr: Long,
    code: List[Int],
    mnemonic: String,
    operand: String,
    halt: String = "--",
    areg: Long = 0,
    breg: Long = 0,
    creg: Long = 0,
    wptr: Long = 0x80000230,
    wptrData: Long = 0
  ) {
    def format: String = {
      val codeStr = code.map(b => f"$b%02X").mkString(" ")
      val codeField = f"$codeStr%-30s"
      val mnemonicField = f"$mnemonic%-8s $operand%-12s"
      f" ${iptr}%08X: $codeField$mnemonicField$halt%-4s ${areg}%08X ${breg}%08X ${creg}%08X   ${wptr}%08X ${wptrData}%08X"
    }
  }
  
  // Instruction decoder
  def decodeInstruction(bytes: List[Int], iptr: Long): (String, String, Int) = {
    if (bytes.isEmpty) return ("???", "", 1)
    
    val opcode = (bytes.head >> 4) & 0xF
    val operand = bytes.head & 0xF
    
    val (mnemonic, operandStr, length) = opcode match {
      case 0x0 => ("J", s"#${operand.toHexString}", 1)
      case 0x1 => ("LDLP", s"#$operand", 1)
      case 0x2 => ("PFIX", s"#$operand", 1)
      case 0x3 => ("LDNL", s"#$operand", 1)
      case 0x4 => ("LDC", s"#$operand", 1)
      case 0x5 => ("LDNLP", s"#$operand", 1)
      case 0x6 => ("NFIX", s"#$operand", 1)
      case 0x7 => ("LDL", s"#$operand", 1)
      case 0x8 => ("ADC", s"#$operand", 1)
      case 0x9 => ("CALL", s"#${operand.toHexString}", 1)
      case 0xA => ("CJ", s"#${operand.toHexString}", 1)
      case 0xB => ("AJW", s"#$operand", 1)
      case 0xC => ("EQC", s"#$operand", 1)
      case 0xD => ("STL", s"#$operand", 1)
      case 0xE => ("STNL", s"#$operand", 1)
      case 0xF => 
        // Secondary operations
        val fullOp = if (bytes.length > 1 && (bytes.head & 0xF) == 0x2) {
          // Extended secondary op
          ((operand << 4) | (bytes(1) & 0xF))
        } else {
          operand
        }
        
        val (secMnemonic, secLength) = fullOp match {
          case 0x00 => ("REV", 1)
          case 0x05 => ("ADD", 1)
          case 0x06 => ("DUP", 1)
          case 0x07 => ("GCALL", 1)
          case 0x08 => ("IN", 1)
          case 0x0B => ("WSUB", 1)
          case 0x0C => ("OUT", 1)
          case 0x0D => ("SUB", 1)
          case 0x18 => ("STHF", 1)
          case 0x19 => ("STLF", 1)
          case 0x1B => ("LDPI", 2)  // Uses prefix
          case 0x20 => ("RET", 1)
          case 0x21 => ("LEND", 2)
          case 0x29 => ("TESTERR", 1)
          case 0x2F => ("TERMINATE", 1)
          case 0x33 => ("XOR", 1)
          case 0x3C => ("GAJW", 1)
          case 0x42 => ("MINT", 1)
          case 0x54 => ("STTIMER", 1)
          case 0x57 => ("CLRHALTERR", 1)
          case 0x58 => ("SETHALTERR", 1)
          case 0x9C => ("FPTESTERR", 1)
          case _ => (f"OPR#${fullOp}%02X", 1)
        }
        (secMnemonic, "", if (fullOp > 15) 2 else secLength)
      case _ => ("???", "", 1)
    }
    
    (mnemonic, operandStr, length)
  }
  
  // Load hex file into memory
  def loadHexFile(filename: String): Map[Long, Int] = {
    val memory = mutable.Map[Long, Int]()
    var extendedAddr = 0L
    
    val source = Source.fromFile(filename)
    try {
      for (line <- source.getLines() if line.startsWith(":")) {
        val recordType = line.substring(7, 9)
        recordType match {
          case "04" => // Extended Linear Address
            extendedAddr = Integer.parseInt(line.substring(9, 13), 16).toLong << 16
          case "00" => // Data
            val byteCount = Integer.parseInt(line.substring(1, 3), 16)
            val address = Integer.parseInt(line.substring(3, 7), 16).toLong + extendedAddr
            for (i <- 0 until byteCount) {
              val byte = Integer.parseInt(line.substring(9 + i * 2, 11 + i * 2), 16)
              memory(address + i) = byte
            }
          case _ => // Ignore other record types
        }
      }
    } finally {
      source.close()
    }
    
    memory.toMap
  }
  
  def main(args: Array[String]): Unit = {
    val hexFile = if (args.nonEmpty) args(0) else "scripts/hex/bootload.hex"
    val traceFile = if (args.length > 1) args(1) else "scripts/test_reports/bootload_trace.txt"
    
    println(s"Loading hex file: $hexFile")
    val memoryMap = loadHexFile(hexFile)
    val memory = mutable.Map[Long, Int]() ++ memoryMap  // Create mutable copy
    println(s"Loaded ${memory.size} bytes")
    
    // Initialize processor state
    var iptr = 0x80000000L
    var areg = 0L
    var breg = 0L
    var creg = 0L
    var wptr = 0x80000230L
    var halted = false
    val traces = mutable.ListBuffer[TraceEntry]()
    
    // Simulation parameters
    val maxCycles = 1000
    var cycle = 0
    
    println("\n" + "-" * 110)
    println("-IPtr------Code--------Mnemonic------------HE---AReg-----BReg-----CReg-------WPtr-----WPtr[0]-")
    println("-" * 110)
    
    val writer = new PrintWriter(new File(traceFile))
    writer.println("-IPtr------Code--------Mnemonic------------HE---AReg-----BReg-----CReg-------WPtr-----WPtr[0]-")
    writer.println("-" * 110)
    
    try {
      while (!halted && cycle < maxCycles && memory.contains(iptr)) {
        // Fetch instruction bytes
        val bytes = (0 to 3).map(i => memory.getOrElse(iptr + i, 0)).toList
        val (mnemonic, operand, length) = decodeInstruction(bytes, iptr)
        val instrBytes = bytes.take(length)
        
        // Create trace entry
        val trace = TraceEntry(
          iptr = iptr,
          code = instrBytes,
          mnemonic = mnemonic,
          operand = operand,
          areg = areg,
          breg = breg,
          creg = creg,
          wptr = wptr,
          wptrData = memory.getOrElse(wptr, 0).toLong
        )
        
        traces += trace
        println(trace.format)
        writer.println(trace.format)
        
        // Simple instruction execution simulation
        val opcode = (bytes.head >> 4) & 0xF
        val op = bytes.head & 0xF
        
        opcode match {
          case 0x4 => // LDC
            creg = breg
            breg = areg
            areg = op
          case 0x7 => // LDL
            creg = breg
            breg = areg
            areg = memory.getOrElse(wptr + op * 4, 0).toLong & 0xFFFFFFFFL
          case 0xB => // AJW
            wptr = wptr + op * 4
          case 0xD => // STL
            memory(wptr + op * 4) = areg.toInt  // Now can modify mutable map
          case 0xF if op == 0x42 => // MINT
            creg = breg
            breg = areg
            areg = 0x80000000L
          case 0xF if op == 0x2F => // TERMINATE
            halted = true
            println("\n-I-TRACE: Processor terminated")
            writer.println("\n-I-TRACE: Processor terminated")
          case _ => // Other instructions - just advance
        }
        
        iptr += length
        cycle += 1
        
        // Add some debug messages for specific operations
        mnemonic match {
          case "IN" =>
            println(s"-I-TRACE: In(1): Channel=#${areg.toHexString}, to memory at #${breg.toHexString}, length #${creg}")
            writer.println(s"-I-TRACE: In(1): Channel=#${areg.toHexString}, to memory at #${breg.toHexString}, length #${creg}")
          case "GCALL" =>
            println(s"-I-TRACE: Calling function at #${areg.toHexString}")
            writer.println(s"-I-TRACE: Calling function at #${areg.toHexString}")
          case _ =>
        }
      }
      
      println("-" * 110)
      writer.println("-" * 110)
      println(s"\nSimulation complete after $cycle cycles")
      writer.println(s"\nSimulation complete after $cycle cycles")
      
      if (cycle >= maxCycles) {
        println("Warning: Maximum cycles reached")
        writer.println("Warning: Maximum cycles reached")
      }
      
    } finally {
      writer.close()
    }
    
    println(s"\nTrace written to: $traceFile")
  }
  
  // Hardware simulation using actual T9000
  def hardwareSimulation(args: Array[String]): Unit = {
    val hexFile = if (args.nonEmpty) args(0) else "scripts/hex/bootload.hex"
    val traceFile = if (args.length > 1) args(1) else "scripts/test_reports/bootload_hardware_trace.txt"
    
    println(s"Loading hex file for hardware simulation: $hexFile")
    val memoryMap = loadHexFile(hexFile)
    println(s"Loaded ${memoryMap.size} bytes")
    
    // Configuration for simulation
    val compiled = SimConfig.withWave.compile {
      val dut = new T9000HardwareTestBench(memoryMap)
      
      println("\n[SimConfig] T9000 testbench created successfully")
      println(s"[SimConfig] Transputer component: ${dut.transputer.getDisplayName()}")
      
      // List the component hierarchy for debugging
      def printComponentTree(comp: Component, indent: String = ""): Unit = {
        val children = comp.children
        println(s"$indent- ${comp.getDisplayName()} (${children.size} children)")
        if (children.nonEmpty && indent.length < 8) {  // Limit depth
          children.foreach(child => printComponentTree(child, indent + "  "))
        }
      }
      
      println("\n[SimConfig] T9000 component structure:")
      printComponentTree(dut.transputer, "  ")
      
      dut
    }
    
    compiled.doSim { dut =>
      // Fork clock
      dut.clockDomain.forkStimulus(period = 10)
      
      // Reset sequence
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      val writer = new PrintWriter(new File(traceFile))
      writer.println("T9000 Hardware Simulation Trace")
      writer.println("=" * 80)
      
      // Try to access internal signals that were made public
      println("\n[Simulation] Attempting to access T9000 internal signals...")
      
      // Try to find exposed signals using SpinalSim's signal access
      var foundSignals = Map[String, Any]()
      
      try {
        // Use reflection to access signals that were made public
        import scala.reflect.runtime.{universe => ru}
        val mirror = ru.runtimeMirror(getClass.getClassLoader)
        val dutMirror = mirror.reflect(dut)
        
        // Try common signal patterns
        val signalPatterns = List(
          "transputer", "fetch", "regstack", "pipeline", 
          "iptr", "areg", "breg", "creg", "wptr"
        )
        
        // For now, let's see the component structure
        println("[Simulation] T9000 component structure:")
        def printComponentTree(comp: Component, indent: String = ""): Unit = {
          val children = comp.children
          println(s"$indent- ${comp.getDisplayName()} (${children.size} children)")
          if (children.nonEmpty && indent.length < 8) {  // Limit depth
            children.foreach(child => printComponentTree(child, indent + "  "))
          }
        }
        printComponentTree(dut.transputer, "  ")
        
      } catch {
        case e: Exception =>
          println(s"[Simulation] Could not access internal structure: ${e.getMessage}")
      }
      
      writer.println("-Cycle--IPtr------Code--------Mnemonic------------AReg-----BReg-----CReg-----WPtr---")
      writer.println("-" * 80)
      
      try {
        var cycle = 0
        val maxCycles = 10000
        var lastIptr = 0L
        
        while (cycle < maxCycles && !dut.io.halted.toBoolean) {
          dut.clockDomain.waitSampling()
          
          // Sample processor state every cycle
          // For now, use the IO signals (which are hardcoded)
          // In a real implementation, we would access internal signals here
          val iptr = dut.io.iptr.toLong
          val areg = dut.io.areg.toLong
          val breg = dut.io.breg.toLong
          val creg = dut.io.creg.toLong
          val wptr = dut.io.wptr.toLong
          
          // Only print trace when instruction pointer changes (new instruction)
          if (iptr != lastIptr || cycle % 100 == 0) {
            // Try to decode the instruction at current iptr
            val bytes = (0 to 3).map(i => 
              memoryMap.getOrElse(iptr + i, 0)
            ).toList
            val (mnemonic, operand, length) = decodeInstruction(bytes, iptr)
            val instrBytes = bytes.take(length)
            val codeStr = instrBytes.map(b => f"$b%02X").mkString(" ")
            
            val traceLine = f"$cycle%6d  ${iptr}%08X  $codeStr%-12s  $mnemonic%-8s $operand%-8s  ${areg}%08X ${breg}%08X ${creg}%08X ${wptr}%08X"
            
            println(traceLine)
            writer.println(traceLine)
            
            lastIptr = iptr
          }
          
          cycle += 1
        }
        
        writer.println("-" * 80)
        writer.println(s"Simulation complete after $cycle cycles")
        writer.println(s"Halted: ${dut.io.halted.toBoolean}")
        
        println("\n" + "=" * 80)
        println(s"Hardware simulation complete after $cycle cycles")
        println(s"Halted: ${dut.io.halted.toBoolean}")
        
      } finally {
        writer.close()
      }
      
      println(s"\nHardware trace written to: $traceFile")
    }
  }
}

// Entry point for hardware simulation
object BootloadHardwareSim extends App {
  BootloadTraceSim.hardwareSimulation(args)
}
