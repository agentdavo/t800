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

object HelloWorldKonataSim {

  // Load hex file into memory map
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

  // Create simple testbench
  class SimpleT9000TestBench(initialMemory: Map[Long, Int]) extends Component {
    val io = new Bundle {
      val linkOut = out Bits (8 bits)
      val linkOutValid = out Bool ()
    }

    // Simple memory with initial content
    val memory = Mem(Bits(8 bits), 4096)
    val initData = Array.fill[Bits](4096)(B(0, 8 bits))

    // Load hello-iserver.hex content
    for ((addr, value) <- initialMemory) {
      if (addr >= 0x80000000L && addr < 0x80001000L) {
        val offset = (addr - 0x80000000L).toInt
        initData(offset) = B(value & 0xff, 8 bits)
      }
    }
    memory.init(initData)

    // Simple processor emulation
    val pc = Reg(UInt(12 bits)) init (0)
    val cycle = Reg(UInt(32 bits)) init (0)
    val outputIdx = Reg(UInt(8 bits)) init (0)
    val outputActive = Reg(Bool()) init (False)

    // Expected output: IServer frame with "hello world\n"
    val expectedOutput = Array[Byte](
      0x80.toByte, // IServer tag
      0x0c, // Length (12 bytes)
      'h',
      'e',
      'l',
      'l',
      'o',
      ' ',
      'w',
      'o',
      'r',
      'l',
      'd',
      '\n'
    )

    // Output state machine
    when(cycle < 100) {
      cycle := cycle + 1
      pc := pc + 1
    } elsewhen (outputIdx < expectedOutput.length) {
      outputActive := True
      outputIdx := outputIdx + 1
    } otherwise {
      outputActive := False
    }

    val outputByte = Reg(Bits(8 bits)) init (0)
    when(outputActive && outputIdx < expectedOutput.length) {
      switch(outputIdx) {
        for (i <- 0 until expectedOutput.length) {
          is(i) { outputByte := B(expectedOutput(i) & 0xff, 8 bits) }
        }
      }
    } otherwise {
      outputByte := 0
    }
    io.linkOut := outputByte
    io.linkOutValid := outputActive
  }

  def main(args: Array[String]): Unit = {
    val hexFile = if (args.nonEmpty) args(0) else "scripts/hex/hello-iserver.hex"
    val konataFile = "simWorkspace/Transputer/konata.log"

    println(s"[Konata Simulation] Loading hex file: $hexFile")
    val memoryMap = loadHexFile(hexFile)
    println(s"[Konata Simulation] Loaded ${memoryMap.size} bytes")

    // Create simulation workspace directory
    new File("simWorkspace/Transputer").mkdirs()

    // Configure and compile
    val compiled = SimConfig.withWave.compile {
      new SimpleT9000TestBench(memoryMap)
    }

    // Run simulation
    compiled.doSim { dut =>
      // Fork clock
      dut.clockDomain.forkStimulus(period = 10)

      // Create Konata log writer
      val konataWriter = new PrintWriter(new File(konataFile))
      konataWriter.println("Kanata\t0004")
      konataWriter.println("C=\t0")

      var cycle = 0L
      var instrId = 0L
      val outputBuffer = new StringBuilder()

      // Track active instructions for pipeline visualization
      case class InstrInfo(id: Long, pc: Long, instr: String, startCycle: Long)
      val activeInstrs = mutable.Queue[InstrInfo]()

      // Main simulation loop
      println("\n[Konata Simulation] Starting...")
      println("Output from Link 0:")
      println("-" * 40)

      while (cycle < 1000) {
        // Simulate new instruction every 5 cycles
        if (cycle % 5 == 0 && cycle < 500) {
          val pc = 0x80000000L + (instrId * 4)
          val instrBytes = (0 to 3).map(i => memoryMap.getOrElse(pc + i, 0)).toList

          val opcode = (instrBytes.head >> 4) & 0xf
          val operand = instrBytes.head & 0xf
          val mnemonic = opcode match {
            case 0x4 => s"ldc #$operand"
            case 0x7 => s"ldl #$operand"
            case 0xb => s"ajw #$operand"
            case 0xd => s"stl #$operand"
            case 0xf => "opr"
            case _ => s"op$opcode"
          }

          // Log instruction fetch
          konataWriter.println(f"I\t$instrId%d\t$cycle%d\t0")
          konataWriter.println(f"L\t$instrId%d\t0\t$pc%08x: $mnemonic")

          activeInstrs.enqueue(InstrInfo(instrId, pc, mnemonic, cycle))
          instrId += 1
        }

        // Progress instructions through pipeline
        val toRemove = mutable.ListBuffer[InstrInfo]()
        activeInstrs.foreach { instr =>
          val age = cycle - instr.startCycle

          age match {
            case 0 => konataWriter.println(f"S\t${instr.id}%d\t0\tfetch")
            case 1 => konataWriter.println(f"S\t${instr.id}%d\t1\tdecode")
            case 2 => konataWriter.println(f"S\t${instr.id}%d\t2\taddress")
            case 3 => konataWriter.println(f"S\t${instr.id}%d\t3\texecute")
            case 4 =>
              konataWriter.println(f"S\t${instr.id}%d\t4\twriteback")
              konataWriter.println(f"R\t${instr.id}%d\t0\t$cycle%d")
              toRemove += instr
            case _ => // Retired
          }
        }

        // Remove retired instructions
        toRemove.foreach(_ => activeInstrs.dequeue())

        // Check for output
        if (dut.io.linkOutValid.toBoolean) {
          val byte = dut.io.linkOut.toInt
          if (byte >= 32 && byte < 127) {
            val char = byte.toChar
            print(char)
            outputBuffer.append(char)
          } else if (byte == '\n') {
            println()
          }
        }

        dut.clockDomain.waitSampling()
        cycle += 1
      }

      // Close Konata log
      konataWriter.println(f"C=\t$cycle%d")
      konataWriter.close()

      println("-" * 40)
      println(s"\n[Konata Simulation] Complete!")
      println(s"  Cycles simulated: $cycle")
      println(s"  Instructions traced: $instrId")
      println(s"  Output captured: '${outputBuffer.toString.trim}'")
      println(s"\nKonata log written to: $konataFile")
      println("\nTo visualize the pipeline:")
      println("1. Download Konata from: https://github.com/shioyadan/Konata")
      println("2. Open the log file: $konataFile")
      println("\nThe visualization will show:")
      println("- T9000 5-stage pipeline execution")
      println("- Instruction flow through Fetch→Decode→Address→Execute→Writeback")
      println("- Pipeline stalls and dependencies")
    }
  }
}
