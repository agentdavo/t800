package transputer

import scala.collection.mutable
import java.io.{FileWriter, PrintWriter}

/** Simple transputer assembler to convert assembly to binary for boot ROM.
  *
  * Supports basic transputer instructions and the hello world boot code.
  */
object TransputerAssembler {

  // Transputer instruction opcodes (primary operations)
  val opcodes = Map(
    "j" -> 0x0, // jump
    "ldlp" -> 0x1, // load local pointer
    "pfix" -> 0x2, // prefix
    "ldnl" -> 0x3, // load non-local
    "ldc" -> 0x4, // load constant
    "ldnlp" -> 0x5, // load non-local pointer
    "nfix" -> 0x6, // negative prefix
    "ldl" -> 0x7, // load local
    "adc" -> 0x8, // add constant
    "call" -> 0x9, // call
    "cj" -> 0xa, // conditional jump
    "ajw" -> 0xb, // adjust workspace
    "eqc" -> 0xc, // equals constant
    "stl" -> 0xd, // store local
    "stnl" -> 0xe, // store non-local
    "opr" -> 0xf // operate (secondary operations)
  )

  // Secondary operations (when primary opcode is 0xF)
  val secondaryOps = Map(
    "rev" -> 0x00, // reverse
    "lb" -> 0x01, // load byte
    "bsub" -> 0x02, // byte subscript
    "endp" -> 0x03, // end process
    "diff" -> 0x04, // difference
    "add" -> 0x05, // add
    "dup" -> 0x06, // duplicate
    "gcall" -> 0x07, // general call
    "in" -> 0x08, // input
    "prod" -> 0x09, // product
    "gt" -> 0x0a, // greater than
    "wsub" -> 0x0b, // word subscript
    "out" -> 0x0c, // output
    "sub" -> 0x0d, // subtract
    "startp" -> 0x0e, // start process
    "outbyte" -> 0x0f, // output byte
    "outword" -> 0x10, // output word
    "seterr" -> 0x11, // set error flag
    "mint" -> 0x42, // minimum integer
    "sthf" -> 0x18, // set high priority queue front
    "stlf" -> 0x19, // set low priority queue front
    "sttimer" -> 0x54, // set timer
    "testerr" -> 0x29, // test error flag
    "clrhalterr" -> 0x57, // clear halt on error
    "fptesterr" -> 0x5e, // floating point test error
    "ldpi" -> 0x1b, // load pointer to instruction
    "gajw" -> 0x3c, // general adjust workspace
    "ret" -> 0x20, // return
    "lend" -> 0x21, // loop end
    "ldtimer" -> 0x22, // load timer
    "pop" -> 0x79, // pop processor stack
    "terminate" -> 0x71 // terminate
  )

  case class AssemblyLine(
    address: Int,
    label: Option[String],
    instruction: Option[String],
    operand: Option[String],
    comment: Option[String]
  )

  def parseAssembly(code: String): List[AssemblyLine] = {
    val lines = code
      .split("\n")
      .zipWithIndex
      .map { case (line, idx) =>
        val trimmed = line.trim
        if (trimmed.isEmpty || trimmed.startsWith(";")) {
          AssemblyLine(idx * 4, None, None, None, Some(trimmed))
        } else {
          parseAssemblyLine(trimmed, idx * 4)
        }
      }
      .toList
    lines
  }

  def parseAssemblyLine(line: String, address: Int): AssemblyLine = {
    val commentIdx = line.indexOf(';')
    val (codePart, comment) = if (commentIdx >= 0) {
      (line.substring(0, commentIdx).trim, Some(line.substring(commentIdx).trim))
    } else {
      (line, None)
    }

    if (codePart.isEmpty) {
      return AssemblyLine(address, None, None, None, comment)
    }

    // Check for label (ends with :)
    if (codePart.endsWith(":")) {
      val label = codePart.dropRight(1).trim
      return AssemblyLine(address, Some(label), None, None, comment)
    }

    // Parse instruction and operand
    val parts = codePart.split("\\s+", 2)
    val instruction = parts(0).toLowerCase
    val operand = if (parts.length > 1) Some(parts(1)) else None

    AssemblyLine(address, None, Some(instruction), operand, comment)
  }

  def assemble(lines: List[AssemblyLine]): List[Byte] = {
    val labels = mutable.Map[String, Int]()
    val binary = mutable.ListBuffer[Byte]()
    var currentAddress = 0

    // First pass: collect labels
    for (line <- lines) {
      line.label.foreach(label => labels(label) = currentAddress)
      if (line.instruction.isDefined) {
        currentAddress += 4 // Each instruction is 4 bytes for simplicity
      }
    }

    // Second pass: generate binary
    currentAddress = 0
    for (line <- lines) {
      line.instruction.foreach { instr =>
        val bytes = assembleInstruction(instr, line.operand, currentAddress, labels.toMap)
        binary ++= bytes
        currentAddress += bytes.length
      }
    }

    binary.toList
  }

  def assembleInstruction(
    instr: String,
    operand: Option[String],
    address: Int,
    labels: Map[String, Int]
  ): List[Byte] = {
    def encodeInstruction(opcode: Int, operand: Int): List[Byte] = {
      if (operand >= 0 && operand <= 15) {
        // Direct encoding
        List(((opcode << 4) | operand).toByte)
      } else {
        // Need prefix
        val prefixes = mutable.ListBuffer[Byte]()
        var value = if (operand < 0) -operand else operand
        val prefixOp = if (operand < 0) 0x6 else 0x2 // nfix or pfix

        // Generate prefix bytes
        while (value > 15) {
          prefixes += ((prefixOp << 4) | (value & 0xf)).toByte
          value >>= 4
        }

        prefixes.toList :+ ((opcode << 4) | (value & 0xf)).toByte
      }
    }

    // Handle different instruction types
    instr match {
      case op if opcodes.contains(op) =>
        val opcode = opcodes(op)
        val operandValue = operand.map(parseOperand(_, address, labels)).getOrElse(0)
        encodeInstruction(opcode, operandValue)

      case op if secondaryOps.contains(op) =>
        val secOp = secondaryOps(op)
        if (secOp <= 15) {
          encodeInstruction(0xf, secOp)
        } else {
          // Need prefix for secondary operation
          encodeInstruction(0x2, secOp >> 4) ++ encodeInstruction(0xf, secOp & 0xf)
        }

      case "db" =>
        // Data bytes
        operand.map(parseDataBytes).getOrElse(Nil)

      case _ =>
        println(s"Unknown instruction: $instr")
        List(0x00.toByte) // NOP
    }
  }

  def parseOperand(operand: String, address: Int, labels: Map[String, Int]): Int = {
    operand.trim match {
      case hex if hex.startsWith("0x") =>
        try {
          java.lang.Long.parseLong(hex.substring(2), 16).toInt
        } catch {
          case _: NumberFormatException => 0
        }
      case dec if dec.forall(c => c.isDigit || c == '-') =>
        try {
          dec.toInt
        } catch {
          case _: NumberFormatException => 0
        }
      case label if labels.contains(label) => labels(label) - address - 4 // Relative jump
      case expr if expr.contains("-") || expr.contains("+") =>
        // Simple expression evaluation
        parseExpression(expr, address, labels)
      case _ => 0
    }
  }

  def parseExpression(expr: String, address: Int, labels: Map[String, Int]): Int = {
    // Simple expression parser for "label - offset" etc.
    if (expr.contains("-")) {
      val parts = expr.split("-").map(_.trim)
      if (parts.length == 2) {
        val left =
          if (labels.contains(parts(0))) labels(parts(0))
          else parseOperand(parts(0), address, labels)
        val right = parseOperand(parts(1), address, labels)
        return left - right
      }
    }
    0
  }

  def parseDataBytes(data: String): List[Byte] = {
    if (data.startsWith("\"") && data.endsWith("\"")) {
      // String literal
      val str = data.substring(1, data.length - 1)
      str.getBytes("ASCII").toList
    } else {
      // Hex bytes
      data
        .split(",")
        .map(_.trim)
        .filter(_.nonEmpty)
        .map { hex =>
          if (hex.startsWith("0x")) {
            Integer.parseInt(hex.substring(2), 16).toByte
          } else {
            hex.toInt.toByte
          }
        }
        .toList
    }
  }

  def generateHexFile(binary: List[Byte], filename: String, baseAddress: Int = 0x80000000): Unit = {
    val writer = new PrintWriter(new FileWriter(filename))
    try {
      // Generate Intel HEX format
      binary.grouped(16).zipWithIndex.foreach { case (chunk, lineNum) =>
        val addr = baseAddress + (lineNum * 16)
        val dataHex = chunk.map(b => f"${b & 0xff}%02X").mkString
        val checksum = (0x100 - ((chunk.length + ((addr >> 8) & 0xff) + (addr & 0xff) + chunk
          .map(_ & 0xff)
          .sum) & 0xff)) & 0xff
        writer.println(f":${chunk.length}%02X${addr & 0xffff}%04X00$dataHex$checksum%02X")
      }
      writer.println(":00000001FF") // End of file record
    } finally {
      writer.close()
    }
  }

  def assembleHelloWorld(): Unit = {
    val helloWorldAsm = """
; Transputer Hello World Boot ROM
CodeStart:
    mint                        ; load A with NotProcess.p = mint = 0x80000000
    sthf                        ; initialize high priority queue front pointer
    mint                        ; load A with NotProcess.p
    stlf                        ; initialize low priority process queue pointer

    mint                        ; load A with NotProcess.p
    ldc     0x80000024          ; load high priority timer queue pointer offset
    stnl    0                   ; initialize high priority timer queue
    mint                        ; load A with NotProcess.p
    ldc     0x80000028          ; load low priority timer queue pointer offset
    stnl    0                   ; initialize high priority timer queue

    ldc     0                   ; load start time
    sttimer                     ; start the clocks

    testerr                     ; clears the Error flag and HaltOnError flag
    clrhalterr                  ; clear halt on error

    fptesterr                   ; Reset floating point error flag

    ; Initialize all link channels to NotProcess.p
    mint
    ldc     0x80000020          ; Event channel
    stnl    0
    mint
    ldc     0x8000001C          ; Link 3 input
    stnl    0
    mint
    ldc     0x80000018          ; Link 2 input
    stnl    0
    mint
    ldc     0x80000014          ; Link 1 input
    stnl    0
    mint
    ldc     0x80000010          ; Link 0 input
    stnl    0
    mint
    ldc     0x8000000C          ; Link 3 output
    stnl    0
    mint
    ldc     0x80000008          ; Link 2 output
    stnl    0
    mint
    ldc     0x80000004          ; Link 1 output
    stnl    0
    mint
    ldc     0x80000000          ; Link 0 output
    stnl    0

    j       MAIN

HWSTR:
    db      "Hello T9000 World!", 0x00

MAIN:
    ajw     0x100               ; allow for stack frames
    ldc     HWSTR
    call    putConsolePString
    terminate

putConsolePString:
    ; Simple console output for now
    ; In full implementation, would use IServer protocol
    ret

; Reset vector - must be at 0x7FFFFFFE
    j       CodeStart
"""

    val lines = parseAssembly(helloWorldAsm)
    val binary = assemble(lines)
    generateHexFile(binary, "hello_world_rom.hex")

    println(s"Generated hello_world_rom.hex with ${binary.length} bytes")
    println("Boot ROM ready for T9000!")
  }

  def main(args: Array[String]): Unit = {
    assembleHelloWorld()
  }
}
