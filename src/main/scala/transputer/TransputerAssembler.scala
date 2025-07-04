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
    "sethalterr" -> 0x58, // set halt on error
    "fptesterr" -> 0x9c, // floating point test error
    "ldpi" -> 0x1b, // load pointer to instruction
    "gajw" -> 0x3c, // general adjust workspace
    "ret" -> 0x20, // return
    "lend" -> 0x21, // loop end
    "ldtimer" -> 0x22, // load timer
    "pop" -> 0x79, // pop processor stack
    "terminate" -> 0x2f, // terminate
    "xor" -> 0x33, // exclusive or
    "bcnt" -> 0x34, // byte count
    "wcnt" -> 0x3f, // word count
    "or" -> 0x4b, // bitwise or
    "and" -> 0x46, // bitwise and
    "not" -> 0x32, // bitwise not
    "shl" -> 0x41, // shift left
    "shr" -> 0x40, // shift right
    "ladd" -> 0x16, // long add
    "lsub" -> 0x38, // long subtract
    "lsum" -> 0x52, // long sum
    "ldiff" -> 0x4f, // long difference
    "lmul" -> 0x31, // long multiply
    "ldiv" -> 0x1a, // long divide
    "rem" -> 0x1f, // remainder
    "xdble" -> 0x1d, // extend to double
    "norm" -> 0x1c // normalize
  )

  case class AssemblyLine(
    address: Int,
    label: Option[String],
    instruction: Option[String],
    operand: Option[String],
    comment: Option[String]
  )

  // Track VAL definitions (constants)
  val valDefinitions = mutable.Map[String, Int]()

  def parseAssembly(code: String): List[AssemblyLine] = {
    var currentAddress = 0
    val lines = code
      .split("\n")
      .zipWithIndex
      .flatMap { case (line, idx) =>
        val trimmed = line.trim
        // Handle both ; and -- comments
        if (trimmed.isEmpty || trimmed.startsWith(";") || trimmed.startsWith("--")) {
          None // Skip comments and empty lines
        } else {
          val parsed = parseAssemblyLine(trimmed, currentAddress)
          if (parsed.instruction.isDefined) {
            currentAddress += estimateInstructionSize(parsed.instruction.get, parsed.operand)
          }
          Some(parsed)
        }
      }
      .toList
    lines
  }

  def estimateInstructionSize(instr: String, operand: Option[String]): Int = {
    instr match {
      case "db" | "byte" => 
        operand.map(data => parseDataBytes(data, 0, Map.empty)).map(_.length).getOrElse(1)
      case "align" => 
        4 // Assume align to 4 bytes
      case "global" =>
        0 // No code generated
      case _ =>
        // Most instructions are 1-4 bytes
        4
    }
  }

  def parseAssemblyLine(line: String, address: Int): AssemblyLine = {
    // Handle both ; and -- comments
    val commentIdx = math.min(
      if (line.contains(";")) line.indexOf(';') else Int.MaxValue,
      if (line.contains("--")) line.indexOf("--") else Int.MaxValue
    )
    val (codePart, comment) = if (commentIdx < Int.MaxValue) {
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
    // Parse VAL definitions
    if (codePart.toUpperCase.startsWith("VAL")) {
      // VAL NAME IS VALUE : comment
      val valPattern = """VAL\s+(\w+)\s+IS\s+([^:]+)(?::.*)?$""".r
      codePart match {
        case valPattern(name, value) =>
          val intValue = parseOperand(value.trim, address, Map.empty)
          valDefinitions(name) = intValue
          return AssemblyLine(address, None, None, None, comment)  // No code generated
        case _ =>
          println(s"Warning: Invalid VAL declaration: $codePart")
      }
    }
    
    val tokens = codePart.split("\\s+", 2)
    val (instruction, operand) = if (tokens.length >= 2) {
      (tokens(0).toLowerCase, Some(tokens(1).trim))
    } else {
      (tokens(0).toLowerCase, None)
    }

    AssemblyLine(address, None, Some(instruction), operand, comment)
  }

  def assemble(lines: List[AssemblyLine]): List[Byte] = {
    val labels = mutable.Map[String, Int]()
    val binary = mutable.ListBuffer[Byte]()
    var currentAddress = 0x80000000  // T9000 default start address

    // First pass: collect labels and calculate addresses
    for (line <- lines) {
      line.label.foreach(label => labels(label) = currentAddress)
      if (line.instruction.isDefined) {
        val estimatedSize = estimateInstructionSize(line.instruction.get, line.operand)
        currentAddress += estimatedSize
      }
    }

    // Second pass: generate binary
    currentAddress = 0x80000000
    for (line <- lines) {
      line.label.foreach(label => {
        // Update label if address changed
        if (labels(label) != currentAddress) {
          println(s"Warning: Label $label address adjusted from 0x${labels(label).toHexString} to 0x${currentAddress.toHexString}")
          labels(label) = currentAddress
        }
      })
      
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
      val operandBits = operand & 0xFFFFFFFF // Handle as unsigned
      
      if (operandBits >= 0 && operandBits <= 15) {
        // Direct encoding
        List(((opcode << 4) | operandBits).toByte)
      } else {
        // Need prefix
        val prefixes = mutable.ListBuffer[Byte]()
        var value = operandBits
        
        // For negative values, use nfix
        val isNegative = operand < 0
        if (isNegative) {
          value = (~operand) & 0xFFFFFFFF  // Two's complement
        }
        
        val prefixOp = if (isNegative) 0x6 else 0x2 // nfix or pfix

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

      case "db" | "byte" =>
        // Data bytes
        operand.map(data => parseDataBytes(data, address, labels)).getOrElse(Nil)

      case "align" =>
        // Align to word boundary (4 bytes)
        val currentAddr = address
        val padding = (4 - (currentAddr % 4)) % 4
        List.fill(padding)(0x00.toByte)

      case "global" =>
        // Global label declaration - no code generated
        Nil

      case "val" =>
        // Value declaration - no code generated
        Nil

      case _ =>
        println(s"Warning: Unknown instruction '$instr' at address 0x${address.toHexString}")
        List(0x00.toByte) // NOP
    }
  }

  def parseOperand(operand: String, address: Int, labels: Map[String, Int]): Int = {
    val trimmed = operand.trim
    
    // Handle special constants first
    trimmed match {
      case "INITIAL_ADJUSTMENT" => 20  // Common T9000 constant
      case "_Start" => 0x80000000  // Common start address
      case name if valDefinitions.contains(name) => valDefinitions(name)  // VAL definitions
      case _ =>
        // Try hex numbers (0x... or #...)
        if (trimmed.startsWith("0x")) {
          try {
            java.lang.Long.parseLong(trimmed.substring(2), 16).toInt
          } catch {
            case _: NumberFormatException => 0
          }
        } else if (trimmed.startsWith("#")) {
          try {
            java.lang.Long.parseLong(trimmed.substring(1), 16).toInt
          } catch {
            case _: NumberFormatException => 0
          }
        } else if (trimmed.forall(c => c.isDigit || c == '-')) {
          // Decimal number
          try {
            trimmed.toInt
          } catch {
            case _: NumberFormatException => 0
          }
        } else if (labels.contains(trimmed)) {
          // Label reference - calculate relative offset
          labels(trimmed) - address - 1  // T9000 uses PC-relative addressing
        } else if (trimmed.contains("-") || trimmed.contains("+")) {
          // Expression
          parseExpression(trimmed, address, labels)
        } else {
          // Unknown - might be a forward reference
          println(s"Warning: Unknown operand '$trimmed' at address 0x${address.toHexString}")
          0
        }
    }
  }

  def parseExpression(expr: String, address: Int, labels: Map[String, Int]): Int = {
    // Simple expression parser for "label - label" or "label + offset" etc.
    val trimmed = expr.trim
    
    // Handle subtraction
    if (trimmed.contains("-") && !trimmed.startsWith("-")) {
      val idx = trimmed.indexOf("-")
      val left = trimmed.substring(0, idx).trim
      val right = trimmed.substring(idx + 1).trim
      
      val leftVal = if (labels.contains(left)) labels(left)
                    else if (left == "Addr0") address  // Special case for current address
                    else parseOperand(left, address, Map.empty)  // Avoid recursion
      
      val rightVal = if (labels.contains(right)) labels(right)
                     else if (right == "Addr0") address
                     else parseOperand(right, address, Map.empty)
      
      return leftVal - rightVal
    }
    
    // Handle addition
    if (trimmed.contains("+")) {
      val idx = trimmed.indexOf("+")
      val left = trimmed.substring(0, idx).trim
      val right = trimmed.substring(idx + 1).trim
      
      val leftVal = if (labels.contains(left)) labels(left)
                    else parseOperand(left, address, Map.empty)
      
      val rightVal = if (labels.contains(right)) labels(right)
                     else parseOperand(right, address, Map.empty)
      
      return leftVal + rightVal
    }
    
    // No operator, try as simple operand
    parseOperand(trimmed, address, Map.empty)
  }

  def parseDataBytes(data: String, currentAddress: Int = 0, labels: Map[String, Int] = Map.empty): List[Byte] = {
    // Handle expressions like (Endprimary-Primary)
    if (data.startsWith("(") && data.endsWith(")")) {
      val expr = data.substring(1, data.length - 1)
      val value = parseExpression(expr, currentAddress, labels)
      return List(value.toByte)
    }
    
    // Handle mixed format with strings and hex values
    val parts = data.split(",").map(_.trim).filter(_.nonEmpty)
    parts.flatMap { part =>
      if (part.startsWith("\"") && part.endsWith("\"")) {
        // String literal
        val str = part.substring(1, part.length - 1)
        str.getBytes("ASCII").toList
      } else if (part.startsWith("0x")) {
        // Hex value
        List(Integer.parseInt(part.substring(2), 16).toByte)
      } else if (part.startsWith("#")) {
        // Hex value with # prefix
        List(Integer.parseInt(part.substring(1), 16).toByte)
      } else if (part.forall(c => c.isDigit || c == '-')) {
        // Decimal value
        List(part.toInt.toByte)
      } else {
        // Expression or label
        val value = parseOperand(part, currentAddress, labels)
        List(value.toByte)
      }
    }.toList
  }

  def generateHexFile(binary: List[Byte], filename: String, baseAddress: Int = 0x80000000): Unit = {
    import java.io.File
    
    // Ensure scripts/hex directory exists
    val hexDir = new File("scripts/hex")
    if (!hexDir.exists()) {
      hexDir.mkdirs()
    }
    
    val fullPath = s"scripts/hex/$filename"
    val writer = new PrintWriter(new FileWriter(fullPath))
    try {
      // Generate Intel HEX format
      binary.grouped(16).zipWithIndex.foreach { case (chunk, lineNum) =>
        val addr = (baseAddress + (lineNum * 16)) & 0xFFFFFFFF
        val addrHigh = (addr >> 16) & 0xFFFF
        val addrLow = addr & 0xFFFF
        
        // Extended Linear Address Record if needed
        if (lineNum == 0 && addrHigh != 0) {
          val extAddrChecksum = (0x100 - ((0x02 + 0x00 + 0x00 + 0x04 + (addrHigh >> 8) + (addrHigh & 0xFF)) & 0xFF)) & 0xFF
          writer.println(f":02000004${addrHigh}%04X${extAddrChecksum}%02X")
        }
        
        val dataHex = chunk.map(b => f"${b & 0xff}%02X").mkString
        val checksum = (0x100 - ((chunk.length + ((addrLow >> 8) & 0xff) + (addrLow & 0xff) + chunk
          .map(_ & 0xff)
          .sum) & 0xff)) & 0xff
        writer.println(f":${chunk.length}%02X${addrLow}%04X00$dataHex$checksum%02X")
      }
      writer.println(":00000001FF") // End of file record
    } finally {
      writer.close()
    }
    println(s"Generated hex file: $fullPath")
  }

  def assembleFile(inputFile: String, outputFile: String): Unit = {
    import scala.io.Source
    
    try {
      val source = Source.fromFile(inputFile)
      val asmCode = try source.mkString finally source.close()
      
      val lines = parseAssembly(asmCode)
      val binary = assemble(lines)
      generateHexFile(binary, outputFile)
      
      println(s"Successfully assembled $inputFile")
      println(s"  Output: scripts/hex/$outputFile")
      println(s"  Size: ${binary.length} bytes")
    } catch {
      case e: Exception =>
        println(s"Error assembling $inputFile: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def assembleBootloader(): Unit = {
    assembleFile("scripts/asm/bootload.asm", "bootload.hex")
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

    println(s"Generated scripts/hex/hello_world_rom.hex with ${binary.length} bytes")
    println("Boot ROM ready for T9000!")
  }

  // Instruction trace support
  case class InstructionTrace(
    iptr: Long,
    bytes: List[Byte],
    mnemonic: String,
    operand: String,
    areg: Long = 0,
    breg: Long = 0,
    creg: Long = 0,
    wptr: Long = 0,
    wptrData: Long = 0
  ) {
    def format: String = {
      val bytesStr = bytes.map(b => f"${b & 0xff}%02X").mkString(" ")
      val codeField = f"$bytesStr%-30s"
      val mnemonicField = f"$mnemonic%-8s $operand%-12s"
      f"${iptr}%08X: $codeField$mnemonicField--   ${areg}%08X ${breg}%08X ${creg}%08X   ${wptr}%08X ${wptrData}%08X"
    }
  }

  def generateTraceHeader(): String = {
    "-IPtr------Code-----------------------Mnemonic------------HE---AReg-----BReg-----CReg-------WPtr-----WPtr[0]-"
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Transputer Assembler")
      println("Usage:")
      println("  sbt \"runMain transputer.TransputerAssembler <input.asm> [output.hex]\"")
      println("  sbt \"runMain transputer.TransputerAssembler --hello\"    (generate hello world ROM)")
      println("  sbt \"runMain transputer.TransputerAssembler --bootload\" (assemble INMOS bootloader)")
      println()
      println("Files are written to scripts/hex/ directory")
      println()
      println("To test assembled code:")
      println("  sbt \"runMain transputer.GenerateWithTest --hex scripts/hex/output.hex --wave\"")
      return
    }

    args(0) match {
      case "--hello" => assembleHelloWorld()
      case "--bootload" => assembleBootloader()
      case inputFile =>
        val outputFile = if (args.length > 1) args(1) 
                        else {
                          val baseName = new java.io.File(inputFile).getName
                          baseName.replaceAll("\\.asm$", "") + ".hex"
                        }
        assembleFile(inputFile, outputFile)
    }
  }
}
