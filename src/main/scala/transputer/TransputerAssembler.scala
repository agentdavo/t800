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
    // Basic operations (0x00-0x0F)
    "rev" -> 0x00, // reverse
    "lb" -> 0x01, // load byte
    "bsub" -> 0x02, // byte subscript
    "endp" -> 0x03, // end process
    "diff" -> 0x04, // difference
    "add" -> 0x05, // add
    "gcall" -> 0x06, // general call
    "in" -> 0x07, // input
    "prod" -> 0x08, // product
    "gt" -> 0x09, // greater than
    "wsub" -> 0x0a, // word subscript
    "out" -> 0x0b, // output
    "sub" -> 0x0c, // subtract
    "startp" -> 0x0d, // start process
    "outbyte" -> 0x0e, // output byte
    "outword" -> 0x0f, // output word

    // Extended operations (0x10-0x1F)
    "seterr" -> 0x10, // set error flag
    "resetch" -> 0x12, // reset channel
    "csub0" -> 0x13, // check subscript 0
    "stopp" -> 0x15, // stop process
    "ladd" -> 0x16, // long add
    "stlb" -> 0x17, // store low byte
    "sthf" -> 0x18, // store high priority front pointer
    "norm" -> 0x19, // normalize
    "ldiv" -> 0x1a, // long divide
    "ldpi" -> 0x1b, // load pointer to instruction
    "stlf" -> 0x1c, // store low priority front pointer
    "xdble" -> 0x1d, // extend to double
    "ldpri" -> 0x1e, // load current priority
    "rem" -> 0x1f, // remainder

    // Control operations (0x20-0x2F)
    "ret" -> 0x20, // return
    "lend" -> 0x21, // loop end
    "ldtimer" -> 0x22, // load timer
    "testlds" -> 0x23, // test load and set
    "testlde" -> 0x24, // test load end
    "testldd" -> 0x25, // test load double
    "teststs" -> 0x26, // test store test set
    "testste" -> 0x27, // test store test end
    "teststd" -> 0x28, // test store test double
    "testerr" -> 0x29, // test error flag
    "testpranal" -> 0x2a, // test processor analysing
    "tin" -> 0x2b, // timer input
    "div" -> 0x2c, // divide
    "testhardchan" -> 0x2d, // test hard channel
    "dist" -> 0x2e, // disable timer
    "disc" -> 0x2f, // disable channel

    // More operations (0x30-0x3F)
    "diss" -> 0x30, // disable skip
    "lmul" -> 0x31, // long multiply
    "not" -> 0x32, // bitwise not
    "xor" -> 0x33, // exclusive or
    "bcnt" -> 0x34, // byte count
    "lshr" -> 0x35, // long shift right
    "lshl" -> 0x36, // long shift left
    "lsum" -> 0x37, // long sum
    "lsub" -> 0x38, // long subtract
    "runp" -> 0x39, // run process
    "xword" -> 0x3a, // extend to word
    "sb" -> 0x3b, // store byte
    "gajw" -> 0x3c, // general adjust workspace
    "savel" -> 0x3d, // save low registers
    "saveh" -> 0x3e, // save high registers
    "wcnt" -> 0x3f, // word count

    // Shift and logic (0x40-0x4F)
    "shr" -> 0x40, // shift right
    "shl" -> 0x41, // shift left
    "mint" -> 0x42, // minimum integer
    "alt" -> 0x43, // alt start
    "altwt" -> 0x44, // alt wait
    "altend" -> 0x45, // alt end
    "and" -> 0x46, // bitwise and
    "enbt" -> 0x47, // enable timer
    "enbc" -> 0x48, // enable channel
    "enbs" -> 0x49, // enable skip
    "move" -> 0x4a, // move message
    "or" -> 0x4b, // bitwise or
    "csngl" -> 0x4c, // check single
    "ccnt1" -> 0x4d, // check count from 1
    "talt" -> 0x4e, // timer alt start
    "ldiff" -> 0x4f, // long difference

    // More operations (0x50-0x5F)
    "sthb" -> 0x50, // store high byte
    "taltwt" -> 0x51, // timer alt wait
    "sum" -> 0x52, // sum
    "mul" -> 0x53, // multiply
    "sttimer" -> 0x54, // set timer
    "stoperr" -> 0x55, // stop on error
    "cword" -> 0x56, // check word
    "clrhalterr" -> 0x57, // clear halt on error
    "sethalterr" -> 0x58, // set halt on error
    "testhalterr" -> 0x59, // test halt on error
    "dup" -> 0x5a, // duplicate
    "move2dinit" -> 0x5b, // 2D block move init
    "move2dall" -> 0x5c, // 2D block move all
    "move2dnonzero" -> 0x5d, // 2D block move non-zero
    "move2dzero" -> 0x5e, // 2D block move zero
    "gtu" -> 0x5f, // greater than unsigned

    // Extended operations (0x60-0x6F)
    "unpacksn" -> 0x63, // unpack single length fp number
    "postnormsn" -> 0x6c, // post-normalize correction of single length fp number
    "roundsn" -> 0x6d, // round single length fp number
    "pack" -> 0x6e, // pack
    "ldinf" -> 0x71, // load single length infinity
    "fmul" -> 0x72, // fast multiply
    "cflerr" -> 0x73, // check floating point error
    "crcword" -> 0x74, // calculate CRC on word
    "crcbyte" -> 0x75, // calculate CRC on byte
    "bitcnt" -> 0x76, // count bits set in word
    "bitrevword" -> 0x77, // reverse bits in word
    "bitrevnbits" -> 0x78, // reverse bottom n bits
    "pop" -> 0x79, // pop integer stack
    "settimeslice" -> 0x7b, // set timeslicing status

    // More operations (0x80-0x8F)
    "ldflags" -> 0x84, // load value of StatusReg
    "stflags" -> 0x85, // store value into StatusReg
    "xbword" -> 0x86, // sign extend byte to word
    "lbx" -> 0x87, // load byte and sign extend
    "cb" -> 0x88, // check byte
    "cbu" -> 0x89, // check byte unsigned
    "insphdr" -> 0x8c, // insert packet header
    "readbfr" -> 0x8d, // read buffer
    "ldconf" -> 0x8e, // load configuration
    "stconf" -> 0x8f, // store configuration
    "ldcnt" -> 0x90, // load count
    "ssub" -> 0x91, // sixteen subscript
    "ldth" -> 0x92, // load trap handler
    "ldchstatus" -> 0x93, // load channel status
    "intdis" -> 0x94, // interrupt disable
    "intenb" -> 0x95, // interrupt enable
    "ldtrapped" -> 0x96, // load trapped process
    "cir" -> 0x97, // check in range
    "ss" -> 0x98, // store sixteen
    "chantype" -> 0x99, // channel type
    "ls" -> 0x9a, // load sixteen
    "fpseterr" -> 0x9b, // set FP error flag
    "fptesterr" -> 0x9c, // test FP error flag
    "fppop" -> 0x9d, // pop floating point stack
    "ciru" -> 0x9f, // check in range unsigned

    // Device operations (0xF0-0xF7)
    "devlb" -> 0xf0, // device load byte
    "devsb" -> 0xf1, // device store byte
    "devls" -> 0xf2, // device load sixteen
    "devss" -> 0xf3, // device store sixteen
    "devlw" -> 0xf4, // device load word
    "devsw" -> 0xf5, // device store word
    "xsword" -> 0xf6, // sign extend sixteen to word
    "lsx" -> 0xf7, // load sixteen and sign extend
    "cs" -> 0xf8, // check sixteen
    "csu" -> 0xf9, // check sixteen unsigned
    "vin" -> 0xfa, // virtual in
    "vout" -> 0xfb, // virtual out
    "swapbfr" -> 0xfc, // swap buffer
    "swapt" -> 0xfd, // swap timer
    "terminate" -> 0x2ff // terminate - special encoding
  )

  case class AssemblyLine(
    address: Int,
    label: Option[String],
    instruction: Option[String],
    operand: Option[String],
    comment: Option[String]
  )

  // Negative operations (prefix with 0x02, 0x60)
  val negativeOps = Map(
    "fpstall" -> 0x00, // FPU store all
    "fpldall" -> 0x01, // FPU load all
    "stshadow" -> 0x02, // store shadow registers
    "ldshadow" -> 0x03, // load shadow registers
    "tret" -> 0x05, // trap return
    "goprot" -> 0x06, // go protected
    "selth" -> 0x07, // select trap handler
    "syscall" -> 0x08, // system call
    "swapgstatus" -> 0x0a, // swap G and GStatus
    "swaptimer" -> 0x0b, // swap timer priority
    "insertqueue" -> 0x0c, // insert at queue head
    "timeslice" -> 0x0d, // timeslice
    "signal" -> 0x0e, // signal on semaphore
    "wait" -> 0x0f, // wait on semaphore
    "trapenb" -> 0x10, // trap enable
    "trapdis" -> 0x11, // trap disable
    "trcenb" -> 0x12, // trace enable
    "trcdis" -> 0x13, // trace disable
    "cbctl" -> 0x14, // control block operation
    "mrg" -> 0x17, // merge
    "swapbfr" -> 0x18, // swap buffers
    "sethdr" -> 0x19, // set header
    "setchmode" -> 0x1a, // set channel mode
    "initvlcb" -> 0x1b, // initialize virtual link control block
    "writehdr" -> 0x1c, // write header
    "readhdr" -> 0x1d, // read header
    "disg" -> 0x1e, // disable guards
    "enbg" -> 0x1f, // enable guards
    "grant" -> 0x20, // grant resource
    "stmove2dinit" -> 0x21, // store 2D block move parameters and initialize
    "causeerror" -> 0x22, // cause error
    "unmkrc" -> 0x23, // unmark resource channel
    "mkrc" -> 0x24, // mark resource channel
    "irdsq" -> 0x25, // insert ready to deferred queue
    "erdsq" -> 0x26, // enable ready to scheduled queue
    "stresptr" -> 0x27, // store resource pointer
    "ldresptr" -> 0x28, // load resource pointer
    "devmove" -> 0x2a, // device move
    "icl" -> 0x2b, // invalidate cache line
    "fdcl" -> 0x2c, // flush and disable cache line
    "ica" -> 0x2d, // invalidate cache all
    "fdca" -> 0x2e, // flush and disable cache all
    "nop" -> 0x2f, // no operation
    "ldmemstartval" -> 0x30, // load value of MemStart
    "pop" -> 0x31, // pop processor stack
    "lddevid" -> 0x32, // load device identity
    "antiwdesc" -> 0x35, // anti-write descriptor
    "antiwd" -> 0x36, // anti-write data
    "swapqueue" -> 0x37, // swap scheduler queue
    "swapcr" -> 0x38, // swap ClockReg
    "jalterb" -> 0x39, // jump alternation branch
    "enbc3" -> 0x3a, // enable channel word 3
    "getpri" -> 0x3b, // get priority
    "getaff" -> 0x3c, // get affinity
    "setaff" -> 0x3d, // set affinity
    "stime" -> 0x3e, // store time
    "sethpri" -> 0x40, // set high priority
    "setnpri" -> 0x41, // set normal priority
    "ldclock" -> 0x42, // load clock
    "stclock" -> 0x43, // store clock
    "getpas" -> 0x44, // get priority and swap
    "antialtwt" -> 0x45, // anti-alt wait
    "alt3enb" -> 0x46, // alt 3 enable
    "alt3dis" -> 0x47, // alt 3 disable
    "alt4enb" -> 0x48, // alt 4 enable
    "alt4dis" -> 0x49, // alt 4 disable
    "alt5enb" -> 0x4a, // alt 5 enable
    "alt5dis" -> 0x4b, // alt 5 disable
    "alt6enb" -> 0x4c, // alt 6 enable
    "alt6dis" -> 0x4d, // alt 6 disable
    "clockenb" -> 0x4e, // clock interrupt enable
    "clockdis" -> 0x4f, // clock interrupt disable
    "reboot" -> 0x50 // reboot
  )

  // FPU extended operations (prefix with 0x20000)
  // These are T9000 extended FPU operations that require special encoding
  // They are NOT the same as the regular FPU operations in fpuOps
  val fpuExtendedOps = Map[String, Int](
    // Currently empty - T9000 extended FPU operations would go here
    // Most FPU operations use standard encoding through fpuOps
  )

  // Standard FPU operations (prefix with 0x8xxx)
  val fpuOps = Map(
    "fpldnldbi" -> 0x8a, // FP load non-local double indirect
    "fpchkerr" -> 0x8b, // check FP error
    "fpstnldb" -> 0x8c, // FP store non-local double
    "fpldnlsni" -> 0x8e, // FP load non-local single indirect
    "fpadd" -> 0x8f, // FP add
    "fpstnlsn" -> 0x90, // FP store non-local single
    "fpsub" -> 0x91, // FP subtract
    "fpldnldb" -> 0x92, // FP load non-local double
    "fpmul" -> 0x93, // FP multiply
    "fpdiv" -> 0x94, // FP divide
    "fprange" -> 0x9a, // FP range check
    "fpldnlsn" -> 0x9b, // FP load non-local single
    "fpldnlmuldb" -> 0x9c, // FP load non-local multiple double
    "fpldnladddb" -> 0x9e, // FP load non-local add double
    "fpldnlmulsn" -> 0x9f, // FP load non-local multiple single
    "fpentry" -> 0xa0, // FP unit entry
    "fpldnladdsn" -> 0xa2, // FP load non-local add single
    "fpusqrtfirst" -> 0xa3, // FP square root first step
    "fpusqrtstep" -> 0xa4, // FP square root step
    "fpusqrtlast" -> 0xa5, // FP square root last step
    "fprem" -> 0xa6, // FP remainder
    "fpnan" -> 0xa7, // FP not a number
    "fpordered" -> 0xa8, // FP ordered
    "fpnotfinite" -> 0xa9, // FP not finite
    "fpgt" -> 0xaa, // FP greater than
    "fpeq" -> 0xab, // FP equal
    "fpi32tor32" -> 0xac, // FP int32 to real32
    "fpge" -> 0xc5, // FP greater or equal
    "fpi32tor64" -> 0xc6, // FP int32 to real64
    "fpb32tor64" -> 0xc7, // FP real32 to real64
    "fprtoi32" -> 0xc9, // FP real to int32
    "fpr32tor64" -> 0xca, // FP real32 to real64
    "fpr64toi32" -> 0xcb, // FP real64 to int32
    "fpexpdec32" -> 0xcc, // FP exponent decrease 32
    "fpr64tor32" -> 0xcd, // FP real64 to real32
    "fptesterr" -> 0xce, // FP test error flag
    "fpdup" -> 0xcf, // FP duplicate
    "fprev" -> 0xd0, // FP reverse
    "fpldnladddb" -> 0xd2, // FP load non-local add double
    "fpldnlmuldb" -> 0xd3, // FP load non-local multiply double
    "fpldnladdsn" -> 0xd5, // FP load non-local add single
    "fpentry3" -> 0xd6, // FP entry 3
    "fpldnlmulsn" -> 0xd7, // FP load non-local multiply single
    "fpuseterr" -> 0x200, // FP set error (special encoding)
    "fpuclrerr" -> 0x201 // FP clear error (special encoding)
  )

  // Additional T9000 instructions that don't fit in other categories
  val specialOps = Map(
    "restart" -> (0x2600 + 0x00), // restart
    "getpatch" -> (0x2600 + 0x05), // get patch
    "testchan" -> (0x2600 + 0x06), // test channel
    "subcarry" -> (0x2600 + 0x07), // subtract with carry
    "addc" -> (0x2600 + 0x08), // add with carry
    "subc" -> (0x2600 + 0x09), // subtract with carry
    "ci" -> (0x2600 + 0x10), // check in
    "dw" -> (0x2600 + 0x11), // data word directive
    "fpint" -> (0x2600 + 0x12), // FP integer
    "ldab" -> (0x2600 + 0x13), // load and add byte
    "ldaddw" -> (0x2600 + 0x14), // load and add word
    "fprtoi64" -> (0x2600 + 0x15), // FP real to int64
    "fpsttest" -> (0x2600 + 0x17), // FP status test
    "wsubword" -> (0x2600 + 0x18) // word subscript word
  )

  // Track VAL definitions (constants)
  val valDefinitions = mutable.Map[String, Int]()

  // Track EQU definitions (INMOS style constants)
  val equDefinitions = mutable.Map[String, Int]()

  // Current ORG address
  var orgAddress = 0x80000000 // Default T9000 start address

  def parseAssembly(code: String): List[AssemblyLine] = {
    var currentAddress = orgAddress
    val result = mutable.ListBuffer[AssemblyLine]()

    code.split("\n").foreach { line =>
      val trimmed = line.trim
      // Handle both ; and -- comments
      if (!trimmed.isEmpty && !trimmed.startsWith(";") && !trimmed.startsWith("--")) {
        val parsed = parseAssemblyLine(trimmed, currentAddress)

        // Update currentAddress if ORG changed it
        if (parsed.address != currentAddress) {
          currentAddress = parsed.address
        }

        if (parsed.instruction.isDefined) {
          currentAddress += estimateInstructionSize(parsed.instruction.get, parsed.operand)
        } else if (parsed.label.isDefined) {
          // Just a label, no address increment
        }

        result += parsed
      }
    }

    result.toList
  }

  def estimateInstructionSize(instr: String, operand: Option[String]): Int = {
    instr.toLowerCase match {
      case "db" | "byte" =>
        operand.map(data => parseDataBytes(data, 0, Map.empty)).map(_.length).getOrElse(1)
      case "align" =>
        4 // Assume align to 4 bytes
      case "global" =>
        0 // No code generated
      case op if negativeOps.contains(op) =>
        // Negative operations are at least 3 bytes
        val negOp = negativeOps(op)
        if (negOp > 15) 4 else 3
      case op if fpuExtendedOps.contains(op) =>
        // FPU extended operations are 6-7 bytes
        val extOp = fpuExtendedOps(op)
        if (extOp > 15) 7 else 6
      case op if fpuOps.contains(op) =>
        // Standard FPU operations
        val fpOp = fpuOps(op)
        if (fpOp <= 15) 1
        else if (fpOp <= 0xff) 2
        else 3 // For extended encodings like 0x200
      case op if secondaryOps.contains(op) =>
        val secOp = secondaryOps(op)
        if (secOp <= 15) 1
        else if (secOp <= 0xff) 2
        else 3 // For extended encodings like terminate (0x2ff)
      case op if opcodes.contains(op) =>
        // Primary opcodes with operands
        if (operand.isEmpty) 1
        else {
          val value = operand.map(parseOperand(_, 0, Map.empty)).getOrElse(0)
          if (value >= 0 && value <= 15) 1
          else if (value >= -16 && value <= -1) 1 // nfix covers negative immediates
          else 4 // Conservative estimate for prefixed values
        }
      case _ =>
        // Unknown instruction, conservative estimate
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

    // Handle INMOS directives
    if (codePart.startsWith(".")) {
      val directive = codePart.toUpperCase
      directive match {
        case ".TRANSPUTER" =>
          // INMOS transputer mode - already default
          return AssemblyLine(address, None, None, None, comment)
        case d if d.startsWith(".CODE") || d.startsWith(".TEXT") =>
          // Code section - already default
          return AssemblyLine(address, None, None, None, comment)
        case _ =>
          // Ignore other directives for now
          return AssemblyLine(address, None, None, None, comment)
      }
    }

    // Handle TITLE directive
    if (codePart.toUpperCase.startsWith("TITLE")) {
      // Just a comment, ignore
      return AssemblyLine(address, None, None, None, comment)
    }

    // Handle PAGE directive
    if (codePart.toUpperCase.startsWith("PAGE")) {
      // Formatting directive, ignore
      return AssemblyLine(address, None, None, None, comment)
    }

    // Handle END directive
    if (codePart.toUpperCase.startsWith("END")) {
      // End of assembly, ignore
      return AssemblyLine(address, None, None, None, comment)
    }

    // Handle ORG directive
    if (codePart.toUpperCase.startsWith("ORG")) {
      val orgPattern = """ORG\s+(.+)$""".r
      codePart match {
        case orgPattern(addr) =>
          val newAddress = parseOperand(addr.trim, address, Map.empty)
          orgAddress = newAddress
          return AssemblyLine(newAddress, None, None, None, comment)
        case _ =>
          println(s"Warning: Invalid ORG directive: $codePart")
      }
    }

    // Handle EQU definitions (INMOS style)
    if (codePart.contains("EQU")) {
      val equPattern = """(\w+)\s+EQU\s+(.+)$""".r
      codePart match {
        case equPattern(name, value) =>
          val intValue = parseOperand(value.trim, address, Map.empty)
          equDefinitions(name) = intValue
          valDefinitions(name) = intValue // Also store in VAL for compatibility
          return AssemblyLine(address, None, None, None, comment)
        case _ =>
          println(s"Warning: Invalid EQU declaration: $codePart")
      }
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
          return AssemblyLine(address, None, None, None, comment) // No code generated
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
    var currentAddress = orgAddress

    // First pass: collect labels and calculate addresses
    for (line <- lines) {
      if (line.address != currentAddress) {
        // ORG directive changed address
        currentAddress = line.address
      }
      line.label.foreach(label => labels(label) = currentAddress)
      if (line.instruction.isDefined) {
        val estimatedSize = estimateInstructionSize(line.instruction.get, line.operand)
        currentAddress += estimatedSize
      }
    }

    // Now resolve any forward-referenced EQU expressions
    for ((name, expr) <- equDefinitions if expr < 0) {
      // Negative values might indicate unresolved expressions
      // Try to resolve them now that we have all labels
      // This is a simplified approach - a full assembler would need proper expression evaluation
    }

    // Second pass: generate binary
    currentAddress = orgAddress
    for (line <- lines) {
      if (line.address != currentAddress) {
        // ORG directive changed address
        currentAddress = line.address
      }
      line.label.foreach(label => {
        // Update label if address changed
        if (labels(label) != currentAddress) {
          println(
            s"Warning: Label $label address adjusted from 0x${labels(label).toHexString} to 0x${currentAddress.toHexString}"
          )
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
      val operandBits = operand & 0xffffffff // Handle as unsigned

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
          value = (~operand) & 0xffffffff // Two's complement
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
        } else if (secOp <= 0xff) {
          // Need prefix for secondary operation
          encodeInstruction(0x2, secOp >> 4) ++ encodeInstruction(0xf, secOp & 0xf)
        } else {
          // Special multi-byte encoding for extended operations like terminate (0x2ff)
          val prefixes = mutable.ListBuffer[Byte]()
          var value = secOp
          while (value > 15) {
            prefixes += ((0x2 << 4) | (value & 0xf)).toByte
            value >>= 4
          }
          prefixes.toList :+ ((0xf << 4) | (value & 0xf)).toByte
        }

      case op if negativeOps.contains(op) =>
        // Negative operations: pfix #2, pfix #6, opr #operand
        val negOp = negativeOps(op)
        List(
          ((0x2 << 4) | 0x2).toByte, // pfix #2
          ((0x2 << 4) | 0x6).toByte, // pfix #6
          ((0xf << 4) | (negOp & 0xf)).toByte // opr with low nibble
        ) ++ (if (negOp > 15) List(((0x2 << 4) | (negOp >> 4)).toByte) else Nil)

      case op if fpuOps.contains(op) =>
        // Standard FPU operations with 0x8xxx encoding
        val fpOp = fpuOps(op)
        if (fpOp <= 0xff) {
          // Simple FPU operation
          encodeInstruction(0x2, fpOp >> 4) ++ encodeInstruction(0xf, fpOp & 0xf)
        } else {
          // Extended FPU operation (e.g., 0x200)
          val prefixes = mutable.ListBuffer[Byte]()
          var value = fpOp
          while (value > 15) {
            prefixes += ((0x2 << 4) | (value & 0xf)).toByte
            value >>= 4
          }
          prefixes.toList :+ ((0xf << 4) | (value & 0xf)).toByte
        }

      case op if fpuExtendedOps.contains(op) =>
        // FPU extended operations use encoding 0x20000 + operand
        // This encodes as the prefix sequence for 0x20000 followed by opr with the specific operand
        val extOp = fpuExtendedOps(op)
        // 0x20000 = 0b10_0000_0000_0000_0000
        // Encoded as: pfix 0, pfix 0, pfix 0, pfix 0, pfix 2, opr operand
        val prefixes = List(
          ((0x2 << 4) | 0x0).toByte, // pfix #0
          ((0x2 << 4) | 0x0).toByte, // pfix #0
          ((0x2 << 4) | 0x0).toByte, // pfix #0
          ((0x2 << 4) | 0x0).toByte, // pfix #0
          ((0x2 << 4) | 0x2).toByte // pfix #2
        )
        if (extOp <= 15) {
          prefixes :+ ((0xf << 4) | extOp).toByte // opr with operand
        } else {
          // Need additional prefix for operand > 15
          prefixes ++ encodeInstruction(0x2, extOp >> 4) :+ ((0xf << 4) | (extOp & 0xf)).toByte
        }

      case op if specialOps.contains(op) =>
        // Special T9000 operations with 0x2600 + operand encoding
        val specOp = specialOps(op)
        val operand = specOp & 0xff
        val prefix = (specOp >> 8) & 0xff
        // Encode as pfix #2, pfix #6, opr #operand
        List(
          ((0x2 << 4) | ((prefix >> 4) & 0xf)).toByte, // pfix high nibble
          ((0x2 << 4) | (prefix & 0xf)).toByte, // pfix low nibble
          ((0xf << 4) | (operand & 0xf)).toByte // opr with operand
        ) ++ (if (operand > 15) encodeInstruction(0x2, operand >> 4) else Nil)

      case "db" | "byte" | "DB" =>
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
      case "INITIAL_ADJUSTMENT" => 20 // Common T9000 constant
      case "_Start" => 0x80000000 // Common start address
      case name if valDefinitions.contains(name) => valDefinitions(name) // VAL definitions
      case name if equDefinitions.contains(name) => equDefinitions(name) // EQU definitions
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
          labels(trimmed) - address - 1 // T9000 uses PC-relative addressing
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

      val leftVal =
        if (labels.contains(left)) labels(left)
        else if (left == "Addr0") address // Special case for current address
        else if (valDefinitions.contains(left)) valDefinitions(left)
        else if (equDefinitions.contains(left)) equDefinitions(left)
        else
          try {
            if (left.startsWith("0x")) java.lang.Long.parseLong(left.substring(2), 16).toInt
            else left.toInt
          } catch {
            case _: NumberFormatException =>
              println(s"Warning: Unknown operand '$left'")
              0
          }

      val rightVal =
        if (labels.contains(right)) labels(right)
        else if (right == "Addr0") address
        else if (valDefinitions.contains(right)) valDefinitions(right)
        else if (equDefinitions.contains(right)) equDefinitions(right)
        else
          try {
            if (right.startsWith("0x")) java.lang.Long.parseLong(right.substring(2), 16).toInt
            else right.toInt
          } catch {
            case _: NumberFormatException =>
              println(s"Warning: Unknown operand '$right'")
              0
          }

      return leftVal - rightVal
    }

    // Handle addition
    if (trimmed.contains("+")) {
      val idx = trimmed.indexOf("+")
      val left = trimmed.substring(0, idx).trim
      val right = trimmed.substring(idx + 1).trim

      val leftVal =
        if (labels.contains(left)) labels(left)
        else if (valDefinitions.contains(left)) valDefinitions(left)
        else if (equDefinitions.contains(left)) equDefinitions(left)
        else
          try {
            if (left.startsWith("0x")) java.lang.Long.parseLong(left.substring(2), 16).toInt
            else left.toInt
          } catch {
            case _: NumberFormatException =>
              println(s"Warning: Unknown operand '$left'")
              0
          }

      val rightVal =
        if (labels.contains(right)) labels(right)
        else if (valDefinitions.contains(right)) valDefinitions(right)
        else if (equDefinitions.contains(right)) equDefinitions(right)
        else
          try {
            if (right.startsWith("0x")) java.lang.Long.parseLong(right.substring(2), 16).toInt
            else right.toInt
          } catch {
            case _: NumberFormatException =>
              println(s"Warning: Unknown operand '$right'")
              0
          }

      return leftVal + rightVal
    }

    // No operator found - this is a simple value
    // Don't call parseOperand here as it would lead to infinite recursion
    if (labels.contains(trimmed)) {
      labels(trimmed)
    } else {
      // Try parsing as number
      try {
        if (trimmed.startsWith("0x")) {
          java.lang.Long.parseLong(trimmed.substring(2), 16).toInt
        } else {
          trimmed.toInt
        }
      } catch {
        case _: NumberFormatException =>
          println(s"Warning: Unknown expression term '$trimmed'")
          0
      }
    }
  }

  def parseDataBytes(
    data: String,
    currentAddress: Int = 0,
    labels: Map[String, Int] = Map.empty
  ): List[Byte] = {
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
        val addr = (baseAddress + (lineNum * 16)) & 0xffffffff
        val addrHigh = (addr >> 16) & 0xffff
        val addrLow = addr & 0xffff

        // Extended Linear Address Record if needed
        if (lineNum == 0 && addrHigh != 0) {
          val extAddrChecksum =
            (0x100 - ((0x02 + 0x00 + 0x00 + 0x04 + (addrHigh >> 8) + (addrHigh & 0xff)) & 0xff)) & 0xff
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
      val asmCode =
        try source.mkString
        finally source.close()

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
      println(
        "  sbt \"runMain transputer.TransputerAssembler --hello\"    (generate hello world ROM)"
      )
      println(
        "  sbt \"runMain transputer.TransputerAssembler --bootload\" (assemble INMOS bootloader)"
      )
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
        val outputFile =
          if (args.length > 1) args(1)
          else {
            val baseName = new java.io.File(inputFile).getName
            baseName.replaceAll("\\.asm$", "") + ".hex"
          }
        assembleFile(inputFile, outputFile)
    }
  }
}
