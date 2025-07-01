package transputer

import spinal.core._

/** Primary and secondary opcode definitions for the Transputer ISA, aligned with IMS T9000
  * Instruction Set Manual (Appendix B).
  */

object Opcode {

  /** Primary functions (Table B.1), 4-bit opcodes. */
  object Primary {
    def J = B(0x0, 4 bits) // Jump to relative address
    def LDLP = B(0x1, 4 bits) // Load local pointer
    def PFIX = B(0x2, 4 bits) // Prefix (extend operand or opcode)
    def LDNL = B(0x3, 4 bits) // Load non-local
    def LDC = B(0x4, 4 bits) // Load constant
    def LDNLP = B(0x5, 4 bits) // Load non-local pointer
    def NFIX = B(0x6, 4 bits) // Negative prefix
    def LDL = B(0x7, 4 bits) // Load local
    def ADC = B(0x8, 4 bits) // Add constant
    def CALL = B(0x9, 4 bits) // Call subroutine
    def CJ = B(0xa, 4 bits) // Conditional jump
    def AJW = B(0xb, 4 bits) // Adjust workspace
    def EQC = B(0xc, 4 bits) // Equals constant
    def STL = B(0xd, 4 bits) // Store local
    def STNL = B(0xe, 4 bits) // Store non-local
    def OPR = B(0xf, 4 bits) // Operate (execute secondary instruction)
  }

  /** Secondary functions organized by prefix usage (Tables B.2–B.4). */
  object Secondary {

    /** Instructions encoded without prefix (Table B.2), 8-bit opcodes via OPR (0xF). */

    object NoPrefix {
      // Arithmetic and Stack Operations
      def REV = B(0x00, 8 bits) // Reverse top of stack
      def ADD = B(0x05, 8 bits) // Add Areg + Breg, store in Areg
      def DIFF = B(0x04, 8 bits) // Difference Areg - Breg
      def PROD = B(0x08, 8 bits) // Product Areg * Breg
      def SUB = B(0x0c, 8 bits) // Subtract Areg - Breg
      def GT = B(0x09, 8 bits) // Greater than (Areg > Breg)

      // Memory Operations
      def LB = B(0x01, 8 bits) // Load byte
      def BSUB = B(0x02, 8 bits) // Byte subscript
      def WSUB = B(0x0a, 8 bits) // Word subscript

      // Communication
      def IN = B(0x07, 8 bits) // Input message
      def OUT = B(0x0b, 8 bits) // Output message
      def OUTBYTE = B(0x0e, 8 bits) // Output byte
      def OUTWORD = B(0x0f, 8 bits) // Output word

      // Process Control
      def ENDP = B(0x03, 8 bits) // End process
      def STARTP = B(0x0d, 8 bits) // Start process
      def GCALL = B(0x06, 8 bits) // General call
    }

    /** Instructions encoded with PFIX prefix (Table B.3), 8-bit opcodes via OPR (0xF). */

    object Prefix {
      // Arithmetic and Stack Operations
      def LADD = B(0x16, 8 bits) // Long add
      def NORM = B(0x19, 8 bits) // Normalize
      def LDIV = B(0x1a, 8 bits) // Long divide
      def XDBLE = B(0x1d, 8 bits) // Extend to double
      def REM = B(0x1f, 8 bits) // Remainder
      def DIV = B(0x2c, 8 bits) // Divide
      def LMUL = B(0x31, 8 bits) // Long multiply
      def NOT = B(0x32, 8 bits) // Bitwise not
      def XOR = B(0x33, 8 bits) // Exclusive or
      def LSHR = B(0x35, 8 bits) // Long shift right
      def LSHL = B(0x36, 8 bits) // Long shift left
      def LSUM = B(0x37, 8 bits) // Long sum
      def LSUB = B(0x38, 8 bits) // Long subtract
      def SHR = B(0x40, 8 bits) // Shift right
      def SHL = B(0x41, 8 bits) // Shift left
      def AND = B(0x46, 8 bits) // Bitwise and
      def OR = B(0x4b, 8 bits) // Bitwise or
      def SUM = B(0x52, 8 bits) // Sum
      def MUL = B(0x53, 8 bits) // Multiply
      def DUP = B(0x5a, 8 bits) // Duplicate top of stack
      def GTU = B(0x5f, 8 bits) // Unsigned greater than
      def LDIFF = B(0x4f, 8 bits) // Long difference
      def MINT = B(0x42, 8 bits) // Minimum integer
      def CSNGL = B(0x4c, 8 bits) // Check single
      def CCNT = B(0x4d, 8 bits) // Check count from 1

      // Memory Operations
      def STLB = B(0x17, 8 bits) // Store local byte
      def STHF = B(0x18, 8 bits) // Store high-priority front pointer
      def LDPI = B(0x1b, 8 bits) // Load pointer to instruction
      def STLF = B(0x1c, 8 bits) // Store low-priority front pointer
      def LDPRI = B(0x1e, 8 bits) // Load current priority
      def STHB = B(0x50, 8 bits) // Store high-priority back pointer
      def CWORD = B(0x56, 8 bits) // Check word
      def BCNT = B(0x34, 8 bits) // Byte count
      def WCNT = B(0x3f, 8 bits) // Word count
      def XWORD = B(0x3a, 8 bits) // Sign extend to word
      def SB = B(0x3b, 8 bits) // Store byte
      def SAVEL = B(0x3d, 8 bits) // Save low
      def SAVEH = B(0x3e, 8 bits) // Save high
      def MOVE = B(0x4a, 8 bits) // Move message
      def WSUBDB = B(0x81, 8 bits) // Form double word subscript
      def XBWORD = B(0xb8, 8 bits) // Extend byte to word
      def LBX = B(0xb9, 8 bits) // Load byte extended
      def CB = B(0xba, 8 bits) // Check byte
      def CBU = B(0xbb, 8 bits) // Check byte unsigned
      def LDCNT = B(0xc0, 8 bits) // Load message byte count
      def SSUB = B(0xc1, 8 bits) // 16-bit word subscript
      def CIR = B(0xc7, 8 bits) // Check in range
      def SS = B(0xc8, 8 bits) // Store 16-bit word
      def CIRU = B(0xcc, 8 bits) // Check in range unsigned
      def XSWORD = B(0xf8, 8 bits) // Sign extend 16-bit word
      def LSX = B(0xf9, 8 bits) // Load sixteen and sign extend
      def CS = B(0xfa, 8 bits) // Check 16-bit word
      def CSU = B(0xfb, 8 bits) // Check 16-bit word unsigned

      // Process Control
      def RET = B(0x20, 8 bits) // Return from subroutine
      def LEND = B(0x21, 8 bits) // Loop end
      def RUNP = B(0x39, 8 bits) // Run process
      def STOPP = B(0x15, 8 bits) // Stop process
      def GAJW = B(0x3c, 8 bits) // General adjust workspace
      def ALT = B(0x43, 8 bits) // Alt start
      def ALTWT = B(0x44, 8 bits) // Alt wait
      def ALTEND = B(0x45, 8 bits) // Alt end
      def TALT = B(0x4e, 8 bits) // Timer alt start
      def TALTWT = B(0x51, 8 bits) // Timer alt wait
      def ENBT = B(0x47, 8 bits) // Enable timer
      def ENBC = B(0x48, 8 bits) // Enable channel
      def ENBS = B(0x49, 8 bits) // Enable skip
      def DIST = B(0x2e, 8 bits) // Disable timer
      def DISC = B(0x2f, 8 bits) // Disable channel
      def DISS = B(0x30, 8 bits) // Disable skip
      def RESETCH = B(0x12, 8 bits) // Reset channel
      def CSUB = B(0x13, 8 bits) // Check subscript from 0
      def INSPHDR = B(0xbc, 8 bits) // Inspect header
      def READBFR = B(0xbd, 8 bits) // Read buffer pointer from VLCB
      def CHANTYPE = B(0xc9, 8 bits) // Channel type

      // Timer Operations
      def LDTIMER = B(0x22, 8 bits) // Load timer
      def TIN = B(0x2b, 8 bits) // Timer input
      def STTIMER = B(0x54, 8 bits) // Store timer
      def TIMERDISABLEH = B(0x7a, 8 bits) // Disable high-priority timer
      def TIMERDISABLEL = B(0x7b, 8 bits) // Disable low-priority timer
      def TIMERENABLEH = B(0x7c, 8 bits) // Enable high-priority timer
      def TIMERENABLEL = B(0x7d, 8 bits) // Enable low-priority timer

      // Error and Trap Handling
      def TESTERR = B(0x29, 8 bits) // Test error flag
      def TESTPRANAL = B(0x2a, 8 bits) // Test processor analyzing
      def STOPERR = B(0x55, 8 bits) // Stop on error
      def CLRHALTERR = B(0x57, 8 bits) // Clear halt-on-error
      def SETHALTERR = B(0x58, 8 bits) // Set halt-on-error
      def TESTHALTERR = B(0x59, 8 bits) // Test halt-on-error
      def LDFLAGS = B(0xb6, 8 bits) // Load error flags
      def STFLAGS = B(0xb7, 8 bits) // Store error flags

      // Floating-Point Operations
      def FMUL = B(0x72, 8 bits) // Fractional multiply
      def FPLDNLDBI = B(0x82, 8 bits) // FP load non-local indexed double
      // def FPSTNLDB = B(0x84, 8 bits) // FP store non-local double - conflicts with T9000 LDPRODID
      def FPLDNLSNI = B(0x86, 8 bits) // FP load non-local indexed single
      def FPADD = B(0x87, 8 bits) // FP add
      def FPSTNLSN = B(0x88, 8 bits) // FP store non-local single
      def FPSUB = B(0x89, 8 bits) // FP subtract
      def FPLDNLDB = B(0x8a, 8 bits) // FP load non-local double
      def FPMUL = B(0x8b, 8 bits) // FP multiply
      def FPDIV = B(0x8c, 8 bits) // FP divide
      def FPRANGE = B(0x8d, 8 bits) // FP range reduce
      def FPLDNLSN = B(0x8e, 8 bits) // FP load non-local single
      def FPNAN = B(0x91, 8 bits) // FP NaN check
      def FPORDERED = B(0x92, 8 bits) // FP orderability
      def FPNOTFINITE = B(0x93, 8 bits) // FP not finite
      def FPGT = B(0x94, 8 bits) // FP greater than
      def FPEQ = B(0x95, 8 bits) // FP equality
      def FPI32TOR32 = B(0x96, 8 bits) // INT32 to REAL32
      def FPGE = B(0x97, 8 bits) // FP greater than or equals
      def FPI32TOR64 = B(0x98, 8 bits) // INT32 to REAL64
      def FPB32TOR64 = B(0x9a, 8 bits) // BIT32 to REAL64
      def FPLG = B(0x9b, 8 bits) // FP less than or greater than
      def FPRTOI32 = B(0x9d, 8 bits) // REAL to INT32
      def FPSTNLI32 = B(0x9e, 8 bits) // FP store non-local INT32
      def FPLDZEROSN = B(0x9f, 8 bits) // Load zero single
      def FPLDZERODB = B(0xa0, 8 bits) // Load zero double
      def FPINT = B(0xa1, 8 bits) // Round to floating integer
      def FPDUP = B(0xa3, 8 bits) // FP duplicate
      def FPREV = B(0xa4, 8 bits) // FP reverse
      def FPLDNLADDDB = B(0xa6, 8 bits) // FP load non-local and add double
      def FPLDNLMULDB = B(0xa8, 8 bits) // FP load non-local and multiply double
      def FPLDNLADDSN = B(0xaa, 8 bits) // FP load non-local and add single
      def FPLDNLMULSN = B(0xac, 8 bits) // FP load non-local and multiply single
      def FPREM = B(0xcf, 8 bits) // FP remainder
      def FPRN = B(0xd0, 8 bits) // Rounding mode to round nearest
      def FPDIVBY2 = B(0xd1, 8 bits) // FP divide by 2.0
      def FPMULBY2 = B(0xd2, 8 bits) // FP multiply by 2.0
      def FPSQRT = B(0xd3, 8 bits) // FP square root
      def FPRP = B(0xd4, 8 bits) // Rounding mode to round plus
      def FPRM = B(0xd5, 8 bits) // Rounding mode to round minus
      def FPRZ = B(0xd6, 8 bits) // Rounding mode to round zero
      def FPR32TOR64 = B(0xd7, 8 bits) // REAL32 to REAL64
      def FPR64TOR32 = B(0xd8, 8 bits) // REAL64 to REAL32
      def FPEXPDEC32 = B(0xd9, 8 bits) // FP divide by 2^32
      def FPEXPINC32 = B(0xda, 8 bits) // FP multiply by 2^32
      def FPABS = B(0xdb, 8 bits) // FP absolute
      def FPCHKI32 = B(0xdc, 8 bits) // FP check in range of INT32
      def FPCHKI64 = B(0xdd, 8 bits) // FP check in range of INT64

      // Bit Operations
      def CRCWORD = B(0x74, 8 bits) // Calculate CRC on word
      def CRCBYTE = B(0x75, 8 bits) // Calculate CRC on byte
      def BITCNT = B(0x76, 8 bits) // Count bits set in word
      def BITREVWORD = B(0x77, 8 bits) // Reverse bits in word
      def BITREVNBITS = B(0x78, 8 bits) // Reverse bottom n bits in word

      // Block Move
      def MOVE2DINIT = B(0x5b, 8 bits) // Initialize data for 2D block move
      def MOVE2DALL = B(0x5c, 8 bits) // 2D block copy
      def MOVE2DNONZERO = B(0x5d, 8 bits) // 2D block copy non-zero bytes
      def MOVE2DZERO = B(0x5e, 8 bits) // 2D block copy zero bytes

      // Configuration and Status
      def LDCONF = B(0xbe, 8 bits) // Load from configuration register
      def STCONF = B(0xbf, 8 bits) // Store to configuration register
      def LDTH = B(0xc2, 8 bits) // Load trap handler
      def LDCHSTATUS = B(0xc3, 8 bits) // Load channel status

      // Interrupts
      def INTDIS = B(0xc4, 8 bits) // Interrupt disable
      def INTENB = B(0xc5, 8 bits) // Interrupt enable

      // Device Operations
      def DEVLB = B(0xf0, 8 bits) // Device load byte
      def DEVSB = B(0xf1, 8 bits) // Device store byte
      def DEVLS = B(0xf2, 8 bits) // Device load sixteen
      def DEVSS = B(0xf3, 8 bits) // Device store sixteen
      def DEVLW = B(0xf4, 8 bits) // Device load word
      def DEVSW = B(0xf5, 8 bits) // Device store word

      // Miscellaneous
      def POP = B(0x79, 8 bits) // Pop processor stack
      def LDMEMSTARTVAL = B(0x7e, 8 bits) // Load value of memstart address
    }

    /** Instructions encoded with NFIX prefix (Table B.4), 8-bit opcodes via OPR (0xF). */

    object NegPrefix {
      // Floating-Point Operations
      def FPSTALL = B(0x01, 8 bits) // FP store all (negative prefix: -0x01)
      def FPLDALL = B(0x02, 8 bits) // FP load all (-0x02)
      def FPENTRY = B(0xab, 8 bits) // FP entry point (-0x55)

      // Process and Trap Handling
      def STSHADOW = B(0x03, 8 bits) // Store shadow registers (-0x03)
      def LDSHADOW = B(0x04, 8 bits) // Load shadow registers (-0x04)
      def TRET = B(0x05, 8 bits) // Trap return (-0x05)
      def GOPROT = B(0x06, 8 bits) // Go protected (-0x06)
      def SELTH = B(0x07, 8 bits) // Select trap handler (-0x07)
      def SYSCALL = B(0x08, 8 bits) // System call (-0x08)
      def WAIT = B(0x0b, 8 bits) // Wait (-0x0B)
      def SIGNAL = B(0x0c, 8 bits) // Signal (-0x0C)
      def TIMESLICE = B(0x0d, 8 bits) // Timeslice (-0x0D)
      def STOPCH = B(0x12, 8 bits) // Stop virtual channel (-0x12)

      // Scheduling
      def INSERTQUEUE = B(0x0e, 8 bits) // Insert at front of scheduler queue (-0x0E)
      def SWAPQUEUE = B(0x10, 8 bits) // Swap scheduler queue (-0x10)
      def SWAPTIMER = B(0x0f, 8 bits) // Swap timer queue (-0x0F)

      // Communication
      def VOUT = B(0x13, 8 bits) // Variable-length output message (-0x13)
      def VIN = B(0x14, 8 bits) // Variable-length input message (-0x14)
      def SWAPBFR = B(0x17, 8 bits) // Swap buffer pointer in VLCB (-0x17)
      def SETHDR = B(0x18, 8 bits) // Set virtual channel header (-0x18)
      def SETCHMODE = B(0x19, 8 bits) // Set channel mode (-0x19)
      def INITVLCB = B(0x1a, 8 bits) // Initialize VLCB (-0x1A)
      def WRITEHDR = B(0x1b, 8 bits) // Write virtual channel header (-0x1B)
      def READHDR = B(0x1c, 8 bits) // Read virtual channel header (-0x1C)

      // Resource Management
      def DISG = B(0x1d, 8 bits) // Disable grant (-0x1D)
      def ENBG = B(0x1e, 8 bits) // Enable grant (-0x1E)
      def GRANT = B(0x1f, 8 bits) // Grant resource (-0x1F)
      def UNMKRC = B(0x23, 8 bits) // Unmark resource channel (-0x23)
      def MKRC = B(0x24, 8 bits) // Mark resource channel (-0x24)
      def IRDSQ = B(0x25, 8 bits) // Insert at front of RDS queue (-0x25)
      def ERDSQ = B(0x26, 8 bits) // Empty RDS queue (-0x26)
      def STRESPTR = B(0x27, 8 bits) // Store resource queue pointer (-0x27)
      def LDRESPTR = B(0x28, 8 bits) // Load resource queue pointer (-0x28)

      // Cache Operations
      def ICL = B(0x2d, 8 bits) // Invalidate cache line (-0x2D)
      def FDCL = B(0x2e, 8 bits) // Flush dirty cache line (-0x2E)
      def ICA = B(0x2f, 8 bits) // Invalidate cache address (-0x2F)
      def FDCA = B(0x30, 8 bits) // Flush dirty cache address (-0x30)

      // Device Operations
      // def DEVMOVE = B(0x2c, 8 bits) // Device move (-0x2C) - conflicts with DIV

      // Miscellaneous
      def STMOVE2DINIT = B(0x20, 8 bits) // Store move2dinit data (-0x20)
      def CAUSEERROR = B(0x21, 8 bits) // Cause error (-0x21)
      def NOP = B(0x40, 8 bits) // No operation (-0x40)
      def LDPRODID = B(0x84, 8 bits) // Load product identity (-0x84)
    }
  }

  /** SpinalEnum variants for plugin use, aligned with hardware decoding. */

  object PrimaryOpcode extends SpinalEnum(binarySequential) {
    val J, LDLP, PFIX, LDNL, LDC, LDNLP, NFIX, LDL, ADC, CALL, CJ, AJW, EQC, STL, STNL, OPR =
      newElement()
  }

  object SecondaryOpcode extends SpinalEnum {

    // format: off
  
    // No Prefix (Table B.2)
    val REV, LB, BSUB, ENDP, DIFF, ADD, GCALL, IN, PROD, GT, WSUB, OUT, SUB, STARTP, OUTBYTE, OUTWORD,
	
    // Prefix (Table B.3)
    RESETCH, CSUB, STOPP, LADD, STLB, STHF, NORM, LDIV, LDPI, STLF, XDBLE, LDPRI, REM, RET, LEND, LDTIMER,
    TESTERR, TIN, DIV, DIST, DISC, DISS, LMUL, NOT, XOR, BCNT, LSHR, LSHL, LSUM, LSUB, RUNP, XWORD, SB, GAJW,
    SAVEL, SAVEH, WCNT, SHR, SHL, MINT, ALT, ALTWT, ALTEND, AND, ENBT, ENBC, ENBS, MOVE, OR, CSNGL, CCNT,
    TALT, LDIFF, STHB, TALTWT, SUM, MUL, STTIMER, STOPERR, CWORD, CLRHALTERR, SETHALTERR, TESTHALTERR, TESTPRANAL, DUP,
    MOVE2DINIT, MOVE2DALL, MOVE2DNONZERO, MOVE2DZERO, GTU, FMUL, CRCWORD, CRCBYTE, BITCNT, BITREVWORD,
    BITREVNBITS, POP, TIMERDISABLEH, TIMERDISABLEL, TIMERENABLEH, TIMERENABLEL, LDMEMSTARTVAL, WSUBDB,
    FPLDNLDBI, FPSTNLDB, FPLDNLSNI, FPADD, FPSTNLSN, FPSUB, FPLDNLDB, FPMUL, FPDIV, FPRANGE, FPLDNLSN,
    FPNAN, FPORDERED, FPNOTFINITE, FPGT, FPEQ, FPI32TOR32, FPGE, FPI32TOR64, FPB32TOR64, FPLG, FPRTOI32,
    FPSTNLI32, FPLDZEROSN, FPLDZERODB, FPINT, FPDUP, FPREV, FPLDNLADDDB, FPLDNLMULDB, FPLDNLADDSN,
    FPLDNLMULSN, LDFLAGS, STFLAGS, XBWORD, LBX, CB, CBU, INSPHDR, READBFR, LDCONF, STCONF, LDCNT, SSUB,
    LDTH, LDCHSTATUS, INTDIS, INTENB, CIR, SS, CHANTYPE, CIRU, FPREM, FPRN, FPDIVBY2, FPMULBY2, FPSQRT,
    FPRP, FPRM, FPRZ, FPR32TOR64, FPR64TOR32, FPEXPDEC32, FPEXPINC32, FPABS, FPCHKI32, FPCHKI64, DEVLB, DEVSB, DEVLS, DEVSS,
    DEVLW, DEVSW, XSWORD, LSX, CS, CSU,
	
    // Negative Prefix (Table B.4)
    FPSTALL, FPLDALL, STSHADOW, LDSHADOW, TRET, GOPROT, SELTH, SYSCALL, WAIT, SIGNAL, TIMESLICE, INSERTQUEUE,
    SWAPTIMER, SWAPQUEUE, STOPCH, VOUT, VIN, SWAPBFR, SETHDR, SETCHMODE, INITVLCB, WRITEHDR, READHDR,
    DISG, ENBG, GRANT, STMOVE2DINIT, CAUSEERROR, UNMKRC, MKRC, IRDSQ, ERDSQ, STRESPTR, LDRESPTR, DEVMOVE,
    ICL, FDCL, ICA, FDCA, NOP, LDPRODID = newElement()

    // format: on

    defaultEncoding = SpinalEnumEncoding("static")(
      // No Prefix (Table B.2)
      REV -> 0x00,
      LB -> 0x01,
      BSUB -> 0x02,
      ENDP -> 0x03,
      DIFF -> 0x04,
      ADD -> 0x05,
      GCALL -> 0x06,
      IN -> 0x07,
      PROD -> 0x08,
      GT -> 0x09,
      WSUB -> 0x0a,
      OUT -> 0x0b,
      SUB -> 0x0c,
      STARTP -> 0x0d,
      OUTBYTE -> 0x0e,
      OUTWORD -> 0x0f,

      // Prefix (Table B.3)
      RESETCH -> 0x12,
      CSUB -> 0x13,
      STOPP -> 0x15,
      LADD -> 0x16,
      STLB -> 0x17,
      STHF -> 0x18,
      NORM -> 0x19,
      LDIV -> 0x1a,
      LDPI -> 0x1b,
      STLF -> 0x1c,
      XDBLE -> 0x1d,
      LDPRI -> 0x1e,
      REM -> 0x1f,
      RET -> 0x20,
      LEND -> 0x21,
      LDTIMER -> 0x22,
      TESTERR -> 0x29,
      TESTPRANAL -> 0x2a,
      TIN -> 0x2b,
      DIV -> 0x2c,
      DIST -> 0x2e,
      DISC -> 0x2f,
      DISS -> 0x30,
      LMUL -> 0x31,
      NOT -> 0x32,
      XOR -> 0x33,
      BCNT -> 0x34,
      LSHR -> 0x35,
      LSHL -> 0x36,
      LSUM -> 0x37,
      LSUB -> 0x38,
      RUNP -> 0x39,
      XWORD -> 0x3a,
      SB -> 0x3b,
      GAJW -> 0x3c,
      SAVEL -> 0x3d,
      SAVEH -> 0x3e,
      WCNT -> 0x3f,
      SHR -> 0x40,
      SHL -> 0x41,
      MINT -> 0x42,
      ALT -> 0x43,
      ALTWT -> 0x44,
      ALTEND -> 0x45,
      AND -> 0x46,
      ENBT -> 0x47,
      ENBC -> 0x48,
      ENBS -> 0x49,
      MOVE -> 0x4a,
      OR -> 0x4b,
      CSNGL -> 0x4c,
      CCNT -> 0x4d,
      TALT -> 0x4e,
      LDIFF -> 0x4f,
      STHB -> 0x50,
      TALTWT -> 0x51,
      SUM -> 0x52,
      MUL -> 0x53,
      STTIMER -> 0x54,
      STOPERR -> 0x55,
      CWORD -> 0x56,
      CLRHALTERR -> 0x57,
      SETHALTERR -> 0x58,
      TESTHALTERR -> 0x59,
      DUP -> 0x5a,
      MOVE2DINIT -> 0x5b,
      MOVE2DALL -> 0x5c,
      MOVE2DNONZERO -> 0x5d,
      MOVE2DZERO -> 0x5e,
      GTU -> 0x5f,
      FMUL -> 0x72,
      CRCWORD -> 0x74,
      CRCBYTE -> 0x75,
      BITCNT -> 0x76,
      BITREVWORD -> 0x77,
      BITREVNBITS -> 0x78,
      POP -> 0x79,
      TIMERDISABLEH -> 0x7a,
      TIMERDISABLEL -> 0x7b,
      TIMERENABLEH -> 0x7c,
      TIMERENABLEL -> 0x7d,
      LDMEMSTARTVAL -> 0x7e,
      WSUBDB -> 0x81,
      FPLDNLDBI -> 0x82,
      FPSTNLDB -> 0x84,
      FPLDNLSNI -> 0x86,
      FPADD -> 0x87,
      FPSTNLSN -> 0x88,
      FPSUB -> 0x89,
      FPLDNLDB -> 0x8a,
      FPMUL -> 0x8b,
      FPDIV -> 0x8c,
      FPRANGE -> 0x8d,
      FPLDNLSN -> 0x8e,
      FPNAN -> 0x91,
      FPORDERED -> 0x92,
      FPNOTFINITE -> 0x93,
      FPGT -> 0x94,
      FPEQ -> 0x95,
      FPI32TOR32 -> 0x96,
      FPGE -> 0x97,
      FPI32TOR64 -> 0x98,
      FPB32TOR64 -> 0x9a,
      FPLG -> 0x9b,
      FPRTOI32 -> 0x9d,
      FPSTNLI32 -> 0x9e,
      FPLDZEROSN -> 0x9f,
      FPLDZERODB -> 0xa0,
      FPINT -> 0xa1,
      FPDUP -> 0xa3,
      FPREV -> 0xa4,
      FPLDNLADDDB -> 0xa6,
      FPLDNLMULDB -> 0xa8,
      FPLDNLADDSN -> 0xaa,
      FPLDNLMULSN -> 0xac,
      LDFLAGS -> 0xb6,
      STFLAGS -> 0xb7,
      XBWORD -> 0xb8,
      LBX -> 0xb9,
      CB -> 0xba,
      CBU -> 0xbb,
      INSPHDR -> 0xbc,
      READBFR -> 0xbd,
      LDCONF -> 0xbe,
      STCONF -> 0xbf,
      LDCNT -> 0xc0,
      SSUB -> 0xc1,
      LDTH -> 0xc2,
      LDCHSTATUS -> 0xc3,
      INTDIS -> 0xc4,
      INTENB -> 0xc5,
      CIR -> 0xc7,
      SS -> 0xc8,
      CHANTYPE -> 0xc9,
      CIRU -> 0xcc,
      FPREM -> 0xcf,
      FPRN -> 0xd0,
      FPDIVBY2 -> 0xd1,
      FPMULBY2 -> 0xd2,
      FPSQRT -> 0xd3,
      FPRP -> 0xd4,
      FPRM -> 0xd5,
      FPRZ -> 0xd6,
      FPR32TOR64 -> 0xd7,
      FPR64TOR32 -> 0xd8,
      FPEXPDEC32 -> 0xd9,
      FPEXPINC32 -> 0xda,
      FPABS -> 0xdb,
      FPCHKI32 -> 0xdc,
      FPCHKI64 -> 0xdd,
      DEVLB -> 0xf0,
      DEVSB -> 0xf1,
      DEVLS -> 0xf2,
      DEVSS -> 0xf3,
      DEVLW -> 0xf4,
      DEVSW -> 0xf5,
      XSWORD -> 0xf8,
      LSX -> 0xf9,
      CS -> 0xfa,
      CSU -> 0xfb,

      // Negative Prefix (Table B.4) - T9000 negative function codes use high enum values
      FPSTALL -> 0x101, // Hardware: -0x01
      FPLDALL -> 0x102, // Hardware: -0x02
      STSHADOW -> 0x103, // Hardware: -0x03
      LDSHADOW -> 0x104, // Hardware: -0x04
      TRET -> 0x105, // Hardware: -0x05
      GOPROT -> 0x106, // Hardware: -0x06
      SELTH -> 0x107, // Hardware: -0x07
      SYSCALL -> 0x108, // Hardware: -0x08
      WAIT -> 0x10b, // Hardware: -0x0B
      SIGNAL -> 0x10c, // Hardware: -0x0C
      TIMESLICE -> 0x10d, // Hardware: -0x0D
      INSERTQUEUE -> 0x10e, // Hardware: -0x0E
      SWAPTIMER -> 0x10f, // Hardware: -0x0F
      SWAPQUEUE -> 0x110, // Hardware: -0x10
      STOPCH -> 0x112, // Hardware: -0x12
      VOUT -> 0x113, // Hardware: -0x13
      VIN -> 0x114, // Hardware: -0x14
      // T9000 negative function codes - using high values to avoid conflicts
      SWAPBFR -> 0x117, // Hardware: -0x17
      SETHDR -> 0x118, // Hardware: -0x18
      SETCHMODE -> 0x119, // Hardware: -0x19
      INITVLCB -> 0x11a, // Hardware: -0x1A
      WRITEHDR -> 0x11b, // Hardware: -0x1B
      READHDR -> 0x11c, // Hardware: -0x1C
      DISG -> 0x11d, // Hardware: -0x1D
      ENBG -> 0x11e, // Hardware: -0x1E
      GRANT -> 0x11f, // Hardware: -0x1F
      // More T9000 negative function codes
      STMOVE2DINIT -> 0x120, // Hardware: -0x20
      CAUSEERROR -> 0x121, // Hardware: -0x21
      UNMKRC -> 0x123, // Hardware: -0x23
      MKRC -> 0x124, // Hardware: -0x24
      IRDSQ -> 0x125, // Hardware: -0x25
      ERDSQ -> 0x126, // Hardware: -0x26
      STRESPTR -> 0x127, // Hardware: -0x27
      LDRESPTR -> 0x128, // Hardware: -0x28
      DEVMOVE -> 0x12c, // Hardware: -0x2C
      ICL -> 0x12d, // Hardware: -0x2D
      FDCL -> 0x12e, // Hardware: -0x2E
      ICA -> 0x12f, // Hardware: -0x2F  
      FDCA -> 0x130, // Hardware: -0x30
      NOP -> 0x140, // Hardware: -0x40
      LDPRODID -> 0x184 // Hardware: -0x84
    )
  }
}
