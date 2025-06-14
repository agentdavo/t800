package t800

import spinal.core._

/** Primary and secondary opcode definitions for the T800 ISA. */
object Opcodes {
  object Primary {
    def PFIX = B(0x2, 4 bits)
    def NFIX = B(0x6, 4 bits)
    def OPR = B(0xf, 4 bits)
    def LDC = B(0x4, 4 bits)
    def LDL = B(0x7, 4 bits)
    def STL = B(0xd, 4 bits)
    def LDLP = B(0x1, 4 bits)
    def ADC = B(0x8, 4 bits)
    def EQC = B(0xc, 4 bits)
    def J = B(0x0, 4 bits)
    def CJ = B(0xa, 4 bits)
    def LDNL = B(0x3, 4 bits)
    def STNL = B(0xe, 4 bits)
    def LDNLP = B(0x5, 4 bits)
    def CALL = B(0x9, 4 bits)
    def AJW = B(0xb, 4 bits)
  }

  object Secondary {
    def REV = B(0x00, 8 bits)
    def LB = B(0x01, 8 bits)
    def BSUB = B(0x02, 8 bits)
    def ENDP = B(0x03, 8 bits)
    def DIFF = B(0x04, 8 bits)
    def ADD = B(0x05, 8 bits)
    def GCALL = B(0x06, 8 bits)
    def IN = B(0x07, 8 bits)
    def PROD = B(0x08, 8 bits)
    def GT = B(0x09, 8 bits)
    def WSUB = B(0x0a, 8 bits)
    def OUT = B(0x0b, 8 bits)
    def SUB = B(0x0c, 8 bits)
    def STARTP = B(0x0d, 8 bits)
    def OUTBYTE = B(0x0e, 8 bits)
    def OUTWORD = B(0x0f, 8 bits)
    def SETERR = B(0x10, 8 bits)
    def RESETCH = B(0x12, 8 bits)
    def CSUB = B(0x13, 8 bits)
    def STOPP = B(0x15, 8 bits)
    def LADD = B(0x16, 8 bits)
    def STLB = B(0x17, 8 bits)
    def STHF = B(0x18, 8 bits)
    def NORM = B(0x19, 8 bits)
    def LDIV = B(0x1a, 8 bits)
    def LDPI = B(0x1b, 8 bits)
    def STLF = B(0x1c, 8 bits)
    def XDBLE = B(0x1d, 8 bits)
    def LDPRI = B(0x1e, 8 bits)
    def REM = B(0x1f, 8 bits)
    def RET = B(0x20, 8 bits)
    def LEND = B(0x21, 8 bits)
    def LDTIMER = B(0x22, 8 bits)
    def TESTLDS = B(0x23, 8 bits)
    def TESTLDE = B(0x24, 8 bits)
    def TESTLDD = B(0x25, 8 bits)
    def TESTSTS = B(0x26, 8 bits)
    def TESTSTE = B(0x27, 8 bits)
    def TESTSTD = B(0x28, 8 bits)
    def TESTERR = B(0x29, 8 bits)
    def TESTPRANAL = B(0x2a, 8 bits)
    def TIN = B(0x2b, 8 bits)
    def DIV = B(0x2c, 8 bits)
    def TESTHARDCHAN = B(0x2d, 8 bits)
    def DIST = B(0x2e, 8 bits)
    def DISC = B(0x2f, 8 bits)
    def DISS = B(0x30, 8 bits)
    def LMUL = B(0x31, 8 bits)
    def NOT = B(0x32, 8 bits)
    def XOR = B(0x33, 8 bits)
    def BCNT = B(0x34, 8 bits)
    def LSHR = B(0x35, 8 bits)
    def LSHL = B(0x36, 8 bits)
    def LSUM = B(0x37, 8 bits)
    def LSUB = B(0x38, 8 bits)
    def RUNP = B(0x39, 8 bits)
    def XWORD = B(0x3a, 8 bits)
    def SB = B(0x3b, 8 bits)
    def GAJW = B(0x3c, 8 bits)
    def SAVEL = B(0x3d, 8 bits)
    def SAVEH = B(0x3e, 8 bits)
    def WCNT = B(0x3f, 8 bits)
    def SHR = B(0x40, 8 bits)
    def SHL = B(0x41, 8 bits)
    def MINT = B(0x42, 8 bits)
    def ALT = B(0x43, 8 bits)
    def ALTWT = B(0x44, 8 bits)
    def ALTEND = B(0x45, 8 bits)
    def AND = B(0x46, 8 bits)
    def ENBT = B(0x47, 8 bits)
    def ENBC = B(0x48, 8 bits)
    def ENBS = B(0x49, 8 bits)
    def MOVE = B(0x4a, 8 bits)
    def OR = B(0x4b, 8 bits)
    def CSNGL = B(0x4c, 8 bits)
    def CCNT = B(0x4d, 8 bits)
    def TALT = B(0x4e, 8 bits)
    def LDIFF = B(0x4f, 8 bits)
    def STHB = B(0x50, 8 bits)
    def TALTWT = B(0x51, 8 bits)
    def SUM = B(0x52, 8 bits)
    def MUL = B(0x53, 8 bits)
    def STTIMER = B(0x54, 8 bits)
    def STOPERR = B(0x55, 8 bits)
    def CWORD = B(0x56, 8 bits)
    def CLRHALTERR = B(0x57, 8 bits)
    def SETHALTERR = B(0x58, 8 bits)
    def TESTHALTERR = B(0x59, 8 bits)
    def DUP = B(0x5a, 8 bits)
    def MOVE2DINIT = B(0x5b, 8 bits)
    def MOVE2DALL = B(0x5c, 8 bits)
    def MOVE2DNONZERO = B(0x5d, 8 bits)
    def MOVE2DZERO = B(0x5e, 8 bits)
    def GTU = B(0x5f, 8 bits)
    def UNPACKSN = B(0x63, 8 bits)
    def POSTNORMSN = B(0x6c, 8 bits)
    def ROUNDSN = B(0x6d, 8 bits)
    def LDINF = B(0x71, 8 bits)
    def FMUL = B(0x72, 8 bits)
    def CFLERR = B(0x73, 8 bits)
    def CRCWORD = B(0x74, 8 bits)
    def CRCBYTE = B(0x75, 8 bits)
    def BITCNT = B(0x76, 8 bits)
    def BITREVWORD = B(0x77, 8 bits)
    def BITREVNBITS = B(0x78, 8 bits)
    def POP = B(0x79, 8 bits)
    def TIMERDISABLEH = B(0x7a, 8 bits)
    def TIMERDISABLEL = B(0x7b, 8 bits)
    def TIMERENABLEH = B(0x7c, 8 bits)
    def TIMERENABLEL = B(0x7d, 8 bits)
    def LDMEMSTARTVAL = B(0x7e, 8 bits)
    def WSUBDB = B(0x81, 8 bits)
    def FPLDNLDBI = B(0x82, 8 bits)
    def FPCHKERR = B(0x83, 8 bits)
    def FPSTNLDB = B(0x84, 8 bits)
    def FPLDNLSNI = B(0x86, 8 bits)
    def FPADD = B(0x87, 8 bits)
    def FPSTNLSN = B(0x88, 8 bits)
    def FPSUB = B(0x89, 8 bits)
    def FPLDNLDB = B(0x8a, 8 bits)
    def FPMUL = B(0x8b, 8 bits)
    def FPDIV = B(0x8c, 8 bits)
    def FPRANGE = B(0x8d, 8 bits)
    def FPLDNLSN = B(0x8e, 8 bits)
    def FPREMFIRST = B(0x8f, 8 bits)
    def FPREMSTEP = B(0x90, 8 bits)
    def FPNAN = B(0x91, 8 bits)
    def FPORDERED = B(0x92, 8 bits)
    def FPNOTFINITE = B(0x93, 8 bits)
    def FPGT = B(0x94, 8 bits)
    def FPEQ = B(0x95, 8 bits)
    def FPI32TOR32 = B(0x96, 8 bits)
    def FPGE = B(0x97, 8 bits)
    def FPI32TOR64 = B(0x98, 8 bits)
    def FPB32TOR64 = B(0x9a, 8 bits)
    def FPLG = B(0x9b, 8 bits)
    def FPTESTERR = B(0x9c, 8 bits)
    def FPRTOI32 = B(0x9d, 8 bits)
    def FPSTNLI32 = B(0x9e, 8 bits)
    def FPLDZEROSN = B(0x9f, 8 bits)
    def FPLDZERODB = B(0xa0, 8 bits)
    def FPINT = B(0xa1, 8 bits)
    def FPDUP = B(0xa3, 8 bits)
    def FPREV = B(0xa4, 8 bits)
    def FPLDNLADDDB = B(0xa6, 8 bits)
    def FPLDNLMULDB = B(0xa8, 8 bits)
    def FPLDNLADDSN = B(0xaa, 8 bits)
    def FPENTRY = B(0xab, 8 bits)
    def FPLDNLMULSN = B(0xac, 8 bits)
    def SETTIMESLICE = B(0xb0, 8 bits)
    def BREAK = B(0xb1, 8 bits)
    def CLRJ0BREAK = B(0xb2, 8 bits)
    def SETJ0BREAK = B(0xb3, 8 bits)
    def TESTJ0BREAK = B(0xb4, 8 bits)
    def LDFLAGS = B(0xb6, 8 bits)
    def STFLAGS = B(0xb7, 8 bits)
    def XBWORD = B(0xb8, 8 bits)
    def LBX = B(0xb9, 8 bits)
    def CB = B(0xba, 8 bits)
    def CBU = B(0xbb, 8 bits)
    def INSPHDR = B(0xbc, 8 bits)
    def READBFR = B(0xbd, 8 bits)
    def LDCONF = B(0xbe, 8 bits)
    def STCONF = B(0xbf, 8 bits)
    def LDCNT = B(0xc0, 8 bits)
    def SSUB = B(0xc1, 8 bits)
    def LDTH = B(0xc2, 8 bits)
    def LDCHSTATUS = B(0xc3, 8 bits)
    def INTDIS = B(0xc4, 8 bits)
    def INTENB = B(0xc5, 8 bits)
    def CIR = B(0xc7, 8 bits)
    def SS = B(0xc8, 8 bits)
    def CHANTYPE = B(0xc9, 8 bits)
    def LS = B(0xca, 8 bits)
    def FPSETERR = B(0xcb, 8 bits)
    def CIRU = B(0xcc, 8 bits)
    def FPREM = B(0xcf, 8 bits)
    def FPRN = B(0xd0, 8 bits)
    def FPDIVBY2 = B(0xd1, 8 bits)
    def FPMULBY2 = B(0xd2, 8 bits)
    def FPSQRT = B(0xd3, 8 bits)
    def FPRP = B(0xd4, 8 bits)
    def FPRM = B(0xd5, 8 bits)
    def FPRZ = B(0xd6, 8 bits)
    def FPR32TOR64 = B(0xd7, 8 bits)
    def FPR64TOR32 = B(0xd8, 8 bits)
    def FPEXPDEC32 = B(0xd9, 8 bits)
    def FPEXPINC32 = B(0xda, 8 bits)
    def FPABS = B(0xdb, 8 bits)
    def FPCLRERR = B(0xdc, 8 bits)
    def FPADDDBSN = B(0xdd, 8 bits)
    def FPCHK32 = B(0xde, 8 bits)
    def FPCHK64 = B(0xdf, 8 bits)
    def DEVLB = B(0xf0, 8 bits)
    def DEVSB = B(0xf1, 8 bits)
    def DEVLS = B(0xf2, 8 bits)
    def DEVSS = B(0xf3, 8 bits)
    def DEVLW = B(0xf4, 8 bits)
    def DEVSW = B(0xf5, 8 bits)
    def XSWORD = B(0xf8, 8 bits)
    def LSX = B(0xf9, 8 bits)
    def CS = B(0xfa, 8 bits)
    def CSU = B(0xfb, 8 bits)
  }

  /** SpinalEnum variants of the opcode tables used by the plugins. */
  object Enum {
    object Primary extends SpinalEnum(binarySequential) {
      /*
       * Order opcodes by their 4-bit values so that "binarySequential"
       * matches the encoding nibble directly:
       *  0 -> J, 1 -> LDLP, 2 -> PFIX, 3 -> LDNL, ... 15 -> OPR
       */
      val J, LDLP, PFIX, LDNL, LDC, LDNLP, NFIX, LDL, ADC, CALL, CJ, AJW, EQC, STL, STNL, OPR =
        newElement()

      /* Sequential encoding via binarySequential simplifies decoding logic */
    }

    object Secondary extends SpinalEnum {
      val REV, LB, ADD, IN, OUT, SUB, STARTP, OUTBYTE, OUTWORD, STLB, STHF, LDPI, STLF, RET,
        LDTIMER, TESTERR, XOR, SHR, SHL, MINT, ALT, ALTWT, ALTEND, AND, MOVE, STHB, STTIMER,
        CLRHALTERR, SETHALTERR, TESTHALTERR, DUP, POP, TIMERDISABLEH, TIMERDISABLEL, TIMERENABLEH,
        TIMERENABLEL, FPADD, FPSUB, FPMUL, FPDIV = newElement()
      defaultEncoding = SpinalEnumEncoding("static")(
        REV -> 0x00,
        LB -> 0x01,
        ADD -> 0x05,
        IN -> 0x07,
        OUT -> 0x0b,
        SUB -> 0x0c,
        STARTP -> 0x0d,
        OUTBYTE -> 0x0e,
        OUTWORD -> 0x0f,
        STLB -> 0x17,
        STHF -> 0x18,
        LDPI -> 0x1b,
        STLF -> 0x1c,
        RET -> 0x20,
        LDTIMER -> 0x22,
        TESTERR -> 0x29,
        XOR -> 0x33,
        SHR -> 0x40,
        SHL -> 0x41,
        MINT -> 0x42,
        ALT -> 0x43,
        ALTWT -> 0x44,
        ALTEND -> 0x45,
        AND -> 0x46,
        MOVE -> 0x4a,
        STHB -> 0x50,
        STTIMER -> 0x54,
        CLRHALTERR -> 0x57,
        SETHALTERR -> 0x58,
        TESTHALTERR -> 0x59,
        DUP -> 0x5a,
        POP -> 0x79,
        TIMERDISABLEH -> 0x7a,
        TIMERDISABLEL -> 0x7b,
        TIMERENABLEH -> 0x7c,
        TIMERENABLEL -> 0x7d,
        FPADD -> 0x87,
        FPSUB -> 0x89,
        FPMUL -> 0x8b,
        FPDIV -> 0x8c
      )
    }
  }
}
