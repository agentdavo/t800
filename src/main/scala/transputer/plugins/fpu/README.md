## The FpuPlugin provides two services:
FpuService

    send(op: FpCmd, a: Bits, b: Bits): Sends an FPU operation with command (FpCmd) and operands (64-bit IEEE-754).
    result: Bits: Retrieves the 64-bit result.
    resultAfix: AFix: Retrieves the result as an AFix (fixed-point).
    isBusy: Bool: Indicates if the FPU is processing a multi-cycle operation.

FpuControlService

    specialValueDetected: Bool: Flags NaN, infinity, denormal, or zero detection.
    specialResult: Bits: Provides special value results.
    trapEnable: Bool: Indicates an IEEE-754 exception.
    trapType: UInt: Specifies trap type (e.g., 0x4 for invalid).
    roundingMode: Bits: Current rounding mode (00: nearest, 01: zero, 10: positive, 11: minus).
    setRoundingMode(mode: Bits): Sets rounding mode.
    getErrorFlags: Bits: Retrieves error flags (overflow, underflow, etc.).
    clearErrorFlags: Clears error flags.
    isFpuBusy(opcode: Bits): Bool: True when the FPU pipeline is executing.

Vector Control Unit (VCU)

    Detects NaNs, infinities, denormals and zeros in either operand.
    Outputs `specialResult` (NaN, ±Infinity or 0) and raises `trapEnable`
    for invalid or denormal values. `comparisonResult` provides ordered
    comparisons when no special value is present.
    The FpuPlugin uses these signals to bypass arithmetic units when a
    special result is required.

Trap Types

    0x1: Invalid memory access (MemoryManagementPlugin).
    0x2: Stack extension needed (MemoryManagementPlugin).
    0x4: FPU invalid/privileged operation (FpuPlugin and MMU).

Special values

    The Utils object exposes `genNaN` and `genInfinity(sign)` helpers.
    These 64-bit constants are supplied to the VCU and can also be pushed
    to FA/FB via `FpuOpsService.push`.

Supported Instructions

    Load/Store: fpldnlsn, fpldnldb, fpldnladdsn, fpstnlsn, etc.
    General: fpentry, fprev, fpdup.
    Rounding: fprn, fprz, fprp, fprm.
    Error: fpchkerr, fptesterr, fpseterr, fpclrerr.
    Comparison: fpgt, fpeq, fpordered, fpnan, etc.
    Conversion: fpr32tor64, fpr64tor32, fprtoi32, etc.
    Arithmetic: fpadd, fpsub, fpmul, fpdiv, fpabs, etc.
    T805 Compatibility: fpusqrtfirst, fpusqrtstep, fpremfirst, etc.
    Additional: fprem, fpsqrt, fprange, fpge, fplg.

Integration

    Pipeline: Execute stage, parallel to PrimaryInstrPlugin and SecondaryInstrPlugin.
    Dependencies: RegFilePlugin (FPAreg, FPBreg, FPCreg), SystemBusService (128-bit BMB), TrapHandlerService.

Using AFix

    val fpu = host[FpuOpsService]

    // Push fixed-point operands onto the FA/FB stack
    fpu.pushAfix(AFix(1.0, 8 exp))
    fpu.pushAfix(AFix(2.5, 8 exp))

    // Pop the top of stack as AFix
    val top: AFix = fpu.popAfix()

    // Execute an addition using AFix operands
    val sum: AFix = fpu.executeAfix(
        FpOp.Arithmetic.FPADD,
        Vec(AFix(0.5, 8 exp), AFix(1.25, 8 exp))
    )

### Opcode enumeration

`FpOp` groups the secondary opcodes into categories:

* **Load/Store**: FPLDNLSN, FPLDNLDB, FPLDNLSNI, FPLDNLDBI, FPLDZEROSN,
  FPLDZERODB, FPLDNLADDSN, FPLDNLADDDB, FPLDNLMULSN, FPLDNLMULDB,
  FPSTNLSN, FPSTNLDB, FPSTNLI32
* **General**: FPENTRY, FPREV, FPDUP
* **Rounding**: FPRN, FPRZ, FPRP, FPRM
* **Error**: FPCHKERR, FPTESTERR, FPSETERR, FPCLRERR
* **Comparison**: FPGT, FPEQ, FPORDERED, FPNAN, FPNOTFINITE, FPCHKI32, FPCHKI64
* **Conversion**: FPR32TOR64, FPR64TOR32, FPRTOI32, FPI32TOR32, FPI32TOR64,
  FPB32TOR64, FPNOROUND, FPINT
* **Arithmetic**: FPADD, FPSUB, FPMUL, FPDIV, FPABS, FPEXPINC32, FPEXPDEC32,
  FPMULBY2, FPDIVBY2
* **T805 Compatibility**: FPUSQRTFIRST, FPUSQRTSTEP, FPUSQRTLAST, FPREMFIRST,
  FPREMSTEP
* **Additional**: FPREM, FPSQRT, FPRANGE, FPGE, FPLG

`FpOp.fromOpcode` uses a `switch` over `Opcode.SecondaryOpcode` values and maps
each byte to the matching `FpOp` entry. Unknown opcodes yield `FpOp.NONE`.

### Rounding modes

The 2-bit `roundingMode` implements IEEE‑754 rounding:

* `00` – round to nearest even
* `01` – round toward zero
* `10` – round toward +∞
* `11` – round toward −∞

### Divider and square-root latency

The divider/root unit executes multi-cycle operations. Typical latencies are:

* `fpdiv` / `fpsqrt` – 15 cycles
* `fprem` – 529 cycles
* `fprange` – 17 cycles
* `fpusqrtfirst`, `fpusqrtstep`, `fpusqrtlast`, `fpremfirst`, `fpremstep`
  – `divRoot.io.cycles` (1 cycle in the minimal implementation)
