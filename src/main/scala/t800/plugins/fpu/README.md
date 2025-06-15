## The FpuPlugin provides two services:
FpuSrv

    send(op: FpCmd, a: Bits, b: Bits): Sends an FPU operation with command (FpCmd) and operands (64-bit IEEE-754).
    result: Bits: Retrieves the 64-bit result.
    resultAfix: AFix: Retrieves the result as an AFix (fixed-point).
    isBusy: Bool: Indicates if the FPU is processing a multi-cycle operation.

FpuControlSrv

    specialValueDetected: Bool: Flags NaN, infinity, denormal, or zero detection.
    specialResult: Bits: Provides special value results.
    trapEnable: Bool: Indicates an IEEE-754 exception.
    trapType: UInt: Specifies trap type (e.g., 0x4 for invalid).
    roundingMode: Bits: Current rounding mode (00: nearest, 01: zero, 10: positive, 11: minus).
    setRoundingMode(mode: Bits): Sets rounding mode.
    getErrorFlags: Bits: Retrieves error flags (overflow, underflow, etc.).
    clearErrorFlags: Clears error flags.

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
    Dependencies: RegFilePlugin (FPAreg, FPBreg, FPCreg), SystemBusSrv (128-bit BMB), TrapHandlerSrv.
