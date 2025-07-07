# FpuControl.scala Field Mismatch Fixes

## Summary
Fixed bundle field mismatches in FpuControl.scala to match the actual bundle definitions in Opcodes.scala.

## Issues Fixed

### 1. ControlRsp Bundle Field Mismatches
**Problem**: Code was trying to access non-existent fields `status`, `hasExceptions`, `exceptionBits`, `selfTestPassed`
**Solution**: Changed all references to use the correct field `statusOut`

**Before**:
```scala
io.rsp.payload.status := currentStatus
io.rsp.payload.hasExceptions := allExceptions =/= 0
io.rsp.payload.exceptionBits := allExceptions
io.rsp.payload.selfTestPassed := testResult
```

**After**:
```scala
io.rsp.payload.statusOut := currentStatus
```

### 2. ControlCmd Bundle Field Mismatches
**Problem**: Code was trying to access non-existent field `operand`
**Solution**: Changed references to use the correct field `statusIn`

**Before**:
```scala
val errorMask = io.cmd.payload.operand(4 downto 0)
newStatus := io.cmd.payload.operand(31 downto 0)
```

**After**:
```scala
val errorMask = io.cmd.payload.statusIn(4 downto 0)
newStatus := io.cmd.payload.statusIn
```

### 3. Bits Comparison Issues
**Problem**: Using comparison operators on Bits without conversion to UInt
**Solution**: Added `.asUInt` conversions

**Before**:
```scala
val roundingValid = io.statusIn(6 downto 5) < 4
val reservedZero = io.statusIn(31 downto 13) === 0
```

**After**:
```scala
val roundingValid = io.statusIn(6 downto 5).asUInt < 4
val reservedZero = io.statusIn(31 downto 13).asUInt === 0
```

## Bundle Definitions (from Opcodes.scala)

### ControlCmd
```scala
case class ControlCmd() extends Bundle {
  val op = FpOp()
  val statusIn = Bits(32 bits)
  val roundingMode = Bits(2 bits)
}
```

### ControlRsp
```scala
case class ControlRsp() extends Bundle {
  val statusOut = Bits(32 bits)
}
```

## Result
All field mismatch errors in FpuControl.scala have been resolved. The code now correctly uses the actual bundle fields defined in Opcodes.scala.