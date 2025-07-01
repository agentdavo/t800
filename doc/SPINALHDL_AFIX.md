# SpinalHDL AFix Type - Fixed-Point Arithmetic

## Overview

AFix (Auto-ranging Fixed-Point) is SpinalHDL's advanced fixed-point arithmetic type that automatically tracks value ranges during operations. This is particularly useful for DSP applications and floating-point implementations.

## Key Concepts

### 1. Declaration Methods

```scala
// By bit sizes
val unsigned12bit = AFix.U(12 bits)         // Range: 0 to 4095
val signed12bit = AFix.S(12 bits)          // Range: -2048 to 2047
val unsignedQformat = AFix.UQ(8 bits, 4 bits) // 8 integer bits, 4 fractional bits

// By exponents
val byExp = AFix.U(8 exp, 12 bits)         // Unsigned with max exponent 8
val signedExp = AFix.S(8 exp, -4 exp)      // Signed with exp range [8, -4]
```

### 2. Automatic Range Expansion

AFix automatically expands ranges during operations:

```scala
val a = AFix.U(4 bits)          // Range: 0 to 15
val b = AFix.UQ(2 bits, 2 bits) // Range: 0 to 3.75
val c = a + b                   // Range automatically becomes 0 to 18.75
```

### 3. Operations

AFix supports standard mathematical operations:
- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Bit shifting: `<<`, `>>`
- Comparison: `>`, `<`, `>=`, `<=`, `===`, `=/=`

### 4. Rounding and Saturation

AFix provides sophisticated rounding modes:
- Round to nearest (ties to even)
- Round toward zero (truncate)
- Round toward +∞
- Round toward -∞

## T9000 FPU Usage

In the T9000 FPU implementation, AFix is used for:

### 1. Mantissa Operations
```scala
def roundIeee754(
  mantissa: AFix,
  mode: Bits,
  guardBit: Bool,
  roundBit: Bool,
  stickyBit: Bool,
  sign: Bool
): AFix
```

### 2. Format Conversions
```scala
def real32ToReal64(value: Bits): Bits = {
  val afix = AFix(value.asSInt.resize(64), 0 exp)
  // ... conversion logic
}
```

### 3. Integer-Float Conversions
```scala
def int32ToReal64(value: SInt): Bits = {
  val afix = AFix(value, 0 exp)
  val sign = afix.isNegative()
  // ... packing logic
}
```

## Best Practices

1. **Use AFix for Fixed-Point DSP**: When implementing filters, FFTs, or other DSP algorithms
2. **Leverage Auto-ranging**: Let AFix handle range calculations automatically
3. **Explicit Rounding**: Always specify rounding mode for precision-critical operations
4. **Check Range**: Use `.maxValue` and `.minValue` to verify representable ranges

## Common Pitfalls

1. **API Instability**: AFix API is still under development - check SpinalHDL version compatibility
2. **Performance**: AFix adds overhead for range tracking - use UFix/SFix for simpler cases
3. **Bit Width Growth**: Operations can cause rapid bit width growth - use truncation carefully

## Example: IEEE 754 Rounding

```scala
// Extended mantissa with guard, round, sticky bits
val extMantissa = AFix.U(55 bits)  // 52 + 3 extra bits

// Apply IEEE 754 rounding
val rounded = roundIeee754(
  mantissa = extMantissa,
  mode = roundingMode,        // 0=nearest, 1=zero, 2=+inf, 3=-inf
  guardBit = extMantissa(2),
  roundBit = extMantissa(1),
  stickyBit = extMantissa(0),
  sign = isNegative
)
```

## T9000-Specific Considerations

The T9000 FPU uses AFix for:
- Maintaining precision during intermediate calculations
- Implementing IEEE 754 compliant rounding
- Converting between single/double precision
- Handling denormalized numbers

AFix's automatic range tracking helps ensure correct implementation of the T9000's floating-point specification without manual range calculations.