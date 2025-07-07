# T9000 FPU AFix Implementation

## Overview

This document describes the enhanced FPU implementation using SpinalHDL's AFix (Arbitrary Fixed-point) type for improved IEEE 754 compliance and cleaner implementation.

## What is AFix?

AFix is SpinalHDL's advanced fixed-point arithmetic type that provides:
- **Automatic precision tracking** through arithmetic operations
- **Built-in overflow/underflow detection**
- **Native support for all IEEE 754 rounding modes**
- **Automatic bit growth calculation**
- **Type-safe fixed-point arithmetic**

## Key Advantages of AFix for FPU

### 1. Automatic Precision Management
```scala
// Traditional approach - manual bit width calculation
val product = (mantissaA.resize(106) * mantissaB.resize(106))(105 downto 0)

// AFix approach - automatic precision
val product = mantissaA * mantissaB  // AFix tracks precision automatically
```

### 2. Built-in Rounding Modes
```scala
// Map IEEE 754 modes directly to AFix RoundType
val rounded = value.rounded(RoundType.ROUNDTOEVEN)   // Round to nearest even
val rounded = value.rounded(RoundType.ROUNDTOZERO)   // Round toward zero
val rounded = value.rounded(RoundType.ROUNDTOINF)    // Round toward +∞
val rounded = value.rounded(RoundType.ROUNDDOWN)     // Round toward -∞
```

### 3. Overflow Detection
```scala
// AFix automatically detects when values exceed representable range
val overflow = result > AFix(maxValue)
val underflow = result < AFix(minValue)
```

### 4. Clean Denormal Handling
```scala
// AFix can represent denormal values naturally
val denormal = AFix.UQ(0 bit, 52 bit)  // 0.xxx format for denormals
```

## Implementation Components

### 1. FpuAFixCore.scala
Basic demonstration of AFix usage for FPU operations:
- FpNumber representation using AFix
- Arithmetic operations with automatic precision
- Exception detection
- Rounding control

### 2. AdderAFix.scala
Enhanced adder/subtractor implementation:
- **Automatic alignment** of mantissas based on exponent difference
- **Built-in normalization** with leading zero detection
- **Native rounding** using AFix RoundType
- **Clean special value handling** (NaN, Inf, Zero)

Key improvements:
- No manual bit width calculations
- Automatic handling of mantissa overflow
- Simplified denormal support

### 3. MultiplierAFix.scala
Booth-3 multiplier with AFix:
- **Automatic precision** for partial products
- **Tree reduction** with correct bit growth
- **Built-in normalization** after multiplication
- **Simplified overflow detection**

Key improvements:
- AFix handles Booth encoding precision automatically
- Partial product accumulation with automatic width
- Clean exponent calculation

### 4. DividerAFix.scala
Newton-Raphson divider using AFix:
- **Iterative refinement** with automatic precision tracking
- **Convergence detection** using AFix comparison
- **Clean reciprocal approximation**
- **Automatic error bound calculation**

Key improvements:
- AFix tracks precision through iterations
- Built-in convergence checking
- Simplified normalization

### 5. FpuAFixPlugin.scala
Main FPU plugin integrating AFix components:
- **Unified command/response interface**
- **Automatic precision selection** (single/double)
- **Integrated exception handling**
- **Clean pipeline integration**

## AFix Type System

### Basic AFix Types
```scala
// Unsigned fixed-point Q notation
AFix.UQ(intBits, fracBits)    // Unsigned Qm.n format

// Signed fixed-point
AFix.SQ(intBits, fracBits)    // Signed Qm.n format

// From/to other types
AFix.U(intBits)               // From unsigned integer
AFix.S(intBits)               // From signed integer
```

### IEEE 754 Formats in AFix
```scala
// Single precision mantissa (1.23 bits)
val singleMant = AFix.UQ(1 bit, 23 bit)

// Double precision mantissa (1.52 bits)  
val doubleMant = AFix.UQ(1 bit, 52 bit)

// Extended precision for intermediates
val extMant = AFix.UQ(3 bit, 64 bit)  // Extra guard bits
```

## Benefits Realized

### 1. Code Clarity
- Eliminated manual bit width calculations
- Removed complex shifting/alignment logic
- Cleaner overflow/underflow handling

### 2. Correctness
- Automatic precision prevents bit loss
- Built-in rounding ensures IEEE 754 compliance
- Type safety prevents precision errors

### 3. Performance
- AFix operations map efficiently to hardware
- Automatic bit growth minimizes unnecessary width
- Native support for common FPU operations

### 4. Maintainability
- Self-documenting precision requirements
- Easier to verify against IEEE 754 spec
- Reduced bug surface area

## Migration Strategy

### Phase 1: Core Arithmetic Units ✅
- Created AFix versions of Adder, Multiplier, Divider
- Validated against existing implementations
- Demonstrated cleaner code and same functionality

### Phase 2: Integration ✅
- Created FpuAFixPlugin integrating AFix units
- Maintained same external interface
- Added enhanced precision tracking

### Phase 3: Full Migration (In Progress)
- Convert remaining FPU components to AFix
- Update special value handling (VCU)
- Complete transcendental functions with AFix

### Phase 4: Optimization
- Leverage AFix for automatic pipeline balancing
- Use AFix precision for optimal bit widths
- Profile and tune for performance

## Example: Addition with AFix

### Traditional Implementation
```scala
// Manual alignment and normalization
val expDiff = expA - expB
val shift = expDiff.abs.min(63)
val alignedMantA = if(expA >= expB) mantA else mantA >> shift
val alignedMantB = if(expB > expA) mantB else mantB >> shift
val sum = alignedMantA + alignedMantB
// ... complex normalization logic ...
```

### AFix Implementation
```scala
// Automatic alignment and precision
val alignedA = mantA >> (expA < expB ? expB - expA : 0)
val alignedB = mantB >> (expB < expA ? expA - expB : 0)
val sum = alignedA + alignedB  // AFix handles precision
val normalized = sum.rounded(RoundType.ROUNDTOEVEN)
```

## Conclusion

The AFix-based FPU implementation provides:
1. **Cleaner code** with automatic precision management
2. **Better IEEE 754 compliance** with native rounding modes
3. **Reduced bugs** through type-safe arithmetic
4. **Easier maintenance** with self-documenting precision

The migration to AFix represents a significant improvement in code quality while maintaining full T9000 FPU compatibility.