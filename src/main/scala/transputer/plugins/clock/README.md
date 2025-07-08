# Clocks Plugin

This plugin provides centralized clock domain management for the T9000 Transputer.

## Overview

The ClocksPlugin manages multiple clock domains required by the T9000:
- **System Clock**: 200MHz - CPU pipeline and core logic
- **Memory Clock**: 200MHz - PMI and cache controllers  
- **Timer Clock**: 1MHz - System timers
- **Debug Clock**: 50MHz - Debug interface
- **DS-Link Clocks**: 200MHz each - 4 communication interfaces
- **Derived Clocks**: Slow (100MHz) and Very Slow (25MHz) system clocks

## Usage

### Basic Integration

```scala
// Add to your plugin list
val plugins = Seq(
  new ClocksPlugin(),
  // Other plugins...
)
```

### Accessing Clock Domains

```scala
// In another plugin
val clockService = host[ClockDomainService]
val sysDomain = clockService.getSystemDomain
val memDomain = clockService.getMemoryDomain

// Work within a specific clock domain
sysDomain {
  val myReg = Reg(UInt(32 bits)) init 0
}
```

### Clock Domain Crossing

```scala
// Create a data crossing between domains
val crossing = clockService.createDataCrossing(
  dataType = Bits(32 bits),
  from = sysDomain,
  to = memDomain
)

// Synchronize a pulse signal
val syncPulse = clockService.synchronizePulse(
  pulse = myPulse,
  from = timerDomain,
  to = sysDomain
)
```

## Integration with T9000

This plugin is optional and provides basic clock domain services. For full T9000 clock integration with PMI and VCP, see `T9000ClockIntegration.scala` which demonstrates advanced usage patterns.

The plugin is NOT included by default in T9000Param to keep it optional. Enable it by setting `enableMultiClock = true` in your T9000Param configuration.