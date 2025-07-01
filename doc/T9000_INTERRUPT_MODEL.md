# T9000 Interrupt and Timer Model

## Unified Communication Model

The T9000 transputer implements a revolutionary approach to interrupts, events, and timers by treating them all as communication channels. This provides a consistent programming model where:

- **Interrupts are inputs**: An interrupt handler is simply a process waiting for input from an Event channel
- **Events are channels**: The 4 Event pins can be configured as inputs (interrupts) or outputs (handshakes)
- **Timers are inputs**: Processes can wait for timer values just like waiting for channel input

## Event Channels

The T9000 has 4 Event channels that can be configured as:

### Input Mode (Interrupts)
- External signal causes waiting process to become ready
- Process waits using standard channel input semantics
- Automatic acknowledgment or manual acknowledgment modes
- No special interrupt instructions needed

### Output Mode (Handshakes)
- Process outputs to Event channel and waits for external handshake
- Provides synchronized output with external devices
- Process automatically descheduled until handshake received

## Timer System

The T9000 implements two timers:

### ClockReg0 - Microsecond Timer
- Increments every microsecond (1 MHz)
- 32-bit counter
- Can be read directly or used for timer input

### ClockReg1 - 64-Microsecond Timer  
- Increments every 64 microseconds (15.625 kHz)
- 32-bit counter
- Derived from ClockReg0 / 64
- Lower power consumption for longer timeouts

### Timer Operations

1. **Timer Read**: Immediate read of current timer value
   ```
   time := TIMER  // Returns current value immediately
   ```

2. **Timer Input**: Process waits until specified time
   ```
   TIN(targetTime)  // Process descheduled until timer >= targetTime
   ```

## Implementation Benefits

This unified model provides several advantages:

1. **Simplicity**: All waiting operations use same mechanism
2. **No Special Code**: Interrupt handlers are normal processes
3. **Automatic Scheduling**: Hardware manages all wait conditions
4. **Priority Preserved**: High-priority processes wake first
5. **Low Latency**: Hardware-managed wake-up on events

## Example Usage

### Interrupt Handler
```occam
PROC interrupt.handler(CHAN OF ANY event.in)
  WHILE TRUE
    ANY data:
    event.in ? data      -- Wait for interrupt (process descheduled)
    -- Handle interrupt
:
```

### Timer Delay
```occam
PROC delay(VAL INT microseconds)
  TIMER clock:
  INT time:
  SEQ
    clock ? time         -- Read current time
    time := time + microseconds
    clock ? AFTER time   -- Wait until time reached
:
```

### Event Output with Handshake
```occam
PROC synchronized.output(CHAN OF ANY event.out)
  SEQ
    event.out ! signal   -- Output and wait for handshake
    -- Continues only after external acknowledgment
:
```

This elegant design makes the T9000 ideal for real-time systems where predictable response times and clean interrupt handling are critical.