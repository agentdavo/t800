# Script Migration Guide

This guide helps migrate from the old individual scripts to the new consolidated scripts.

## Script Consolidation

The T9000 project scripts have been consolidated from 17+ individual scripts into 4 main scripts for better organization and ease of use.

### Old Script → New Script Mapping

| Old Script | New Command | Description |
|------------|-------------|-------------|
| `build_t9000_system.sh` | `./scripts/build.sh --config all` | Build all configurations |
| `build_t9000_tests.sh` | `./scripts/test.sh --type full` | Run comprehensive tests |
| `validate_t9000_system.sh` | `./scripts/test.sh --type quick` | Quick validation |
| `validate_t9000_pipeline.sh` | `./scripts/test.sh --type pipeline` | Pipeline validation |
| `assemble_inmos.sh` | `./scripts/assemble.sh --assembler inmos` | INMOS assembler |
| `generate_konata_demo.sh` | `./scripts/utils.sh konata --demo` | Konata demo |
| `test_bootload_trace.sh` | `./scripts/utils.sh trace --hex bootload.hex` | Bootload trace |
| `test_verilator_simple.sh` | `./scripts/utils.sh verilator-fix` | Verilator fixes |
| `patch_verilator_makefile.sh` | `./scripts/utils.sh verilator-fix` | Fix PCH issues |
| `run_sim_no_pch.sh` | Built into main scripts with auto-fix | PCH disabled by default |

## New Script Overview

### 1. `build.sh` - Build Management
- Replaces: `build_t9000_system.sh`
- Features: Multiple configurations, custom parameters, clean output

### 2. `test.sh` - Test Execution
- Replaces: `build_t9000_tests.sh`, `validate_*.sh`
- Features: Quick/full/pipeline/integration tests, report generation

### 3. `assemble.sh` - Assembly Management
- Replaces: `assemble_inmos.sh`
- Features: Multiple assemblers, format options, example listing

### 4. `utils.sh` - Development Utilities
- Replaces: All other utility scripts
- Features: Konata, trace, waveform, clean, info commands

## Benefits of Consolidation

1. **Fewer Files**: 4 scripts instead of 17+
2. **Consistent Interface**: All scripts use similar option patterns
3. **Better Help**: Each script has `--help` with detailed usage
4. **Unified Reports**: All test reports go to `scripts/test_reports/`
5. **Easier Maintenance**: Less duplication, cleaner code

## Quick Start Examples

```bash
# Old way
./scripts/build_t9000_system.sh
./scripts/validate_t9000_system.sh
./scripts/build_t9000_tests.sh

# New way
./scripts/build.sh --config all
./scripts/test.sh --type quick
./scripts/test.sh --type full
```

## Getting Help

Each script supports `--help`:
```bash
./scripts/build.sh --help
./scripts/test.sh --help
./scripts/assemble.sh --help
./scripts/utils.sh --help
```

For command-specific help:
```bash
./scripts/utils.sh konata --help
./scripts/utils.sh trace --help
```