<p align="center">
  <img src="https://raw.githubusercontent.com/simple-eiffel/claude_eiffel_op_docs/main/artwork/LOGO.png" alt="simple_ library logo" width="400">
</p>

# SIMPLE_MMAP

**[Documentation](https://simple-eiffel.github.io/simple_mmap/)**

### Memory-Mapped File Library for Eiffel

[![Language](https://img.shields.io/badge/language-Eiffel-blue.svg)](https://www.eiffel.org/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Platform](https://img.shields.io/badge/platform-Windows-blue.svg)]()
[![SCOOP](https://img.shields.io/badge/SCOOP-compatible-orange.svg)]()
[![Design by Contract](https://img.shields.io/badge/DbC-enforced-orange.svg)]()
[![Tests](https://img.shields.io/badge/tests-10%20passing-brightgreen.svg)]()

---

## Overview

SIMPLE_MMAP provides SCOOP-compatible memory-mapped file operations for Eiffel applications. It wraps the Win32 file mapping API (CreateFileMapping, MapViewOfFile, etc.) through a clean C interface, enabling efficient file I/O and shared memory without threading complications.

The library supports file-backed mappings, anonymous mappings, and shared memory segments for inter-process communication, with type-safe read/write operations for bytes, integers, strings, and raw byte arrays.

**Developed using AI-assisted methodology:** Built interactively with Claude Opus 4.5 following rigorous Design by Contract principles.

---

## Features

### Mapping Types

- **File Mapping** - Map entire files or file regions into memory
- **Anonymous Mapping** - Create memory-backed mappings without files
- **Shared Memory** - Named mappings for inter-process communication

### Access Modes

| Mode | Description |
|------|-------------|
| `Access_read` | Read-only access |
| `Access_read_write` | Read and write access |

### Data Operations

| Operation | Description |
|-----------|-------------|
| `read_byte` / `write_byte` | Single byte access |
| `read_integer` / `write_integer` | 32-bit integer access |
| `read_string` / `write_string` | Null-terminated strings |
| `read_bytes` / `write_bytes` | Raw byte arrays |

---

## Quick Start

### Installation

1. Clone the repository:
```bash
git clone https://github.com/simple-eiffel/simple_mmap.git
```

2. Compile the C library:
```bash
cd simple_mmap/Clib
compile.bat
```

3. Set the environment variable:
```bash
set SIMPLE_MMAP=D:\path\to\simple_mmap
```

4. Add to your ECF file:
```xml
<library name="simple_mmap" location="$SIMPLE_MMAP\simple_mmap.ecf"/>
```

### Basic Usage

```eiffel
class
    MY_APPLICATION

feature

    file_mapping_example
        local
            mmap: SIMPLE_MMAP
        do
            -- Map an existing file
            create mmap.make_from_file ("data.bin", mmap.Access_read_write)

            if mmap.is_valid then
                -- Read/write at specific offsets
                mmap.write_string (0, "Hello, World!")
                print (mmap.read_string (0))

                mmap.close
            end
        end

    anonymous_mapping_example
        local
            mmap: SIMPLE_MMAP
        do
            -- Create anonymous (memory-only) mapping
            create mmap.make_anonymous (4096)  -- 4KB

            if mmap.is_valid then
                mmap.write_integer (0, 42)
                print (mmap.read_integer (0))  -- 42

                mmap.close
            end
        end

    shared_memory_example
        local
            mmap: SIMPLE_MMAP
        do
            -- Create named shared memory (accessible from other processes)
            create mmap.make_shared ("MySharedMem", 1024)

            if mmap.is_valid then
                mmap.write_string (0, "Shared data")
                mmap.close
            end
        end

end
```

---

## API Reference

### SIMPLE_MMAP Class

#### Creation

```eiffel
make_from_file (a_path: STRING_8; a_access: INTEGER)
    -- Create mapping from file with specified access mode.

make_anonymous (a_size: INTEGER)
    -- Create anonymous (memory-only) mapping of given size.

make_shared (a_name: STRING_8; a_size: INTEGER)
    -- Create named shared memory mapping.
```

#### Reading Data

```eiffel
read_byte (a_offset: INTEGER): NATURAL_8
    -- Read single byte at offset.

read_integer (a_offset: INTEGER): INTEGER
    -- Read 32-bit integer at offset.

read_string (a_offset: INTEGER): STRING_8
    -- Read null-terminated string at offset.

read_bytes (a_offset, a_count: INTEGER): ARRAY [NATURAL_8]
    -- Read array of bytes at offset.
```

#### Writing Data

```eiffel
write_byte (a_offset: INTEGER; a_value: NATURAL_8)
    -- Write single byte at offset.

write_integer (a_offset: INTEGER; a_value: INTEGER)
    -- Write 32-bit integer at offset.

write_string (a_offset: INTEGER; a_value: STRING_8)
    -- Write null-terminated string at offset.

write_bytes (a_offset: INTEGER; a_bytes: ARRAY [NATURAL_8])
    -- Write array of bytes at offset.
```

#### Status Queries

```eiffel
is_valid: BOOLEAN
    -- Is the mapping valid?

size: INTEGER
    -- Size of the mapped region in bytes.

close
    -- Unmap and release resources.
```

---

## Building & Testing

### Build Library

```bash
cd simple_mmap
ec -config simple_mmap.ecf -target simple_mmap -c_compile
```

### Run Tests

```bash
ec -config simple_mmap.ecf -target simple_mmap_tests -c_compile
./EIFGENs/simple_mmap_tests/W_code/simple_mmap.exe
```

**Test Results:** 10 tests passing

---

## Project Structure

```
simple_mmap/
├── Clib/                       # C wrapper library
│   ├── simple_mmap.h           # C header file
│   ├── simple_mmap.c           # C implementation
│   └── compile.bat             # Build script
├── src/                        # Eiffel source
│   └── simple_mmap.e           # Main wrapper class
├── testing/                    # Test suite
│   ├── application.e           # Test runner
│   └── test_simple_mmap.e      # Test cases
├── simple_mmap.ecf             # Library configuration
├── README.md                   # This file
└── LICENSE                     # MIT License
```

---

## Dependencies

- **Windows OS** - File mapping API is Windows-specific
- **EiffelStudio 23.09+** - Development environment
- **Visual Studio C++ Build Tools** - For compiling C wrapper

---

## SCOOP Compatibility

SIMPLE_MMAP is fully SCOOP-compatible. The C wrapper handles all Win32 API calls synchronously without threading dependencies, making it safe for use in concurrent Eiffel applications.

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

## Contact

- **Author:** Larry Rix
- **Repository:** https://github.com/simple-eiffel/simple_mmap
- **Issues:** https://github.com/simple-eiffel/simple_mmap/issues

---

## Acknowledgments

- Built with Claude Opus 4.5 (Anthropic)
- Uses Win32 File Mapping API (Microsoft)
- Part of the simple_ library collection for Eiffel
