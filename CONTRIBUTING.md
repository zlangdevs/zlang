# Contributing to ZLang

## Prerequisites

- **Zig 0.16.0** — exact version required; Zig releases are not backward compatible
- **LLVM 22** — `build.zig` auto-detects via `llvm-config`; headers and libs must be present
- **Flex 2.6+** and **Bison 3.0+**

`build.zig` locates LLVM automatically using `llvm-config`. If detection fails, pass flags manually:

```bash
zig build -Dllvm-lib=LLVM-22 -Dllvm-version-major=22
```

## Project Structure

```
zlang/
├── src/              # Compiler source (Zig + C)
│   ├── main.zig
│   ├── lexer/        # Flex-generated lexer
│   ├── parser/       # Bison-generated parser
│   └── zlx/          # Extension host runtime
├── stdlib/           # Standard library (ZLang)
├── examples/
│   └── tests/        # Test suite
├── tests/
│   └── distro_install_test.sh
├── build.zig
└── run_tests.sh
```

## Building

```bash
zig build                          # debug build → zig-out/bin/zlang
zig build -Doptimize=ReleaseFast   # release build
zig build system-install           # install to /usr/local/lib/zlang + /usr/bin/zlang
```

## Testing

```bash
./run_tests.sh                     # run all tests (builds compiler first)
ZLANG_TEST_JOBS=$(nproc) ./run_tests.sh   # parallel
./run_tests.sh factorial           # filter by name
```

### Test format

Tests are ZLang programs that print `(expected <value>):<actual>` lines:

```zl
fun main() >> i32 {
    @printf("(expected 120):%d\n", factorial(5));
    return 0;
}
```

Compile-fail tests live in `examples/tests/compile_fail/`. Each `.zl` has a sidecar file listing expected diagnostic fragments, one per line:

```
# Expected diagnostic fragments
undefined variable 'x'
```

## Distro install tests

`tests/distro_install_test.sh` builds and runs the test suite inside Docker containers across multiple Linux distributions and architectures.

Run it when changing anything that affects the build environment: LLVM detection logic in `build.zig`, system dependencies, or toolchain requirements.

```bash
# all distros, amd64 only (recommended for local runs)
sudo ./tests/distro_install_test.sh --arch linux/amd64 --parallel 4

# single distro
sudo ./tests/distro_install_test.sh --distro void --arch linux/amd64

# full matrix (amd64 + arm64 + riscv64)
sudo ./tests/distro_install_test.sh --parallel 6

# clean up images when done
sudo ./tests/distro_install_test.sh clean
```

> **Warning:** A full matrix run requires QEMU binfmt support for cross-arch emulation, 60+ GB of disk space for images and build cache, and significant CPU time. Single-arch (`--arch linux/amd64`) is sufficient for most changes and runs in a fraction of the time.

Supported distros: Ubuntu, Debian, Fedora, Arch, Alpine, openSUSE, Void.

## Code style

### Zig

Follow `zig fmt`. No comments unless the why is non-obvious. Explicit error handling with `!` and `try`.

### ZLang

- 4-space indent
- `snake_case` variables, `camelCase` functions
- `??` single-line comments, `... ...` block comments
- Semicolons required

## Submitting changes

```
feat: add range syntax
fix: off-by-one in slice bounds check
docs: document extern keyword
```

PRs target `main`. Keep commits focused; one logical change per commit.

## Extension development

Extensions (`.zlx` packages) add syntax and runtime behavior via the C ABI plugin interface. See `EXTENSIONS_ROADMAP.md` for design details.

## License

Contributions are licensed under GNU GPL v3.0 (see [LICENSE](LICENSE)).
