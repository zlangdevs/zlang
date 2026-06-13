<div align="center">

<img src="https://avatars.githubusercontent.com/u/225277494?s=200&v=4" alt="ZLang logo" width="120" height="120">

# ZLang

[![Zig](https://img.shields.io/badge/Zig-0.16.0-f7a41d?logo=zig&logoColor=white)](https://ziglang.org)
[![LLVM](https://img.shields.io/badge/LLVM-22-262d3a?logo=llvm&logoColor=white)](https://llvm.org)
[![Version](https://img.shields.io/badge/version-0.1.1-blue)](https://github.com/zlangdevs/zlang/releases)
[![Platform](https://img.shields.io/badge/platform-linux--x86__64-555?logo=linux&logoColor=white)](#)
[![License](https://img.shields.io/badge/license-GPL--3.0-green)](LICENSE)
[![Extensions](https://img.shields.io/badge/.zlx-extensible-purple)](EXTENSIONS_ROADMAP.md)

</div>

A small statically-typed systems language that compiles to LLVM IR and then to
a native binary. C-like syntax, a modern type system, real SIMD, first-class
C interop, and no runtime.

```zl
use std
fun main() >> void {
    println("Hello, World!");
}
```

## Why ZLang?

ZLang is small enough to read end to end and ships with the things you actually
want for real work: a flexible type system, direct C interop, first-class SIMD,
guard clauses, expression blocks, and a real error flow. LISP in the
middle of your function as an inline block, because the compiler is built to be
extended.

## Quick start

```sh
zig build system-install
zlang examples/hello_world.zl
zlang examples/hello_world.zl -o hello -optimize
```

`zlang run <file.zl>` builds, executes, and cleans up in one step; the
exit code is propagated to the shell. For exploration, `zlang zli`
drops you into an interactive REPL with `:load` / `:files` / `:quit`.

Zig 0.16, Flex, Bison 3.x, and LLVM/Clang 22 are required. Currently targets
`linux-x86_64`. Check your build with `zlang -version`.

A short tour of the language, from `examples/`:

```zl
fun factorial(n: i32) >> i32 {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}

fun main() >> i32 {
    printf("5! = %d\n", factorial(5));
    return 0;
}
```

Pointers, arrays, SIMD, expression blocks, and guard clauses — same shape as
C, fewer footguns:

```zl
fun divide(a: i32, b: i32) when (b != 0) >> i32 {
    return a / b;
}

i32 score = <i32> {
    i32 base = 40;
    i32 bonus = 2;
    base + bonus
};

simd<f32, 4> v = {1.0, 2.0, 3.0, 4.0};
```

C interop is just a `@` prefix or `wrap` declarations:

```zl
wrap printf(format: ptr<u8>) >> i32;
fun @malloc(size: u64) >> ptr<void>;

fun main() >> i32 {
    ptr<i32> buf = @malloc(100) as ptr<i32>;
    *buf = 42;
    printf("%d\n", *buf);
    return 0;
}
```

## Error flow

ZLang doesn't have exceptions or `Result` types. It has two explicit,
resumable signal channels declared at the language level: a callee either
**notifies** the caller that something happened, or **solicits** the
caller's help to keep going. The caller can ignore them, handle them, or
mutate the callee's local state to fix the problem in place.

Errors are named up front:

```zl
error FileNotFound = 1;
error FileLocked = _;    ?? _ assigns an automatic unique code
```

A function `send`s a signal to report an event. The caller catches it with
`on` at the call site:

```zl
fun open_file(path: ptr<u8>) >> i32 {
    if path == null { send FileNotFound; }
    return 0;
}

fun main() >> i32 {
    open_file(null)
        on FileNotFound { println("not found"); }
        on _            { println("something else"); };
    return 0;
}
```

`solicit` is the unusual one. The callee pauses mid-expression, the caller
gets to inspect and **rewrite the callee's locals**, and execution continues
inside the callee — no propagation, no rethrow, no return-trip:

```zl
error NeedSeed = _;

fun next_value() >> i32 {
    i32 seed = 0;
    solicit NeedSeed;          ?? caller decides what seed should be
    return seed + 41;
}

fun main() >> i32 {
    i32 out = next_value() on solicit NeedSeed {
        seed = 1;              ?? we reach into next_value's local `seed`
    };
    printf("%d\n", out);      ?? 42
    return 0;
}
```

`solicit` is its own channel: `on` doesn't catch it, and `on solicit`
doesn't catch `send`. Handlers attach at the call expression and match by
name, by code, or by `_`. See the full reference in
[DOCUMENTATION.md](DOCUMENTATION.md#error-flow).

## Extensions (`.zlx`)

The compiler is modular. Brainfuck, Lisp, threading, file-type handlers —
all live as separate `.zlx` packages. Install, list, and load them through
the compiler itself:

```sh
zlang module install ./brainfuck.zlx
zlang module list
zlang module load-order    ?? topological order of installed plugins
zlang module dev ./myext   ?? auto-reload on change (Linux)
zlang module help
```

Once installed, plugin syntax is just a normal block in your source:

```zl
fun main() >> i32 {
    brainfuck {
        ++++++++[>++++++++<-]>+.
    }
    return 0;
}
```

The work's in progress, see [EXTENSIONS_ROADMAP.md](EXTENSIONS_ROADMAP.md).

## Installation

**Arch Linux (AUR):**

```sh
# AppImage — no build needed, comes with bundled stdlib
yay -S zlang-bin

# Build from source, tracks latest git
yay -S zlang-git
```

- [`zlang-bin`](https://aur.archlinux.org/packages/zlang-bin) — pre-built AppImage, installs as `/usr/bin/zlang`
- [`zlang-git`](https://aur.archlinux.org/packages/zlang-git) — builds from source, requires LLVM + Zig 0.16

**Build from source:**

See [INSTALL.md](INSTALL.md) for full instructions, dependency lists per distro, and verified platforms.

```sh
git clone https://github.com/zlangdevs/zlang
cd zlang
zig build system-install
```

## Architecture

```
.zl source  →  Flex lexer  →  Bison GLR parser  →  AST (Zig)
            →  zlx preprocessor (extension blocks)
            →  Zig LLVM IR generator  →  clang/llvm-toolchain  →  native binary
```

Frontend is Flex + Bison GLR. Backend is a custom Zig generator on top of
the LLVM C API. Extensions are `.zlx` packages with a stable C ABI v6 and
host-side `dlopen` with explicit handshake. Core language keywords are
reserved by the host; in v6, attempting to `register_syntax_block` a
core keyword name returns `ZLANG_REGISTER_RESERVED` — use
`register_keyword_block` to override on purpose.

## Documentation

The full language reference, stdlib, error flow, and codegen internals live
in [DOCUMENTATION.md](DOCUMENTATION.md).

For a hands-on walkthrough of the compiler's feature surface as it actually
ships, see [examples/README.md](examples/README.md). It groups the 29
example programs and 67 integration tests by topic (getting started,
stdlib, C interop, modules, the CHIP-8 emulator, plugins) and points at the
test files that double as small language-references.

## In the wild

ZLang projects under the `zlangdevs` org:

- [zltris](https://github.com/zlangdevs/zltris) — Tetris on raylib with a
  network protocol; the largest zlang codebase so far.
- [brainfuck-zlx](https://github.com/zlangdevs/brainfuck-zlx) — `brainfuck { ... }`
  blocks and standalone `.b`/`.bf` files.
- [zlsip](https://github.com/zlangdevs/zlsip) — `lisp { ... }` blocks with
  bidirectional zlang/lisp calls and template macros.
- [raylib-zl](https://github.com/zlangdevs/raylib-zl) — Raylib bindings.
- [ncurses-zl](https://github.com/zlangdevs/ncurses-zl) — ncurses wrapper.
- [zlib](https://github.com/zlangdevs/zlib) — standard library modules.
- [zed-zlang](https://github.com/zlangdevs/zed-zlang) — Zed syntax extension.
- [zlang-vscode](https://github.com/zlangdevs/zlang-vscode) — VS Code syntax
  highlighting (WIP).
- [zl-cloc](https://github.com/zlangdevs/zl-cloc) — cloc fork that counts
  zlang code.

## Contributing

Active development. Open areas: stdlib coverage, more optimization passes,
error message polish, IDE plugins, documentation. Extensions are especially
welcome — write a `.zlx` against the v1 C ABI in `include/` and ship it.

See [CONTRIBUTING.md](CONTRIBUTING.md) for build setup, test conventions, and PR guidelines.

## License

GNU GPL v3.0. See [LICENSE](LICENSE).

---

Empire starts here 😈

Made with ⚡ by the ZLang team
