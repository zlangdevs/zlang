# ZLang

A small systems language that compiles to LLVM IR. C-like syntax,
modern type system, real SIMD, first-class C interop, no runtime.

```zl
fun main() >> i32 {
    @printf("Empire starts here\n");
    return 0;
}
```

[Full documentation](DOCUMENTATION.md) · [Examples](examples/) · [Extension docs](EXTENSIONS_ROADMAP.md)

## Features

- C-like syntax with sane defaults — no semicolons-everywhere, no header dance.
- Native SIMD vectors as first-class types.
- Direct C interop with the `@` prefix; full `wrap` declarations.
- Const pointer safety; mutable/const distinction at the type level.
- Function templates and overloads.
- Guard clauses (`when (...)`) on function definitions.
- Expression blocks: `<type> { ... last_expr }` returns a value.
- Error flow v2: `error`, `send`, `solicit`, `on` handlers (named and numeric).
- Modular `.zlx` extensions — Brainfuck, Lisp, and friends live outside the core.
- Zero runtime. The compiler emits LLVM IR straight to a native binary.

## Quick start

```bash
zig build
./zig-out/bin/zlang examples/hello_world.zl
./zig-out/bin/zlang myprogram.zl -o myprogram -optimize -keepll
./zig-out/bin/zlang wrap mylib.h -o mylib.zl
```

System install / uninstall via `zig build install` / `zig build uninstall`.

### Hello world

```zl
fun main() >> i32 {
    @printf("Hello, World!\n");
    return 0;
}
```

### Factorial

```zl
fun factorial(n: i32) >> i32 {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}

fun main() >> i32 {
    @printf("5! = %d\n", factorial(5));
    return 0;
}
```

## Language tour

### Variables and types

```zl
i32 count = 42;                   ?? Signed:   i8, i16, i32, i64
u64 big = 1000000;                ?? Unsigned: u8, u16, u32, u64
f32 temp = 98.6;                  ?? Floats:   f16, f32, f64
bool flag = true;
ptr<i32> p = &count;              ?? Mutable pointer to mutable data
ptr<const i32> ro = &count;       ?? Read-only through pointer
const ptr<i32> fixed = &count;    ?? Pointer can't be reassigned
arr<i32, 100> numbers;            ?? Fixed-size array
const i32 MAX = 100;
```

Comments use `??` (yes, really).

### Control flow

```zl
if temperature > 100.0 {
    @printf("Too hot\n");
} else if temperature < 0.0 {
    @printf("Freezing\n");
} else {
    @printf("Just right\n");
}

for { @printf("forever\n"); break; }       ?? infinite
for countdown { countdown--; }              ?? while-style
for (i < 10 && i != 5) { i++; }             ?? while + parens
for i32 i = 0; i < 10; i++ { ... }          ?? C-style
```

### Guard clauses

```zl
fun divide(a: i32, b: i32) when (b != 0) >> i32 {
    return a / b;
}
```

### Expression blocks

```zl
i32 score = <i32> {
    i32 base = 40;
    i32 bonus = 2;
    base + bonus
};
```

### Error flow

```zl
error FileNotFound = 1;
error PermissionDenied = 1;
error FileLocked = _;

fun open(path: ptr<u8>) >> i32 {
    if path == "missing" { send FileNotFound; return -1; }
    if path == "protected" { solicit PermissionDenied; }
    return 1;
}

fun main() >> i32 {
    i32 r = open("protected")
        on 1 { @printf("code 1\n"); }
        on solicit PermissionDenied { @printf("solicited\n"); }
        on _ {}
        on solicit _ {};
    return 0;
}
```

Handler forms: `on Name`, `on N`, `on _`, `on solicit Name`, `on solicit N`, `on solicit _`.

### Structs and enums

```zl
struct Point { x i32, y i32, }

struct Person {
    name arr<u8, 50> = "Anonymous",
    age i32 = 0,
    active bool = true
}

enum Status {
    IDLE,
    RUNNING = 100,
    COMPLETED,
    FAILED = 200
}
```

### SIMD

```zl
simd<f32, 4> v1 = {1.0, 2.0, 3.0, 4.0};
simd<f32, 4> v2 = {5.0, 6.0, 7.0, 8.0};
simd<f32, 4> sum = v1 + v2;
v1[2] = 100.0;
```

### Casts and pointers

```zl
i32 x = 42;
f32 y = x as f32;       ?? explicit
f64 z = x as _;         ?? infer target

i32 v = 100;
ptr<i32> p = &v;
*p = 200;
p = p + 1;              ?? arithmetic

ptr<const i32> ro = &v; ?? read-only; reassignment OK
const ptr<i32> kp = &v; ?? mutable value; pointer locked
```

### C interop

```zl
fun @printf(format: ptr<u8>) >> i32;
fun @malloc(size: u64) >> ptr<void>;

fun main() >> i32 {
    ptr<i32> buf = @malloc(100) as ptr<i32>;
    *buf = 42;
    @printf("%d\n", *buf);
    return 0;
}

wrap @some_c_function(x: i32, y: f32) >> i32;
```

### Imports and stdlib

```zl
module app.main;
use net.http;
use std.math;
```

`std.<module>` resolves under `$ZSTDPATH` or `stdlib/` next to the
compiler binary. Shipped modules: `std.math`, `std.random`,
`std.assert`, `std.time`, `std.fs`, `std.io` (and more in
`stdlib/`).

## Numeric literals

```zl
i32 million = 1'000'000;
i64 big = 999'999'999'999;
f32 pi = 3.141'592'653;
i32 hex = 0xFF;
i32 bin = 0b1010;
i32 oct = 0o755;
```

## Extensions (`.zlx`)

ZLang ships with a real extension system. Each extension is a
single-file archive containing a manifest, a native plugin, and
optional plugin-shipped `.zl` modules.

```bash
zlang module install ./brainfuck.zlx
zlang module list
zlang module doctor
zlang module help
```

See `EXTENSIONS_ROADMAP.md` for the full design and v1 ABI. Two
official extensions live as their own repos:

- [brainfuck.zlx](https://github.com/zlangdevs/brainfuck-zlx) — `brainfuck { ... }`
  blocks with `?len?`, `?cell_size?`, `?load?` directives.
- [zlisp.zlx](https://github.com/zlangdevs/zlsip) — `lisp { ... }`
  blocks with `defn`, `let`, `if`, `while`, full bidirectional
  zlang/lisp calls.

Build a plugin against `include/zlang_plugin_api_v1.h`; install
with `zlang module install <file.zlx>`. Use `-no-extensions` for a
build with native plugins disabled, or `-isolated` for a fully
hermetic build.

## Examples

`examples/` covers the basics — hello world, factorial, math demo,
assertions, struct/loop/cast/pointer tests, SIMD. `examples/tests/`
exercises the more recent features.

## Architecture

```
.zl source -> Flex lexer -> Bison GLR parser -> AST (Zig)
           -> zlx preprocessor (extension blocks)
           -> Zig LLVM IR generator -> clang -> native binary
```

- Frontend: Flex + Bison GLR.
- Backend: custom Zig codegen on top of the LLVM C API.
- ABI: System V (WIP).
- Extensions: `.zlx` archive, v1 plugin C ABI, host-side dlopen
  with explicit handshake.

## Building from source

Requires Zig 0.15.2, Flex, Bison 3.x+, LLVM/Clang.

```bash
git clone https://github.com/zlangdevs/zlang
cd zlang
zig build -Doptimize=ReleaseFast
```

## Editor support

Zed syntax extension: <https://github.com/zlangdevs/zed-zlang>.
Install it as a dev extension in Zed.

## Contributing

Active development. Open areas: stdlib coverage, more
optimization passes, error message polish, IDE plugins,
documentation. Extensions especially welcome — write a `.zlx`
against the v1 ABI and link it back.

## License

GNU GPL v3.0. See [LICENSE](LICENSE).

## Why ZLang

- For learning: small enough to read end to end.
- For performance: SIMD plus direct LLVM IR.
- For real work: full C interop, you keep your existing libraries.
- For fun: drop a `brainfuck { ... }` or `lisp { ... }` block in
  the middle of your program and ship it.

---

**Empire starts here** 😈

Made with ⚡ by the ZLang team
