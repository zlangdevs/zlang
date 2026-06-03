# ZLang Documentation

> **Comprehensive language reference for ZLang `0.1.0`.**
> For the project overview, quick start, and pitch, see [`README.md`](README.md).
> For the runtime extension roadmap, see [`EXTENSIONS_ROADMAP.md`](EXTENSIONS_ROADMAP.md).

This document is the canonical reference for the ZLang language as of the
`0.1.0` release. It is organized so that you can read it sequentially, or jump
to a specific section using the table of contents below. Every example in the
distribution can be browsed in [`examples/`](examples/) and the test suite in
[`examples/tests/`](examples/tests/).

---

## Table of Contents

### Getting Started
- [1. Installation & Building](#1-installation--building)
- [2. The ZLang Build](#2-the-zlang-build)
- [3. Hello World](#3-hello-world)
- [4. Multi-File Projects](#4-multi-file-projects)

### Language Reference
- [5. Lexical Structure](#5-lexical-structure)
- [6. Type System](#6-type-system)
- [7. Variables, Constants & Aliases](#7-variables-constants--aliases)
- [8. Operators & Expressions](#8-operators--expressions)
- [9. Control Flow](#9-control-flow)
- [10. Functions](#10-functions)
- [11. Modules & Imports](#11-modules--imports)

### Error Flow
- [12. The Error Flow Model](#12-the-error-flow-model)
- [13. Declaring Errors](#13-declaring-errors)
- [14. `send` vs `solicit`](#14-send-vs-solicit)
- [15. Handlers at the Call Site](#15-handlers-at-the-call-site)
- [16. Handler Capture & Resumption](#16-handler-capture--resumption)
- [17. Diagnostics](#17-diagnostics)

### Memory & Pointers
- [18. The Memory Model](#18-the-memory-model)
- [19. Pointers & Pointer Arithmetic](#19-pointers--pointer-arithmetic)
- [20. Const Pointers](#20-const-pointers)
- [21. Arrays, Structs & Unions](#21-arrays-structs--unions)
- [22. Layout Compatibility with C](#22-layout-compatibility-with-c)

### Standard Library
- [23. `std.assert`](#23-stdassert)
- [24. `std.env`](#24-stdenv)
- [25. `std.fs`](#25-stdfs)
- [26. `std.io`](#26-stdio)
- [27. `std.math`](#27-stdmath)
- [28. `std.mem`](#28-stdmem)
- [29. `std.path`](#29-stdpath)
- [30. `std.random`](#30-strdrandom)
- [31. `std.string`](#31-stdstring)
- [32. `std.thread`](#32-stdthread)
- [33. `std.time`](#33-stdtime)

### C Interoperability
- [34. Declaring External C Functions](#34-declaring-external-c-functions)
- [35. System V ABI Notes](#35-system-v-abi-notes)
- [36. Linking & `#flag` Directives](#36-linking--flag-directives)
- [37. Variadic Functions](#37-variadic-functions)
- [38. Header Wrapper Generation](#38-header-wrapper-generation)

### Advanced Features
- [39. SIMD Vectors](#39-simd-vectors)
- [40. Expression Blocks](#40-expression-blocks)
- [41. Numeric Literals](#41-numeric-literals)
- [42. Compile-Time Constants](#42-compile-time-constants)

### Extension System (zlx)
- [43. Extension Concepts](#43-extension-concepts)
- [44. The `module` CLI Subcommand](#44-the-module-cli-subcommand)
- [45. The Plugin ABI (v6)](#45-the-plugin-abi-v6)
- [46. Building a Custom Extension](#46-building-a-custom-extension)

### Compiler
- [47. CLI Reference](#47-cli-reference)
- [48. The REPL (`zlang zli`)](#48-the-repl-zlang-zli)
- [49. Build Pipeline](#49-build-pipeline)
- [50. Type System Notes](#50-type-system-notes)
- [51. LLVM IR Mapping](#51-llvm-ir-mapping)
- [52. Optimization Passes](#52-optimization-passes)
- [53. Diagnostics Reference](#53-diagnostics-reference)

### Reference
- [54. Best Practices](#54-best-practices)
- [55. Common Patterns](#55-common-patterns)
- [56. Troubleshooting](#56-troubleshooting)
- [57. Future Direction](#57-future-direction)

---

## 1. Installation & Building

ZLang targets **Zig `0.16.0` or newer** and **LLVM 14–21**. The compiler
itself is built with `zig build`, and the resulting `zlang` binary drives
`clang`/`llc` for code generation and linking.

### System requirements

| Requirement | Minimum | Recommended |
|---|---|---|
| Zig | 0.16.0 | latest stable |
| LLVM | 14 | 21 |
| Clang | matching LLVM | 21 |
| CMake | 3.16 | 3.22+ |
| Flex / Bison | 2.6 / 3.0 | latest |

Linux and macOS are first-class. Windows is unmaintained but the toolchain
parses.

### Cloning the repository

```bash
git clone https://github.com/zlangdevs/zlang.git
cd zlang
```

Submodules (if any) are not required for a base build; the ZLex grammar and
the standard library are vendored.

### Building the compiler

```bash
zig build -Doptimize=ReleaseSafe
```

The binary is emitted to `zig-out/bin/zlang`. The `zlang` executable is
self-contained: it locates the standard library relative to its own
location, then defers to LLVM tools for code generation and linking.

For development builds (faster, slower binaries):

```bash
zig build
./zig-out/bin/zlang --help
```

### LLVM selection

The compiler prefers the highest-version `clang`/`llc`/`opt`/`lli` it can
find and falls back through `21`, `20`, `19`, … down to `14`, then to
unversioned tool names. Absolute paths under common install prefixes
(`/opt/llvm-21/bin`, Homebrew, distro layouts) are also checked.

You can force a specific LLVM bin directory:

```bash
ZLANG_LLVM_BIN=/opt/llvm-21/bin zlang main.zl -optimize
```

If you build against a non-default LLVM shared library, override the SONAME
explicitly:

```bash
zig build -Dllvm-lib=LLVM-20
```

A complete build is verified by the integration test runner:

```bash
./run_tests.sh
```

See [`run_tests.sh`](run_tests.sh) for the test list. A passing run
exercises the lexer, parser, semantic analyzer, code generator, linker, and
the full `examples/tests/` suite.

---

## 2. The ZLang Build

A ZLang "build" is a one-shot invocation of the compiler. There is no
separate build tool: the compiler reads its source files, generates LLVM
IR, and shells out to `llc` and `clang` (or `lld`) to produce an
executable. For multi-file projects, all translation units are passed on a
single command line.

```bash
zlang main.zl utils.zl renderer.zl -lGL -lGLU -lm -optimize -o myapp
```

The compiler is invoked as a single process per executable. The pipeline
is described in [§49](#49-build-pipeline).

---

## 3. Hello World

The smallest valid ZLang program:

```zl
use std

fun main() >> i32 {
    std.println("Hello, World!");
    return 0;
}
```

Save this as `hello.zl` and run:

```bash
zlang hello.zl
./output
```

This is the canonical example; see
[`examples/hello_world.zl`](examples/hello_world.zl).

### Walking through the example

- `use std` imports the [`std`](examples/../stdlib/) module family. A bare
  `use std` is equivalent to `use std.*` — every `std.*` submodule becomes
  accessible as `std.<name>`.
- `fun main() >> i32 { … }` declares the program entry point. The return
  type is `i32`; a non-zero value indicates failure to the shell.
- `std.println("Hello, World!")` writes a line to standard output. The
  return value is the number of characters written.

The compiler emits an executable named `output` by default. Override the
name with `-o <name>`.

### A slightly larger example

```zl
use std

fun main() >> i32 {
    i32 n = 5;
    i32 fact = 1;
    for i32 i = 1; i <= n; i++ {
        fact = fact * i;
    }
    std.println("5! = ");
    std.printInt(fact);
    return 0;
}
```

See [`examples/factorial.zl`](examples/factorial.zl) for the
`factorial` test.

---

## 4. Multi-File Projects

Pass multiple source files on the command line:

```bash
zlang main.zl app.zl renderer.zl -o app
```

Or pass a directory; every `*.zl` file inside is included:

```bash
zlang src/ -o app
```

Files share a single namespace defined by their `module` declarations.
Imports are by module path, not by file path, so file layout is free:

```zl
?? main.zl
module app.main;

use app.renderer;
use std

fun main() >> i32 {
    renderer.draw();
    std.println("done");
    return 0;
}
```

```zl
?? renderer.zl
module app.renderer;

fun draw() >> void {
    std.println("renderer: draw()");
}
```

`use app.renderer` imports only `app.renderer`. `use app` imports `app` and
all of its submodules (`app.*`). See
[`examples/multi_file/`](examples/multi_file/) and
[`examples/tests/module_test.zl`](examples/tests/module_test.zl).

---

## 5. Lexical Structure

### 5.1 Comments

ZLang has two comment forms, both borrowed from older traditions:

```zl
?? single-line comment, ends at newline
...
multi-line comment
spans newlines and whitespace
...
```

`??` is the canonical single-line comment. Multi-line `... … ...` blocks
are nestable and may be used to disable large regions of code.

### 5.2 Identifiers

Identifiers begin with a letter or underscore, followed by letters, digits,
or underscores. They are case-sensitive.

```zl
foo    _internal    CamelCase_allowed    x1
```

### 5.3 Keywords

The reserved word set is small and stable:

```
fun   if   else   for   return   break   continue   goto
const struct enum union wrap use
null  as   when   match
error send solicit on
```

A few of these double as contextual markers — see [§10](#10-functions) for
`when` (guard clauses) and [§12](#12-the-error-flow-model) for `error`,
`send`, `solicit`, and `on`.

### 5.4 The `const` modifier

`const` is a context-sensitive modifier with two meanings:

- **On a variable declaration** — prevents reassignment:
  `const i32 x = 10;`
- **On a pointer type** — prevents modification through the pointer:
  `ptr<const i32> p;`

The two can combine: `const ptr<const i32> r;` cannot be reassigned and
cannot be written through. See [§20](#20-const-pointers) for the full
table.

### 5.5 String and character literals

String literals are C-style null-terminated byte arrays. Character literals
are single bytes in single quotes. Escape sequences in both forms are
C-compatible:

| Escape | Meaning | ASCII |
|---|---|---|
| `\n` | newline | 10 |
| `\t` | horizontal tab | 9 |
| `\r` | carriage return | 13 |
| `\'` | single quote | 39 |
| `\"` | double quote | 34 |
| `\\` | backslash | 92 |
| `\0` | null terminator | 0 |

The terminating `\0` is **automatically appended** to a string literal but
**not** to a character array. When you build a string by hand, you must
write the null terminator explicitly. See
[`examples/tests/string_literal_test.zl`](examples/tests/) for examples.

### 5.6 Numeric literals

Numeric literals are described in detail in [§41](#41-numeric-literals).
In short: decimal with `'` separators, `0x`/`0b`/`0o` prefixes for hex,
binary, and octal, and suffixes for explicit types (`i32`, `u64`, `f32`,
`f64`).

---

## 6. Type System

ZLang is statically typed. Types must be known at every use site, either
through an explicit annotation or through inference from an initializer.

### 6.1 Primitive types

| Kind | Types |
|---|---|
| Signed integers | `i8` `i16` `i32` `i64` |
| Unsigned integers | `u8` `u16` `u32` `u64` |
| Floats | `f16` `f32` `f64` |
| Boolean | `bool` (`true` / `false`) |
| Void | `void` (no value) |

A runnable catalogue of primitives appears in
[`examples/tests/int_test.zl`](examples/tests/int_test.zl),
[`examples/tests/float_test.zl`](examples/tests/float_test.zl),
[`examples/tests/bool_test.zl`](examples/tests/bool_test.zl), and
[`examples/tests/bitwise_test.zl`](examples/tests/bitwise_test.zl).

### 6.2 Composite types

```zl
ptr<i32>                 ?? mutable pointer to mutable i32
ptr<const i32>           ?? pointer to const i32 (read-only)
const ptr<i32>           ?? const pointer (cannot be reassigned)
const ptr<const i32>     ?? const pointer to const i32
ptr<ptr<u8>>             ?? pointer to pointer

arr<i32, 100>            ?? fixed-size array of 100 i32
arr<arr<f32, 10>, 5>     ?? 5×10 matrix of f32

simd<f32, 4>             ?? 4-wide float vector (SSE)
simd<i64, 2>             ?? 2-wide signed vector

struct Point { x i32, y i32 }
union  Word  { i i32, u u32, b bool }
enum   Color { RED, GREEN = 7, BLUE }
```

Arrays are value types laid out contiguously in memory. SIMD vectors map
to LLVM vector types and receive the platform's auto-vectorization
treatment when used in arithmetic expressions. See
[`examples/tests/simd_test.zl`](examples/tests/) and
[`examples/tests/union_test.zl`](examples/tests/) for runs.

### 6.3 Type casting

The `as` operator performs an explicit conversion. The target type can be
elided as `_` to let the compiler infer it from context.

```zl
i32 x = 42;
f32 y = x as f32;       ?? explicit
f32 z = x as _;         ?? inferred
```

The valid conversions are:

- integer ↔ integer (truncation or sign/zero extension)
- integer → float (exact or approximate)
- float → integer (truncation toward zero)
- float → float (precision change)
- pointer → pointer (reinterpret, no provenance tracking)
- integer ↔ pointer (unsafe, FFI only)

Casts that lose range or precision emit a warning, not an error.
See [`examples/tests/cast_test.zl`](examples/tests/cast_test.zl).

### 6.4 Truthiness

In any boolean context (`if`, `for`, `&&`, `||`, `!`), a value of any
numeric or pointer type is considered true iff it is non-zero (numerics)
or non-null (pointers). `bool` is the canonical type and is one byte.

```zl
i32 n = 0;
while n {           ?? false at start
    n--;
}
```

---

## 7. Variables, Constants & Aliases

### 7.1 Local variables

```zl
i32 x = 10;          ?? mutable, type inferred from literal
i32 y;               ?? uninitialized, undefined
f64 pi = 3.14;
```

Variables are stack-allocated by default. A declaration without an
initializer leaves the slot undefined; reading it before writing is
undefined behavior.

### 7.2 `const` variables

```zl
const i32 ANSWER = 42;
const f64 PI = 3.14159265;
```

A `const` declaration is a compile-time check that the binding is never
reassigned. The storage class and lifetime are identical to a normal
variable — `const` is not a constant expression in the C sense.

### 7.3 Global variables

Globals follow the same syntax and live for the entire program:

```zl
i32 counter = 0;

fun tick() >> void {
    counter = counter + 1;
}
```

See [`examples/tests/global_test.zl`](examples/tests/global_test.zl).

### 7.4 Type aliases

ZLang does not have a separate `typedef` keyword. To reuse a type, name
the declaration or use a struct. Type aliases (planned for a later
release) will use `type` as a keyword.

---

## 8. Operators & Expressions

### 8.1 Operator catalogue

| Category | Operators |
|---|---|
| Arithmetic | `+` `-` `*` `/` `%` |
| Comparison | `==` `!=` `<` `>` `<=` `>=` |
| Logical | `&&` `\|\|` `!` |
| Bitwise | `&` `\|` `^` `~` `<<` `>>` |
| Assignment | `=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `&=` `\|=` `^=` |
| Increment / decrement | `++` `--` |
| Pointer | `&` (address-of), `*` (dereference) |
| Member | `.` (struct / union field) |
| Cast | `as` |

Precedence and associativity match C, with one addition: `as` sits at
the top of the precedence table and is right-associative.

### 8.2 Expressions and statements

A *statement* is an expression followed by a semicolon, or a control
flow construct. An *expression* is anything that yields a value, including
a bare identifier, a literal, an arithmetic combination, a function call,
or an expression block ([§40](#40-expression-blocks)).

The grammar is C-like: `if`, `for`, `return`, `break`, `continue`, and
`goto` are statements. Function calls, arithmetic, and casts are
expressions. The ZLang-specific `send`, `solicit`, and `on` constructs
are statements that may also appear in expression position — see
[§12](#12-the-error-flow-model).

---

## 9. Control Flow

### 9.1 `if` / `else`

```zl
if condition {
    ...
} else if other {
    ...
} else {
    ...
}
```

The condition may be any truthy expression; an explicit `== 0` is
unnecessary.

### 9.2 `for` loops

ZLang uses a single `for` keyword with C-style three-part syntax:

```zl
for i32 i = 0; i < 10; i++ {
    ...
}

for i32 i = 0; i < n; i++ {
    for i32 j = 0; j < n; j++ {
        if j == 5 { break; }       ?? breaks inner only
    }
}
```

A bare `for cond { ... }` loops while the condition holds. A bare
`for { ... }` loops forever (use `break` to exit).

### 9.3 `break` and `continue`

`break` exits the innermost enclosing loop. `continue` jumps to the next
iteration. Both apply only to the *nearest* loop, mirroring C.

### 9.4 `goto` and labels

```zl
fun example() >> i32 {
    i32 x = 10;
    goto skip;
    x = 20;          ?? skipped
    skip:
    return x;
}
```

Labels are identifiers followed by a colon. Forward and backward jumps
within a function are both legal. Jumping into a block scope is
permitted; jumping out of a function is not.

`goto` is the recommended idiom for cleanup in pre-error-flow code:

```zl
fun process_file(path: ptr<u8>) >> i32 {
    ptr<void> f = @fopen(path, "r");
    if f == null { goto error; }
    ptr<void> buf = @malloc(1024);
    if buf == null { goto cleanup_file; }

    ?? ... work ...

    @free(buf as ptr<void>);
    cleanup_file:
    @fclose(f);
    return 0;
    error:
    return -1;
}
```

`goto` is the only control-flow construct that does **not** run
`defer` statements in scopes it jumps out of, so the
goto-to-cleanup-label pattern is still the correct idiom when
cleanup must run on an error path that does not return through
the rest of the function. For ordinary cleanup where control
flow does fall through or return, prefer `defer` (see
[§9.6](#96-defer)) — the same code is much shorter and is
automatically correct on every exit path:

```zl
fun process_file(path: ptr<u8>) >> i32 {
    ptr<void> f = @fopen(path, "r");
    if f == null { return -1; }
    defer @fclose(f);

    ptr<void> buf = @malloc(1024);
    if buf == null { return -1; }
    defer @free(buf);

    ?? ... work ...
    return 0;
}
```

For error reporting (as opposed to cleanup), use the error flow
model — see [§12](#12-the-error-flow-model).
A runnable test of `goto` is in
[`examples/tests/goto_test.zl`](examples/tests/goto_test.zl).

### 9.5 `match` statements

`match` dispatches on a value. Each arm lists the literal values it
matches, separated by commas:

```zl
fun classify(a: i32) >> i32 {
    i32 result = 0;

    match a + 1 {
        0 {
            result = -1;
        }
        1, 2, 3 {
            result = 3;
        }
        99 {
            result = 99;
        }
    }

    return result;
}
```

A `match` arm that omits the block defaults to running the next arm
implicitly; a block body is recommended for clarity. When the operand
is an `enum`, the arms must be exhaustive — falling off the end of a
non-exhaustive `match` over an enum is a compile error. See
[`examples/tests/match_test.zl`](examples/tests/match_test.zl) for a
runnable test.

### 9.6 `defer`

A `defer` statement schedules a single expression to run when the
current scope exits. The syntax is just `defer <expression>;`:

```zl
fun acquire() >> ptr<void> { ... }
fun release(p: ptr<void>) >> void { ... }

fun with_resource() >> i32 {
    ptr<void> r = acquire();
    defer release(r);
    ?? ... use r ...
    if something_went_wrong() {
        return -1;  ?? release(r) still runs.
    }
    ?? ... more work ...
    return 0;  ?? and here too.
}
```

Three rules govern `defer`:

1. **LIFO order.** If multiple defers are registered in the same
   scope, they fire in reverse order of registration — the last
   one registered runs first. This is the same rule Go, Zig, and
   Swift use.
2. **Capture by reference.** The deferred expression is evaluated
   at the moment the scope exits, not at the moment the `defer` is
   reached. The expression sees the current value of every
   variable it references. The `defer` itself is reached and
   registered as soon as control flow passes it.
3. **Scope-bound.** A `defer` only fires when *its* scope ends.
   A `defer` inside a conditional arm, match arm, or loop body
   fires when that arm or body exits; a `defer` at function level
   fires when the function returns (whether by an explicit
   `return`, by falling off the end, or by an error path that
   returns).

Where `defer` fires:

- Function return — including the implicit return at the end of a
  `fun` that lacks one.
- `if` / `else` arm exit, both the taken and the not-taken path.
- `match` arm exit, for the arm that was selected.
- `for` / `while` / `for (...)` loop body exit (including `break`).
- `on` error handler exit.
- Expression-block termination.

Where `defer` does **not** fire in v0.1.0:

- `goto`. A `goto` is a low-level transfer of control: if you
  `goto` out of a scope, defers registered inside that scope are
  skipped. If you `goto` into a scope, defers in inner scopes of
  the target are not affected. Code that mixes `goto` and
  resource cleanup is the canonical case for the goto-to-label
  pattern shown in [§9.4](#94-goto-and-labels) — defer is *not*
  safe there.

A second codegen quirk is worth knowing about even though it is
not a `defer` rule per se: when an `on` handler absorbs a `send`,
the join point after the handler is treated as a scope-exit point
by the defer infrastructure. In practice, an `on`-handled call
will cause defers registered *before* the call to fire once, even
if the handler did not actually run (i.e. the call returned
normally without sending). The defer does not re-fire when the
function returns later. See
[`defer_comprehensive_test.zl`](examples/tests/defer_comprehensive_test.zl)
test 7 for an executable demonstration and the expected trace
order.

A runnable demonstration of every rule above is in
[`examples/tests/defer_test.zl`](examples/tests/defer_test.zl)
(basic if/return) and
[`examples/tests/defer_loop_test.zl`](examples/tests/defer_loop_test.zl)
(for/continue/break). A more comprehensive test that also covers
LIFO ordering, capture by reference, scope boundaries, `match`
arms, `on` handlers, and accumulation across loop iterations is
in
[`examples/tests/defer_comprehensive_test.zl`](examples/tests/defer_comprehensive_test.zl).

---

## 10. Functions

### 10.1 Declaration

```zl
fun name(p1: T1, p2: T2) >> ReturnType {
    ?? body
    return value;
}
```

The return type follows the `>>` arrow. A function with no meaningful
return value uses `void`; such functions may omit the `return` or use
a bare `return;`.

```zl
fun greet() >> void { std.println("hi"); }
fun add(a: i32, b: i32) >> i32 { return a + b; }
fun modify(v: ptr<i32>) >> void { *v = 100; }
fun print_point(p: Point) >> void { ?? ... }
```

### 10.2 Parameters and calling convention

Parameters are passed by value. To pass by reference, take a pointer:

```zl
fun swap(a: ptr<i32>, b: ptr<i32>) >> void {
    i32 t = *a; *a = *b; *b = t;
}
```

Small structs (≤16 bytes on x86_64 SysV) are passed in registers; large
structs are passed by hidden pointer. This is described in
[§35](#35-system-v-abi-notes).

### 10.3 Guard clauses

A function may declare a guard that runs before the body:

```zl
fun divide(a: i32, b: i32) when (b != 0) >> i32 {
    return a / b;
}
```

If the guard expression evaluates to false, the function fails fast. The
guard can reference any parameter and any name in scope. See
[`examples/tests/guard_clause_test.zl`](examples/tests/guard_clause_test.zl).

### 10.4 Variadic functions

See [§37](#37-variadic-functions) for `vararg<_>` (C-style, ABI-compat)
and `vararg<T>` (typed, ZLang-only).

### 10.5 Recursion

Functions may call themselves or any other function defined in scope.
There is no tail-call optimization guarantee.

```zl
fun fib(n: i32) >> i32 {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
```

### 10.6 Function templates

ZLang supports compile-time generic functions with bracketed type
parameters:

```zl
fun identity[T](v: T) >> T { return v; }
fun first[A, B](a: A, b: B) >> A { return a; }
```

Type parameters are inferred at the call site. Generic type *declarations*
(e.g. `struct List[T]`) are not yet supported — see
[§57](#57-future-direction).

### 10.7 Function overloading

Functions can be overloaded by their parameter types:

```zl
fun add(a: i32, b: i32) >> i32 { return a + b; }
fun add(a: f32, b: f32) >> f32 { return a + b; }
```

Overload resolution is by exact type match; no implicit conversions are
considered.

### 10.8 Function pointers

A function type can be written inline as `fun(args) >> ret`. The
resulting value can be passed to and returned from other functions,
and stored in variables:

```zl
fun apply(f: fun(i32, i32) >> i32, a: i32, b: i32) >> i32 {
    return @__zinterp(f, a, b);
}
```

To *call* a function-typed value, use the `@__zinterp` intrinsic. It
takes the function value followed by the call's arguments and lowers
to an indirect call. Function pointers may not be cast to or from
`ptr<u8>` in this release.

---

## 11. Modules & Imports

Every `.zl` file may declare a module name:

```zl
module app.net.http;
```

The module name is independent of the file path; the compiler maps the
file into the program through the `use` statements of other files.

```zl
use std                    ?? imports std.*
use std.io                 ?? imports std.io and its members
use std.io.println         ?? imports a single symbol
use app.net.http as http   ?? imports under an alias
```

`use std` (no dot) imports the whole `std` tree. `use std.io` imports
`std.io` and its symbols, but not `std.fs`, `std.math`, etc. See
[`examples/tests/module_test.zl`](examples/tests/module_test.zl) and
[`examples/tests/module_alias_test.zl`](examples/tests/module_alias_test.zl).

---

## 12. The Error Flow Model

> **ZLang replaces the throw/catch tradition with two explicit, resumable
> signal channels: `send` and `solicit`.** Both are checked at compile
> time: unhandled `send`s and `solicit`s produce warnings at the call
> site.

Errors are not exceptions, not return values, and not panic-style aborts.
They are ordinary statements in the body of a function. A handler is
attached to the *call expression* at the call site, and the handler is
the unit of control transfer.

The two channels are completely independent:

- `send` means *"the callee finished (possibly unsuccessfully) — here's
  the reason; let the caller continue."*
- `solicit` means *"the callee cannot continue without help from the
  caller; pause and ask."*

A handler for one channel does not catch the other.

This is the same model described in the README's
[Error flow section](README.md#error-flow). The sections below cover it
in detail.

---

## 13. Declaring Errors

Errors are declared with the `error` keyword. They have a name and an
integer code:

```zl
error FileNotFound = 1;
error PermissionDenied = 10;
error CustomError = _;             ?? _ = auto-assigned unique code
```

Three forms:

| Form | Meaning |
|---|---|
| `error Name = N;` | explicit integer code |
| `error Name = OtherName;` | alias of another error's code |
| `error Name = _;` | compiler assigns a unique code |

Aliases share a code with their referent. This matters for numeric
handlers ([§15](#15-handlers-at-the-call-site)) and for matching by
code in the semantic pass.

Error declarations are first-class top-level statements, not declarations
in a special block. They are visible in their declaring module and any
module that `use`s it.

---

## 14. `send` vs `solicit`

### 14.1 `send`

```zl
error DiskFull = 7;

fun write(buf: ptr<u8>, n: u64) >> void {
    if n > 4096 {
        send DiskFull;
        return;
    }
    ?? ... write ...
}
```

A `send` is a *terminal* signal for the current invocation. Once sent,
control flow does not return to the call site of the sender — the
nearest enclosing handler takes over. If the call expression has no
matching handler, the semantic pass emits an
*unhandled-error* warning.

### 14.2 `solicit`

```zl
error NeedSeed = _;

fun next_value() >> i32 {
    i32 seed = 0;
    solicit NeedSeed;
    return seed + 41;
}
```

A `solicit` is a *suspension* point. Control returns to the call site,
the handler runs, and then execution resumes immediately after the
`solicit` statement — as if the function were paused. The handler may
mutate local state in the caller, the callee, or both; see
[§16](#16-handler-capture--resumption) for the rules.

`solicit` is what makes ZLang's error flow *resumable*: the callee
re-evaluates the `solicit` line on resume, not from the top of the
function. There is no "exception unwind" — the stack frame is intact.

### 14.3 Channel separation

A handler for `send` does not catch `solicit` and vice versa:

```zl
i32 x = foo() on MyError { ... }              ?? catches send MyError only
i32 y = bar() on solicit MyError { ... }      ?? catches solicit MyError only
```

This is deliberate: `send` is final and `solicit` is resumable, and
mixing them in a single handler would obscure the intent.

---

## 15. Handlers at the Call Site

A handler is appended directly to a call expression. It uses the `on`
keyword and matches by error name, error code, or wildcard:

```zl
i32 res = open_file("data", false)
    on FileNotFound { return -1; }
    on 1            { return -1; }       ?? matches code 1 (FileNotFound)
    on _            { return -1; }       ?? catch-all
    on solicit PermissionDenied { ask_user(); }
    on solicit 10  { ... }
    on solicit _   { ... };
```

The six forms:

| Form | Catches |
|---|---|
| `on Name { ... }` | `send Name` |
| `on N { ... }` | `send` with code `N` |
| `on _ { ... }` | any `send` from this call |
| `on solicit Name { ... }` | `solicit Name` |
| `on solicit N { ... }` | `solicit` with code `N` |
| `on solicit _ { ... }` | any `solicit` from this call |

A handler may itself contain `send` or `solicit` statements, or call
other functions. A handler may not contain another `on` block at this
nesting level — handlers are flat per call.

Numeric handlers are the right tool when several error names share a
code (e.g. aliases of `FileNotFound`).

### 15.1 Expression-position handlers

Handlers may be attached to a call used as a sub-expression:

```zl
bool err = false;
i32 res = handler_test() on _ {
    err = true;
};
```

The return value of the call (here, `7`) is still bound to `res`. The
handler does not change the value; it only decides whether subsequent
control flow runs the handler body or falls through.

A runnable example is in
[`examples/tests/handler_capture_test.zl`](examples/tests/handler_capture_test.zl).

### 15.2 A worked example: `solicit` with state repair

```zl
error NeedSeed = _;

fun next_value() >> i32 {
    i32 seed = 0;
    solicit NeedSeed;
    return seed + 41;
}

fun main() >> i32 {
    i32 out = next_value() on solicit NeedSeed {
        seed = 1;
    };
    std.println("got value:");
    std.printInt(out);   ?? 42
    return 0;
}
```

The handler mutates the callee's `seed` local — the call resumes and
reads the new value. This pattern is essential for systems where a
callee cannot proceed without caller-supplied state (a key, a buffer, a
configuration value). See
[`examples/tests/solicit_state_repair_test.zl`](examples/tests/solicit_state_repair_test.zl)
and
[`examples/tests/solicit_callee_mutation_test.zl`](examples/tests/solicit_callee_mutation_test.zl).

---

## 16. Handler Capture & Resumption

When a `solicit` handler runs, the compiler must decide which variables
the handler body can see and how it can affect the callee's locals. The
rules are:

- **Read-only by value.** A variable that the handler only reads is
  captured *by value* — the handler sees a snapshot.
- **Mutated by reference.** A variable that the handler writes to is
  captured *by reference* — the change is visible to the callee on
  resume.
- **Hybrid by usage.** The compiler looks at the handler body to decide
  the capture mode per variable. A variable that is read in one handler
  and written in another (on the same call) is captured by reference.
- **No cross-frame aliasing.** A handler may not take the address of a
  callee local and keep it past the call's return; the address is valid
  only for the duration of the handler.

These rules are checked at compile time. The semantic pass rejects
captures that would require cross-function escape. See
[`examples/tests/handler_capture_byref_bool_test.zl`](examples/tests/handler_capture_byref_bool_test.zl)
and
[`examples/tests/handled_call_expression_test.zl`](examples/tests/handled_call_expression_test.zl).

After a `solicit` handler returns, the callee resumes on the line
*after* the `solicit` statement. The callee sees any writes the
handler made and re-runs from the suspension point.

---

## 17. Diagnostics

The compiler emits two error-flow warnings at the call site:

- **Unhandled possible error** — a call can `send` an error that has no
  matching `on` block. Either add a handler or restructure the call.
- **Dead handler** — an `on` block matches nothing the callee can
  produce. This usually means a typo in the error name or a stale
  handler after a refactor.

Both warnings are opt-in for warning tests:
[`examples/tests/warning/`](examples/tests/warning/) holds the expected
text for each diagnostic.

---

## 18. The Memory Model

ZLang exposes the platform's C-compatible memory model. There is no
implicit heap, no garbage collector, and no borrow checker. The
programmer allocates, owns, and frees explicitly.

### 18.1 Stack allocation

Local variables and arrays are stack-allocated by default:

```zl
i32 x = 42;
arr<i32, 100> buf;
Point p = {10, 20};
```

Stack frames are entered on function call and torn down on return. A
function returning a pointer to its own local produces a dangling
pointer.

### 18.2 Heap allocation

Heap allocation goes through C's `malloc` / `calloc` / `realloc` /
`free`, declared as `wrap` functions:

```zl
wrap @malloc(size: u64) >> ptr<void>;
wrap @free(p: ptr<void>) >> void;

ptr<i32> buf = @malloc(100 * 4) as ptr<i32>;
buf[0] = 42;
@free(buf as ptr<void>);
```

Allocation is unchecked: a `null` return must be tested by the caller.
A canonical example is in
[`examples/tests/malloc_test.zl`](examples/tests/) — search the suite
for `malloc` and `free`.

### 18.3 The C runtime

ZLang links against the host C runtime by default. `printf`, `malloc`,
`sin`, `fopen`, and any other libc symbol are available. Calling a libc
function that has not been declared is a link error.

---

## 19. Pointers & Pointer Arithmetic

```zl
arr<i32, 10> a;
ptr<i32> p = &a[0];     ?? address-of
i32 x = *p;             ?? dereference
p = p + 1;              ?? pointer arithmetic
i32 y = *(p + 2);
```

Pointer arithmetic is permitted on `ptr<T>` for any `T`; the compiler
multiplies the offset by `sizeof(T)`. The type of `p + 1` is `ptr<T>`,
and `*(p + n)` yields a value of type `T`. Negative offsets and
`p - q` (pointer difference) are not yet supported.

---

## 20. Const Pointers

`const` on a pointer type is the C/C++ model:

| Type | Reassign pointer | Write through |
|---|---|---|
| `ptr<T>` | yes | yes |
| `ptr<const T>` | yes | **no** |
| `const ptr<T>` | **no** | yes |
| `const ptr<const T>` | **no** | **no** |

`const` on a *value* declaration — `const i32 x = 5;` — prevents
reassignment of the local. See
[`examples/tests/const_pointer_test.zl`](examples/tests/) for the
canonical runs.

Compound assignments (`+=`, `*=` etc.) through a `ptr<const T>` are
compile errors. Pointer arithmetic on `ptr<const T>` is allowed
because moving the address does not modify the pointee.

---

## 21. Arrays, Structs & Unions

### 21.1 Arrays

`arr<T, N>` is a fixed-size, stack-allocated array of `N` elements of
type `T`. Indexing is bounds-checked only in debug builds; the release
build trusts the program.

```zl
arr<i32, 10> a;
a[0] = 1;
i32 x = a[9];
```

Arrays can be initialized with a brace list:

```zl
arr<i32, 4> primes = {2, 3, 5, 7};
```

### 21.2 Structs

```zl
struct Point {
    x i32,
    y i32,
    label ptr<u8> = null
}
```

Field order is the layout order. A field may have a default value with
`= expr`. Struct literals use brace syntax; missing fields take their
defaults:

```zl
Point p = {10, 20, "origin"};
Point q = {10, 20};          ?? label defaults to null
```

### 21.3 Unions

```zl
union Word {
    i i32,
    u u32,
    b bool
}
```

Unions share storage; the size and alignment are those of the largest
member. The "active" member is the programmer's responsibility to
track — usually with a tag field. See
[§55](#55-common-patterns) for the tagged-union idiom.

---

## 22. Layout Compatibility with C

ZLang follows the platform C ABI for:

- struct layout (declaration order, alignment, padding)
- union layout (overlapping at offset 0)
- pointer representation
- integer and float representation
- function calling convention (within the constraints of [§35](#35-system-v-abi-notes))

This means a ZLang struct can be passed to or returned from a C function
without conversion, and a ZLang pointer can alias a C pointer to the
same layout. Layout details are platform-dependent and described in
[§35](#35-system-v-abi-notes).

---

## 23. `std.assert`

> Module: [`stdlib/assert.zl`](stdlib/assert.zl)

A small set of assertion macros that print a diagnostic and abort on
failure. `assert` takes a condition; `assert_eq` compares two values for
equality with a human-readable format.

```zl
use std

fun main() >> i32 {
    std.assert(2 + 2 == 4);
    std.assert_eq(2 + 2, 4, "two plus two");
    return 0;
}
```

---

## 24. `std.env`

> Module: [`stdlib/env.zl`](stdlib/env.zl)

Wrappers around `getenv`, `setenv`, and `unsetenv`. See the file
header for the exact signatures.

---

## 25. `std.fs`

> Module: [`stdlib/fs.zl`](stdlib/fs.zl)

Thin wrappers over `fopen`, `fclose`, `fread`, `fwrite`, `fseek`,
`ftell`, and friends. Returns C-compatible `ptr<FILE>` / `ptr<void>`
handles.

---

## 26. `std.io`

> Module: [`stdlib/io.zl`](stdlib/io.zl)

The I/O module provides formatted printing and line-based reading:

| Function | Purpose |
|---|---|
| `print(text: ptr<u8>) >> i32` | write a string, no newline |
| `println(text: ptr<u8>) >> i32` | write a string and `\n` |
| `printInt(value: i32) >> i32` | decimal integer |
| `printFloat(value: f64) >> i32` | decimal float |
| `eprint(text: ptr<u8>) >> i32` | print to stderr, no newline |
| `eprintln(text: ptr<u8>) >> i32` | print to stderr with newline |
| `readLine(buffer: ptr<u8>, max_len: i32) >> i32` | read up to a line from stdin |
| `printf(fmt: ptr<u8>, args: vararg<_>) >> i32` | pass-through to libc `printf` |

The `print*` family is what the README's "Hello World" example uses:

```zl
use std

fun main() >> i32 {
    std.println("Hello, World!");
    return 0;
}
```

---

## 27. `std.math`

> Module: [`stdlib/math.zl`](stdlib/math.zl)

Wraps `<math.h>`: `sqrt`, `cbrt`, `pow`, `exp`, `log`, `log2`, `log10`,
trig (`sin`, `cos`, `tan` and inverses), hyperbolic (`sinh`, `cosh`,
`tanh`), and a handful of constants (`PI`, `E`, `TAU`, `SQRT2`, `SQRT3`,
`PHI`). All functions take and return `f64`.

```zl
use std

fun main() >> i32 {
    std.println("sqrt(2) = ");
    std.printFloat(std.math.sqrt(2.0));
    return 0;
}
```

The module adds `#flag -lm` to the link line; you do not need `-lm` on
the command line.

---

## 28. `std.mem`

> Module: [`stdlib/mem.zl`](stdlib/mem.zl)

A thin layer over `malloc`, `calloc`, `realloc`, and `free` with
`std.mem` types and a few convenience helpers (zero-initialization,
size-checked copies). All allocations are unchecked: test for `null`
on the way out.

---

## 29. `std.path`

> Module: [`stdlib/path.zl`](stdlib/path.zl)

Path manipulation: joining, splitting, basename, dirname, extension. All
functions operate on `ptr<u8>` byte arrays and produce new byte arrays;
the caller is responsible for the lifetime of the inputs and outputs.

---

## 30. `std.random`

> Module: [`stdlib/random.zl`](stdlib/random.zl)

| Function | Purpose |
|---|---|
| `seed()` | seed from current time |
| `randRange(min: i32, max: i32) >> i32` | uniform integer in `[min, max]` |
| `randFloat() >> f64` | uniform in `[0, 1)` |
| `randBool() >> bool` | true or false |

A simple `XorShift` struct is also defined for deterministic streams.

---

## 31. `std.string`

> Module: [`stdlib/string.zl`](stdlib/string.zl)

C-string helpers: `strlen`, `strcpy`, `strcat`, `strcmp`, `strchr`,
`strstr`, `strdup`, `atoi`, plus a `strEq(a, b) >> bool` convenience
and an `itoa(i32) >> ptr<u8>` for decimal formatting (returns a
pointer to a 16-byte internal buffer; copy if you need it longer-lived).

---

## 32. `std.thread`

> Module: [`stdlib/thread.zl`](stdlib/thread.zl)

Provided by the `threading` extension; not part of the base standard
library. The extension adds the following when enabled:

| Function | Purpose |
|---|---|
| `threadCreate(out: ptr<Thread>, entry: ptr<fun(ptr<void>) >> ptr<void>>, arg: ptr<void>) >> bool` | spawn a thread running `entry(arg)`; result written to `out` |
| `threadJoin(t: Thread) >> bool` | block until thread `t` exits |
| `threadDetach(t: Thread) >> bool` | release the thread's resources when it exits |
| `mutexInit(m: ptr<Mutex>) >> bool` | initialize a 40-byte raw mutex storage |
| `mutexDestroy(m: ptr<Mutex>) >> bool` | release the mutex |
| `mutexLock(m: ptr<Mutex>) >> bool` | acquire |
| `mutexUnlock(m: ptr<Mutex>) >> bool` | release |
| `condInit(c: ptr<CondVar>) >> bool` | initialize a 48-byte condition variable |
| `condDestroy(c: ptr<CondVar>) >> bool` | release the condition variable |
| `condWait(c: ptr<CondVar>, m: ptr<Mutex>) >> bool` | atomically unlock `m` and block on `c` |
| `condSignal(c: ptr<CondVar>) >> bool` | wake one waiter |
| `condBroadcast(c: ptr<CondVar>) >> bool` | wake all waiters |

The `Mutex` and `CondVar` types are raw fixed-size storage; both must
be zero-initialized (e.g. as a global or with `memset`) before the
`*Init` call. The extension links `-lpthread` automatically on Linux.

---

## 33. `std.time`

> Module: [`stdlib/time.zl`](stdlib/time.zl)

| Function | Purpose |
|---|---|
| `now() >> i64` | seconds since Unix epoch |
| `nowMillis() >> i64` | seconds × 1000 (approximate) |
| `clock() >> i64` | CPU clock ticks |
| `sleep(seconds: u32) >> u32` | sleep |
| `usleep(usec: u32) >> i32` | microsecond sleep |

Time-formatting helpers (`gmtime`, `localtime`, `strftime`, `asctime`,
`ctime`, `mktime`, `difftime`) wrap the libc functions with
`ptr<u8>`-based interfaces that return into static libc buffers.

---

## 34. Declaring External C Functions

A function whose name begins with `@` is a declaration of an external C
symbol. The leading `@` is dropped at link time:

```zl
fun @printf(fmt: ptr<u8>, args: vararg<_>) >> i32;
fun @malloc(size: u64) >> ptr<void>;
fun @strcpy(dest: ptr<u8>, src: ptr<u8>) >> ptr<u8>;
```

A `wrap` declaration does the same, but emits the call through a thin
adapter to handle ABI edge cases (struct returns, large aggregates, etc.):

```zl
wrap @some_c_lib_function(x: i32, y: f32) >> ptr<void>;
```

The `wrap` keyword is the safer default for unknown libraries; the bare
`fun @name` form is for direct calls where the ABI is known to match.

### 34.1 Custom wrappers

To do work around a C call, declare a local function and a matching
`@` external:

```zl
fun @cfunction() >> i32;

fun cfunction() >> i32 {
    ?? setup
    return @cfunction();
}
```

---

## 35. System V ABI Notes

ZLang implements the System V AMD64 ABI for Linux and the analogous
Itanium C++ ABI on other platforms, with the following nuances:

- **Small aggregates (≤16 bytes)** are passed in registers and
  returned in registers. No special handling is required.
- **Large aggregates (>16 bytes)** are returned via a hidden pointer
  (`sret`). To call a C function that returns a large struct by value,
  declare a wrapper that takes the hidden pointer explicitly:

  ```zl
  fun @LoadImage(result: ptr<Image>, path: ptr<u8>) >> void;

  fun LoadImage(path: ptr<u8>) >> Image {
      Image result;
      @LoadImage(&result, path);
      return result;
  }
  ```
- **Struct arguments** follow the same rules: small structs in
  registers, large structs by hidden pointer.

The `wrap` keyword handles these cases automatically.

---

## 36. Linking & `#flag` Directives

### 36.1 Link flags on the command line

```bash
zlang main.zl -lm            ?? link libm
zlang main.zl -lGL -lGLU     ?? link OpenGL
zlang main.zl -L/opt/lib -lraylib
zlang main.zl -Wl,-rpath,/usr/local/lib
```

`-l<name>` adds `-l<name>` to the link line. `-L<path>` adds a library
search path. `-Wl,<option>` is passed through to the linker.

### 36.2 `#flag` directives in source

A `#flag` directive inside a ZLang source file adds flags to the
link line *for that translation unit*:

```zl
module std.math;
#flag -lm
```

This is how `stdlib/math.zl` self-attaches the math library: any program
that uses `std.math` does not need to remember `-lm` on the command
line.

`#flag` is a stable extension point. Plugin authors are encouraged to
use it to attach native libraries their extension depends on.

---

## 37. Variadic Functions

### 37.1 Untyped: `vararg<_>`

For C compatibility, the untyped form uses the platform `va_list`:

```zl
wrap @printf(fmt: ptr<u8>, args: vararg<_>) >> i32;
```

Inside a function, declare the `va_list` struct (x86_64 shown) and use
the three intrinsics:

```zl
struct va_list {
    gp_offset u32,
    fp_offset u32,
    overflow_arg_area ptr<void>,
    reg_save_area ptr<void>
}

fun print_n(count: i32, args: vararg<_>) >> void {
    va_list vl;
    @va_start(&vl);
    for i32 i = 0; i < count; i++ {
        i32 v = @va_arg(&vl, "i32");
        std.printInt(v);
        std.print(" ");
    }
    @va_end(&vl);
    std.println("");
}
```

Supported `@va_arg` types: `i8` `i16` `i32` `i64` `u8` `u16` `u32`
`u64` `f32` `f64` and `ptr<T>`.

### 37.2 Typed: `vararg<T>`

For ZLang-only functions, the typed form is simpler:

```zl
fun sum(nums: vararg<i32>) >> i32 {
    i32 total = 0;
    i32 n = @vararg_len(nums);
    for i32 i = 0; i < n; i++ {
        total = total + @vararg_get(nums, i);
    }
    return total;
}
```

No `va_list` and no `@va_start`/`@va_end` — the compiler tracks the
argument count and stores them contiguously. The typed form is the
right choice for new ZLang code that does not need C-ABI compatibility.

### 37.3 Side-by-side

| | `vararg<_>` | `vararg<T>` |
|---|---|---|
| Compatibility | C ABI | ZLang only |
| Type safety | runtime | compile-time |
| Access | `va_list` + `@va_arg` | `@vararg_len` / `@vararg_get` |
| Setup | `@va_start` / `@va_end` | none |

### 37.4 Limits

- `vararg` is only valid in the parameter list of a `fun` or `wrap`
  declaration; it is not a first-class type and cannot be used as
  the type of a local variable, a field, or a `fun` return.
- Variadic parameters must be the last parameter of the function.
- Forwarding variadic arguments from one variadic function to another
  is not supported in `0.1.0`. To forward, accept `vararg<_>`, build
  the format string at the call site, and call the target directly.

---

## 38. Header Wrapper Generation

ZLang ships with a header-to-bindings tool:

```bash
zlang wrap /usr/include/GL/gl.h -o gl_bindings.zl
```

It reads a C header, ignores preprocessor conditionals it cannot
evaluate, and emits a `.zl` file with one `wrap` declaration per
function. Structs become ZLang structs with field types translated from
C. The tool targets the SysV ABI for layout.

Generated files are meant to be checked in and tweaked by hand; the
tool is a starting point, not a complete C parser.

---

## 39. SIMD Vectors

`simd<T, N>` is a fixed-width vector type. The compiler lowers it to the
matching LLVM vector, which in turn maps to SSE / AVX / NEON
instructions when the target supports them.

```zl
simd<f32, 4> a = {1.0, 2.0, 3.0, 4.0};
simd<f32, 4> b = {5.0, 6.0, 7.0, 8.0};
simd<f32, 4> c = a + b;        ?? {6, 8, 10, 12}
simd<f32, 4> d = b - a;        ?? {4, 4, 4, 4}
```

Element access uses `[i]`; the type of `a[i]` is `T`. Supported
sizes: 2, 4, 8, 16, 32. Supported element types: `i8` `i16` `i32`
`i64` `u8` `u16` `u32` `u64` `f16` `f32` `f64`.

A small kernel is in
[`examples/tests/simd_test.zl`](examples/tests/simd_test.zl).

---

## 40. Expression Blocks

```zl
i32 result = <i32> {
    i32 a = 10;
    i32 b = 32;
    a + b
};
```

`<type> { ... expr }` evaluates the body for side effects, then yields
the final expression as the block's value. Useful in assignments, return
expressions, and function arguments where C would need a comma
expression or a separate function.

A test is in
[`examples/tests/expression_block_test.zl`](examples/tests/expression_block_test.zl).

---

## 41. Numeric Literals

```zl
i32   million = 1'000'000;            ?? decimal with separators
f32   pi      = 3.141'592'653;
i32   color   = 0xFF00AA;             ?? hex
u32   mask    = 0xDEAD'BEEF;          ?? hex with separators
i32   flags   = 0b1010'1100;          ?? binary
i32   perms   = 0o755;                ?? octal
i64   big     = 999'999'999'999;
```

The `'` separator may appear anywhere between digits and is purely
visual. There are no implicit suffixes: a literal with no decimal
point is an integer (its width is determined by the target type or
by the value), and a literal with a decimal point is a `f64` unless
explicitly cast.

---

## 42. Compile-Time Constants

The `#define` directive defines a compile-time substitution. Used
sparingly, it is the right tool for platform conditions and feature
flags:

```zl
#define PLATFORM_LINUX 1
#define MAX_PATH 4096
```

The current release does not evaluate `#define`s in expression
context; they are textual substitutions applied at the lexer level.
For real compile-time computation, use function templates
([§10.6](#106-function-templates)).

### Overriding `#define` from the command line

Any `#define` value can be overridden on the command line with `-D`:

```bash
zlang main.zl -D MAX_PATH=8192
zlang main.zl -DLOGMODE=true -o myapp
```

Both `-D NAME=VALUE` (with a space) and `-DNAME=VALUE` (fused) are
accepted. The value side is taken literally, including spaces, until the
end of the argument — quote the value in the shell if you need
whitespace. Multiple `-D` flags for the same name are legal; the last
one wins.

This is a textual override, applied at the lexer level: the source
file's `#define MAX_PATH 4096` is replaced by `MAX_PATH 8192` *before*
the rest of the file is tokenized. It does not require the file to be
edited and does not affect `#define`s in any other file's context.

If a `-D NAME=...` flag is passed but no `#define NAME` is found in
any of the parsed `.zl` files, the compiler emits a warning so the
typo does not go silent:

```
Warning: -DLOGMODE=... was provided but #define LOGMODE was not found in parsed .zl files
```

This makes `-D` the right tool for build-time configuration: keep
sensible defaults in source, and let the build system override them
without touching the file.

---

## 43. Extension Concepts

ZLang `0.1.0` ships with the **`zlx` extension system**: language and
runtime features beyond the core are packaged as installable
extensions, each in its own repository and built as a dynamically
loadable library.

The full design and roadmap is in
[`EXTENSIONS_ROADMAP.md`](EXTENSIONS_ROADMAP.md). This section is a
quick reference for the parts of the model that the rest of this
document depends on.

An extension declares:

- A name, version, and a range of supported plugin ABI versions.
- The set of core keywords it registers (reserving them at compile
  time so they can be used in ZLang source).
- Optional syntax blocks (e.g. `brainfuck { ... }`) that the parser
  dispatches to.
- Native libraries to link.
- CLI flags it adds to `zlang`.
- A list of modules it provides.

Core language keywords are reserved by ABI version, not by extension.
The compiler checks that no two installed extensions reserve the same
keyword for the same ABI version.

---

## 44. The `module` CLI Subcommand

The `zlang` binary includes a subcommand for managing installed
extensions:

```bash
zlang module list                ?? list installed extensions
zlang module info <name>         ?? show manifest and ABI info
zlang module install <path>      ?? install a built .so/.dylib/.dll
zlang module uninstall <name>    ?? remove an installed extension
zlang module enable <name>       ?? activate a disabled extension
zlang module disable <name>      ?? deactivate without uninstalling
zlang module load-order          ?? print the topological load order
zlang module dev <dir>           ?? watch <dir> and auto-reload on change (Linux only)
```

`module install` verifies the plugin's SHA-256 manifest hash before
loading it; tampered packages are refused. `--no-extensions` and
`--isolated` flags on the main command temporarily disable all
extensions; see [§47](#47-cli-reference).

`module load-order` walks the dependency graph of installed
extensions and prints the order in which they would be initialized —
useful when debugging "extension X needs extension Y" failures.

`module dev <dir>` watches the given directory with `inotify` and
reloads the extension on every change. The watcher is Linux-only and
honors `Ctrl+C` for clean shutdown. Use it during extension
development to skip `install` + restart cycles.

---

## 45. The Plugin ABI (v6)

The ABI is bundled into the compiler. Run `zlang module sdk` to
materialize the C header `zlang_plugin_api_v1.h`, the convenience header
`zlx_plugin_sdk.h`, and the Zig SDK (`sdk.zig`) into `~/.zlang/sdk`.
The current version is **6**.

Key points:

- Plugins are shared libraries exporting a single `zlang_register` entry
  point.
- The host calls `zlang_register` with a `ZlangPluginApi*` table; the
  plugin fills in its metadata and registers keywords, syntax blocks,
  CLI flags, and modules.
- The plugin advertises `api_min` and `api_max`; the host refuses to
  load a plugin outside the supported range.
- A plugin may declare a list of `requires_host_features` in its
  probe result. If the host lacks a required feature, the plugin is
  declined at probe time with a structured reason.
- Core language keywords (the 23 listed in [§5.3](#53-keywords)) are
  reserved by the host. A plugin that calls `register_syntax_block`
  with a core-keyword name receives the `ZLANG_REGISTER_RESERVED = 4`
  return code (new in v6). To override a core keyword intentionally,
  use `register_keyword_block` instead.
- Modules are first-class: a plugin can `provide("std.thread")` and the
  host will route `use std.thread` to it.
- The host computes a SHA-256 over the plugin file at install time and
  stores the hash in the manifest. Subsequent loads verify the hash.

Plugins cannot reach into the compiler's private structs; everything
goes through the API table.

---

## 46. Building a Custom Extension

The minimum viable plugin is roughly:

```c
#include "zlang_plugin_api_v1.h"

ZLANG_EXPORT void zlang_register(ZlangPluginApi* api) {
    ZlangPluginInfo* info = api->alloc_info();
    info->name = "myext";
    info->version = "0.1.0";
    info->api_min = 6;
    info->api_max = 6;
    info->provides = "myext.runtime";
    api->register_keyword_block(info, "mykw", parse_my_keyword);
    api->provide(info, "myext.runtime");
    api->publish(info);
}
```

Compile as a shared library against the bundled SDK, with no fixed
paths into the compiler tree:

```bash
clang -shared -fPIC -O2 -o myext.zlx.so myext.c -I"$(zlang module sdk --include)"
zlang module install ./myext.zlx.so
```

For a Zig plugin that does `const sdk = @import("zlx");`, let the
compiler wire the SDK module and pack the package in one step:

```bash
zlang module build .
```

See any of the bundled extensions for a worked example:

- [`../brainfuck/`](https://github.com/zlangdevs/brainfuck) — adds the
  `brainfuck { ... }` block and standalone Brainfuck mode.
- [`../threading/`](https://github.com/zlangdevs/threading) — adds
  `std.thread` and links `pthread` on Linux.
- [`../zlisp/`](https://github.com/zlangdevs/zlisp) — adds embedded
  Lisp-style blocks.
- [`../zlb/`](https://github.com/zlangdevs/zlb) — registers a file
  extension handler for `.zlb` files.

Each lives in its own repository and is built independently of the
core compiler.

---

## 47. CLI Reference

### 47.0 Top-level commands

The `zlang` binary is a single executable with several modes of
operation selected by the first positional argument:

| Command | Meaning |
|---|---|
| `zlang <input.zl> [flags...]` | default — compile the source file(s) to an executable named `output` (or `-o <name>`) |
| `zlang run <input.zl> [flags...]` | compile, execute, and remove the temporary binary; the exit code is propagated to the shell |
| `zlang zli` | start the interactive REPL — see [§48](#48-the-repl-zlang-zli) |
| `zlang wrap <header.h> -o <bindings.zl>` | generate `wrap` declarations from a C header |
| `zlang wrap-clang <header.h> -o <bindings.zl>` | same as `wrap`, but uses `clang -ast-dump` for more accurate type recovery |
| `zlang module <verb> ...` | manage installed extensions — see [§44](#44-the-module-cli-subcommand) |
| `zlang help` | print the full help text |
| `zlang version` | print the compiler version plus the major versions of `clang` / `llc` / `opt` / `lli` / `lld` detected on `PATH` |

### 47.1 Basic compilation

```bash
zlang <input.zl> [-o <name>] [options...]
```

The default output name is `output`. To compile to an object file
without linking, use `-c`.

### 47.2 Output control

| Flag | Meaning |
|---|---|
| `-o <name>` | output executable name |
| `-keepll` | keep the generated LLVM IR file (`output.ll`) |
| `-c` | compile to `.o`, skip linking |

### 47.3 Optimization

| Flag | Meaning |
|---|---|
| `-optimize` | enable the LLVM optimization pipeline |
| (default) | unoptimized for fast iteration |

`-optimize` runs the standard LLVM `-O2` pass set on the generated
bitcode. Without it, every emitted function carries `noinline` and
`optnone` attributes and no passes are run. There is no public knob
for finer-grained optimization levels in this release.

### 47.4 Inspection

| Flag | Meaning |
|---|---|
| `-dast` | dump the AST after parsing and stop |
| `-verbose` | show AST plus per-stage messages |
| `-q`, `-quiet` | suppress non-error output |
| `-stats` | print per-stage timing and unit count at the end of the build |
| `-verify-ir` | call `LLVMVerifyModule` after each IR emission; useful when chasing a codegen bug |

### 47.5 Linking

| Flag | Meaning |
|---|---|
| `-l<name>` | link `-l<name>` |
| `-L<path>` | add library search path |
| `-Wl,<opts>` | pass through to the linker |
| `-link <file.o>` | link an additional object file |

### 47.6 Target architecture

| Flag | Meaning |
|---|---|
| `-arch <triple>` | target triple (default: host) |
| `-arch x86_64` | shorthand for the matching triple |
| `-arch x86_64-linux-gnu.2.17` | explicit full triple |
| `-j N` | limit parallel backend compilation to `N` workers (default: host core count); use `-j 1` to debug crashes that disappear under parallelism |

`-arch` uses `zig cc` for sysroot-aware linking. Ensure `zig` is on
`PATH`.

### 47.7 Preprocessor overrides

| Flag | Meaning |
|---|---|
| `-D NAME=VALUE` | override the value of a `#define NAME` in any parsed `.zl` file; the space is optional (`-DNAME=VALUE` is the same flag) |
| `-D NAME=` | override with an empty value |

If `-D NAME=...` is passed but no `#define NAME` appears in any of the
parsed files, the compiler emits a warning. See [§42](#42-compile-time-constants)
for the full description of how this interacts with the `#define`
directive.

### 47.8 Extensions

| Flag | Meaning |
|---|---|
| `--no-extensions` | compile with extensions disabled |
| `--isolated` | as above, plus no third-party stdlib includes |
| `--extension-dir <path>` | override the extension search path |

### 47.9 LLVM toolchain selection

The compiler probes for `clang-21`/`clang21` first, then `clang-20`,
`clang-19`, …, and finally the unversioned `clang`. Absolute paths
under common prefixes are also tried. Override with:

```bash
ZLANG_LLVM_BIN=/opt/llvm-21/bin zlang main.zl -optimize
```

At build time, set the LLVM library SONAME:

```bash
zig build -Dllvm-lib=LLVM-20
```

### 47.10 Header wrapper generation

```bash
zlang wrap <header.h> -o <bindings.zl>
zlang wrap-clang <header.h> -o <bindings.zl>
```

The two forms produce equivalent output; `wrap-clang` shells out to
`clang -ast-dump` for more accurate struct and macro recovery on
headers with heavy preprocessor use. Both are described in
[§38](#38-header-wrapper-generation).

### 47.11 Module management

```bash
zlang module list|info|install|uninstall|enable|disable
```

See [§44](#44-the-module-cli-subcommand).

---

## 48. The REPL (`zlang zli`)

`zlang` ships with an interactive Read-Eval-Print Loop modeled on
`ghci`:

```bash
$ zlang zli
zli> :load examples/factorial.zl
zli: loaded examples/factorial.zl (module factorial)
zli> factorial(5)
120 : i32
zli> println("done")
done
zli> :quit
```

### 48.1 Commands

| Command | Purpose |
|---|---|
| `:load <file.zl>` | parse and link a source file into the session |
| `:import <module>` | register a `use` statement for the current session |
| `:files` | list the files currently loaded |
| `:clear` | drop everything from the session |
| `:help` | print the REPL command summary |
| `:quit`, `:q`, `:exit` | leave the REPL |

### 48.2 Statements and expressions

Each line is one of:

- A *command* (above) — handled internally, no code is generated.
- A *statement* — compiled and run; no value is printed.
- An *expression* — compiled, run, and the resulting value is printed
  with its type. A bare identifier prints the value of that variable.

The REPL wraps every input in a fresh `main()` and runs it; there is
no incremental compilation in `0.1.0`. This is fast enough for
exploration but not for tight numerical work — use a real program for
that.

### 48.3 Limits

- No multi-line input. A function body, struct, or any block that
  spans lines must be `:load`-ed from a file.
- No step-back. `:clear` is the only way to undo state.
- The REPL shares the global extension set; if an extension changes
  while the REPL is running, restart the REPL.

---

## 49. Build Pipeline

A single `zlang` invocation runs four stages:

1. **Lexical analysis** — Flex. Produces tokens; handles
   `??`/`...` comments, string and character escapes, numeric literal
   forms, and the contextual `>>` (return-type arrow vs. right shift
   vs. generic).
2. **Parsing** — Bison GLR. Produces the AST. Three shift/reduce
   conflicts remain; they are intentional and correspond to the
   expression / statement ambiguity in ZLang.
3. **Semantic analysis** — Zig. Resolves names, infers types, checks
   error-flow handlers, emits warnings for unhandled `send`s and dead
   `on` blocks. Runs extension preprocessors here.
4. **Code generation and linking** — LLVM (via the `llvm-c` API),
   `llc`, and `clang`/`lld`. Produces the final executable or `.o`.

The `-dast` flag stops after stage 2; `-keepll` keeps the file
produced at the end of stage 3.

---

## 50. Type System Notes

- **Sign inference is local.** A literal in a context where the type
  is known takes that type. A bare literal defaults to `i32` or `f64`
  by its form. Casts through `as` are explicit.
- **Implicit conversions are limited.** No implicit numeric narrowing;
  no implicit `int ↔ float`; no implicit `pointer ↔ integer`. C-style
  promotions happen only inside variadic calls ([§37](#37-variadic-functions)).
- **Generic functions are monomorphized** at the call site. There is
  no runtime type information.
- **No implicit `null` for non-pointer types.** Only `ptr<T>` can
  receive `null`.

---

## 51. LLVM IR Mapping

| ZLang | LLVM |
|---|---|
| `i8`/`i16`/`i32`/`i64` | `i8`/`i16`/`i32`/`i64` |
| `u8`/`u16`/`u32`/`u64` | `i8`/`i16`/`i32`/`i64` (with `nuw`/`nsw` hints) |
| `f16`/`f32`/`f64` | `half`/`float`/`double` |
| `bool` | `i1` |
| `void` | `void` |
| `ptr<T>` | `ptr` (opaque) |
| `arr<T, N>` | `[N x T]` |
| `simd<T, N>` | `<N x T>` |
| `struct S { ... }` | `%S = type { ... }` (matching SysV layout) |
| `union U { ... }` | `%U = type { ... }` (size of largest member) |

A function like

```zl
fun add(a: i32, b: i32) >> i32 {
    return a + b;
}
```

lowers to

```llvm
define i32 @add(i32 %0, i32 %1) {
entry:
  %ai32 = alloca i32, align 4
  store i32 %0, ptr %ai32, align 4
  %bi32 = alloca i32, align 4
  store i32 %1, ptr %bi32, align 4
  %load = load i32, ptr %ai32, align 4
  %load1 = load i32, ptr %bi32, align 4
  %add = add i32 %load, %load1
  ret i32 %add
}
```

Use `-keepll` to inspect the IR for a specific program.

---

## 52. Optimization Passes

When `-optimize` is passed, the compiler runs the LLVM O2 pipeline:

- dead code elimination
- constant folding and propagation
- function inlining
- loop unrolling (small loops)
- auto-vectorization of `simd<T, N>` arithmetic
- instruction combining
- GVN

The compiler does not run its own optimization passes; everything
optimization-related is delegated to LLVM. This keeps the compiler
small and lets LLVM's well-tested passes do the work.

---

## 53. Diagnostics Reference

### 53.1 Lexer / parser

```
Parse error at file.zl:10:5: error.ParseFailed
```

Usually a syntax error. Check for:

- missing `;` after a statement
- missing `>>` in a function signature
- `??` vs `//` for comments
- unbalanced `{` `}`

### 53.2 Semantic

| Diagnostic | Meaning |
|---|---|
| `Type mismatch: expected T, found U` | assignment or argument type wrong |
| `Undefined name: x` | typo or missing import |
| `Invalid cast` | conversion not in the allowed list |
| `Cannot assign to const` | writing to a `const` binding |
| `Cannot write through const pointer` | writing through `ptr<const T>` |
| `break/continue outside loop` | misplaced control flow |

### 53.3 Error flow

| Diagnostic | Meaning |
|---|---|
| `unhandled error 'X' from call to f` | add an `on X { ... }` block |
| `handler 'on Y' cannot match any error from f` | dead handler — typo or stale code |

Both are warnings, not errors, so existing programs keep compiling. They
can be promoted to errors in a future release.

### 53.4 Linker

| Diagnostic | Meaning |
|---|---|
| `undefined reference to 'sym'` | C function not declared (missing `fun @sym`) or library not linked (missing `-l<name>`) |

---

## 54. Best Practices

### 54.1 Naming

- Functions and variables: `snake_case`.
- Types (`struct`, `enum`, `union`): `PascalCase`.
- Constants: `SCREAMING_SNAKE_CASE`.
- Error names: `PascalCase` ending in a noun (`FileNotFound`,
  `PermissionDenied`).
- Module names: `lower.dotted` (`std.io`, `app.net.http`).

### 54.2 Memory

- Always check `malloc` results before use.
- Pair every allocation with a free path. For C-style cleanup, use
  `goto` and labels ([§9.4](#94-goto-and-labels)); for new code, prefer
  `send`/`solicit` to keep cleanup logic localized.
- Prefer `arr<T, N>` over `malloc` for fixed-size buffers.
- Mark function parameters that should not be modified as
  `ptr<const T>`.

### 54.3 Performance

- Use `simd<T, N>` for data-parallel numeric kernels.
- Pass large structs by pointer (`ptr<BigStruct>`) to avoid copies.
- Mark read-only data `const` to enable pointer-displacement
  optimization.
- Keep stack frames small; very large stack arrays belong on the heap.

### 54.4 Error flow style

- `send` is for *"this call failed, take it from here"*.
- `solicit` is for *"I can't continue without you giving me X"*.
- Keep handler bodies short; if a handler is doing real work, move
  it into a named function and call that from the handler.
- Name errors after their *condition*, not their handler. `FileNotFound`
  is good; `HandleFileNotFound` is not.

---

## 55. Common Patterns

### 55.1 Tagged union

```zl
union Value {
    i i32,
    f f32,
    p ptr<u8>
}

struct TaggedValue {
    tag u8,        ?? 0 = int, 1 = float, 2 = pointer
    data Value
}

fun print_value(tv: TaggedValue) >> void {
    if tv.tag == 0 {
        std.printInt(tv.data.i);
    } else if tv.tag == 1 {
        std.printFloat(tv.data.f);
    } else {
        std.println("<ptr>");
    }
}
```

### 55.2 Option type

```zl
struct Option_i32 {
    has_value bool,
    value i32
}

fun some(v: i32) >> Option_i32 { return {true, v}; }
fun none() >> Option_i32 { return {false, 0}; }
```

A more general `Option<T>` is not first-class in `0.1.0`; per-type
Option structs are the current idiom.

### 55.3 Iterator

```zl
struct Iter {
    cur ptr<i32>,
    end ptr<i32>
}

fun next(it: ptr<Iter>) >> bool {
    if it.cur >= it.end { return false; }
    it.cur = it.cur + 1;
    return true;
}
```

### 55.4 Resource acquisition with `solicit` cleanup

```zl
error NeedResource = _;

fun use_resource() >> void {
    ptr<u8> r = null;
    solicit NeedResource;          ?? ask caller for r
    ?? ... use r ...
    if r == null { return; }
    ?? ... caller supplies r next time ...
}
```

`solicit` lets the callee *ask* for a resource rather than fail when
it isn't there.

### 55.5 Generic data (today)

Function templates ([§10.6](#106-function-templates)) are supported.
Generic *type declarations* are not yet first-class. For reusable
containers today, use concrete structs or code generation.

---

## 56. Troubleshooting

### 56.1 Build issues

- **"cannot find `llc`"** — install LLVM (14–21) and ensure its `bin/`
  is on `PATH`, or set `ZLANG_LLVM_BIN`.
- **"undefined reference to `sin`"** — link with `-lm`, or add
  `#flag -lm` to the file that calls `sin` / `cos` / etc.
- **"undefined reference to `pthread_*`"** — install the `threading`
  extension, or link with `-lpthread` and declare the functions.

### 56.2 Compile-time issues

- **`Parse error at line X`** — see [§53.1](#531-lexer--parser).
- **`Type mismatch: expected i32, found f32`** — add an explicit
  `as` cast.
- **`handler cannot match any error from f`** — the call site has a
  stale `on` block; either remove it or add the matching `send`/`solicit`
  in the callee.

### 56.3 Runtime issues

- **Segfault on pointer dereference** — the pointer is null or dangling.
  Check `malloc` returns and lifetimes.
- **Buffer overflow** — arrays are not bounds-checked in release builds;
  check indices manually.
- **Wrong output** — use `-keepll` and inspect the LLVM IR, or
  recompile with debug info:

  ```bash
  clang -g output.ll -o program
  gdb ./program
  ```

### 56.4 Extensions

- **`zlang module install` rejects the file** — the SHA-256 in the
  manifest does not match the file. Re-build and re-install.
- **`api version mismatch`** — the plugin's `api_min`/`api_max` does
  not include the current compiler version (6). Update the plugin or
  the compiler.
- **`keyword 'foo' already registered`** — two installed extensions
  reserve the same keyword for the same ABI version. Disable one.

---

## 57. Future Direction

The `0.1.0` line is a stable language and ABI baseline. Subsequent
releases will add (in approximate order):

- Generic type declarations with constraints.
- First-class `Option<T>`, `Result<T, E>`, and a richer error-flow
  type.
- Async / await built on top of `solicit` and a scheduler extension.
- A package manager that consumes the same manifest format that
  extensions already use.
- IDE integration (LSP) for the core language, with extension authors
  contributing their own server extensions.
- A formal language specification document.
- More optimization passes and an MLIR-based backend for retargeting.

The roadmap is intentionally modular. The core language changes
slowly; new functionality is expected to land as extensions, with the
core absorbing the most-used ones in subsequent major releases.

See [`EXTENSIONS_ROADMAP.md`](EXTENSIONS_ROADMAP.md) for the extension
plan and [`README.md`](README.md) for the project-level roadmap.

---

*ZLang 0.1.0 — built with intent.*
