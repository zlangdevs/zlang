# ZLang examples

Every `.zl` file in this directory is a self-contained program or library that
the compiler can build with `zlang <file>` (or the convenience wrapper
`zlang run <file>`). Programs use `use std.*` for standard library access and
`@name(...)` for direct libc / C symbol calls. Most examples print to stdout;
a few read files or take CLI arguments.

This README is a tour. If you want the language reference, see
[../DOCUMENTATION.md](../DOCUMENTATION.md).

## Quick start

The smallest program in the tree is four lines long:

```zl
fun main() >> i32 {
    @printf("Hello World!\n");
    return 0;
}
```

Build and run it from the repo root:

```sh
zlang examples/hello_world.zl
./hello_world
```

Every example below works the same way; the only thing that changes is what
the program does. To run the test suite, see [the tests section](#tests).

## Tour

The examples are grouped by what they teach. Work through them in order the
first time; later, jump straight to the topic you need.

### 1. Getting started

- [`hello_world.zl`](hello_world.zl) — `main` returns `i32`, call libc
  `printf` via `@printf`.
- [`factorial.zl`](factorial.zl) — recursion and the `fun ... >> T` return
  arrow.
- [`fizzbuzz.zl`](fizzbuzz.zl) — `for`, `if` / `else if` / `else`, the `??`
  line comment, modulo and boolean operators.
- [`arguments.zl`](arguments.zl) — `main(argc, argv)`: reading CLI arguments
  through `ptr<ptr<u8>>`.
- [`square.zl`](square.zl) — nested `for { ... if ... { break; } }` loops
  using the empty-condition form.
- [`dynamic_array.zl`](dynamic_array.zl) — manual `malloc` / `realloc` /
  `free` of an `i32` buffer (compare to [`array_sort.zl`](#3-data-and-arrays)).

Language references: §1 getting started, §3 expressions, §9 control flow,
§10 functions in [DOCUMENTATION.md](../DOCUMENTATION.md).

### 2. The standard library

Each file under `examples/` named `*_demo.zl` shows the public surface of one
stdlib module end to end. They are the best way to skim the stdlib before
reading the per-module reference in §23-§33.

- [`assert_demo.zl`](assert_demo.zl) — `use std.assert` (`assert`, `assert_eq`,
  `assert_ne`, `assert_true`, `assert_false`).
- [`math_demo.zl`](math_demo.zl) — `use std.math` (`sqrt`, `pow`, plus the
  free-form helpers `square`, `cube`, and integer clamp helpers).
- [`math_with_assert_demo.zl`](math_with_assert_demo.zl) — combining
  `std.math` with `std.assert` for self-checking numeric code.
- [`random_demo.zl`](random_demo.zl) — `use std.random` (`seed_rng`, `rand_range`,
  `rand_float`, `rand_bool`, the `xor_shift` struct from `xor_shift_init`).
- [`create_file.zl`](create_file.zl) — `use std.fs` (write a file, read it
  back, exercise `pathExists`).
- [`struct_init_syntax.zl`](struct_init_syntax.zl) — designated initializers
  for structs and how they compose with `arr<T, N>` literals.

Module reference: §23 assert, §27 math, §29 random, §25 fs in
[DOCUMENTATION.md](../DOCUMENTATION.md).

### 3. Data and arrays

- [`array_sort.zl`](array_sort.zl) — bubble sort on a fixed-size
  `arr<i32, 8>` literal; reads and writes array elements by index.
- [`slice_test.zl`](slice_test.zl) — pointer/array indexing, the implicit
  decay of arrays to `ptr<T>`, and bounds-driven loops.
- [`const_pointer.zl`](const_pointer.zl) /
  [`const_pointer_syntax.zl`](const_pointer_syntax.zl) — `ptr<T>` vs
  read-only pointer conventions; the C-string `const u8*` case in
  particular.
- [`fizzbuzz.zl`](fizzbuzz.zl) — bool locals as guards in nested conditionals
  (also serves as a control-flow reminder).

Reference: §6 types, §11 arrays, §13 pointers in
[DOCUMENTATION.md](../DOCUMENTATION.md).

### 4. Talking to C

ZLang has no runtime of its own; every "I/O" you see is a direct libc call
declared with `wrap` or invoked through `@`. These examples show the patterns.

- [`arguments.zl`](arguments.zl) — `printf` with `%d` / `%s` format specifiers.
- [`hello_world.zl`](hello_world.zl) — the bare minimum to link against
  libc and run.
- [`donut.zl`](donut.zl) — `malloc` / `free` / `usleep` / `sin` / `cos`,
  double buffers, the standard "render a 3D donut" demo. Around 200 lines.
- [`stbds/`](stbds/) — integrate the `stb_ds.h` single-header C library into
  a ZLang program. Comes with a `build.sh` and a `wrapper.zl` that declares
  the dynamic-array API in ZLang syntax. See
  [the C interop reference](../DOCUMENTATION.md#37-c-interoperability).
- [`multi_file_c/`](multi_file_c/) — call a hand-written C function
  (`cfunc.c` / `cfunc`) from a multi-file ZLang project, with `wrapper.zl`
  bridging the C declaration. The example has its own
  [README](multi_file_c/README.md); build with `bash build.sh` from inside
  the directory.

Reference: §37 C interoperability in [DOCUMENTATION.md](../DOCUMENTATION.md).

### 5. The module system

- [`multi_file/`](multi_file/) — two `.zl` files compiled together. The
  `main.zl` file declares `module app;` and does `use utils;`; the
  helper functions live in `utils.zl`. Build from the directory with
  `zlang main.zl utils.zl` (the manifest is optional for two-file projects
  but is required once you have three or more).
- [`multi_file_c/`](multi_file_c/) — module + C interop combined. See
  [the previous section](#4-talking-to-c) and its in-directory README.

Reference: §38 modules and packages in [DOCUMENTATION.md](../DOCUMENTATION.md).

### 6. Real programs

- [`donut.zl`](donut.zl) — animated 3D donut; libc only, ~200 lines.
- [`brainfuck.zl`](brainfuck.zl) — a hand-written Brainfuck interpreter
  reading source from stdin into a fixed-size code array, with a
  pre-computed bracket jump table. Around 100 lines.
- [`chip8/`](chip8/) — a complete CHIP-8 emulator: 35 opcodes, 4 KB memory,
  64x32 framebuffer, ROM loading from disk, 60 Hz timers. About 500 lines
  of ZLang. See [chip8/README.md](chip8/README.md) for the ROM-loading
  instructions and a feature checklist.

These are the files to read when you want to see "idiomatic" ZLang in
context, beyond the toy-sized snippets in this README.

### 7. Plugins and the extension API

- [`source_map_plugin/`](source_map_plugin/) — a C plugin that registers a
  new syntax block (`brokenmap { ... }`), injects generated ZLang source
  into the parse stream, and attaches a source map so errors point back at
  the user-written `source_map_error.zl` rather than the generated text.
  Includes a `run_test.sh` that demonstrates the source map in action by
  emitting a deliberate semantic error inside the generated code.

If you are writing your own plugin, this is the canonical example to copy
from. The plugin API contract is documented in
[§45](../DOCUMENTATION.md#45-plugin-abi) and in
[`include/zlang_plugin_api_v1.h`](../include/zlang_plugin_api_v1.h).

## Tests

`examples/tests/` is the integration test suite. There are 68 `*_test.zl`
files plus three sub-folders for the negative cases:

- [`tests/`](tests/) — pass tests. Run any one with `zlang tests/<name>.zl`
  and check the exit code; many print expected values that you can grep for
  (the convention used in `error_handling_test.zl` and friends is to include
  `(expected N)` in the format string so a regex like
  `grep -F '(expected 42)'` confirms a match).
- [`tests/compile_fail/`](tests/compile_fail/) — files that must fail to
  compile. Each one is paired with a `.expected` file containing the
  diagnostic text the compiler is supposed to print. Use these to verify
  that the diagnostic engine still produces useful messages.
- [`tests/warning/`](tests/warning/) — files that compile but should emit a
  warning. Each one is paired with a `.expected` file. Some also have a
  `.args` file that records the CLI flags to pass to the compiler.
- [`tests/stdlib/`](tests/stdlib/) — per-module smoke tests for every
  `std.*` module. Each file targets exactly one module.
- [`tests/wrap_tests/`](tests/wrap_tests/) — wraps of C functions and
  libc-style helpers used by other tests.

The repository's [`run_tests.sh`](../run_tests.sh) drives the whole
directory. Run it from the repo root:

```sh
bash run_tests.sh
```

A useful subset to run by hand while iterating on the compiler:

```sh
zlang examples/tests/error_handling_test.zl && ./error_handling_test
zlang examples/tests/match_test.zl              && ./match_test
zlang examples/tests/send_solicit_channel_separation_test.zl \
    && ./send_solicit_channel_separation_test
```

Test files worth reading as documentation in their own right:

- `error_handling_test.zl` — `error ... = ...;`, `send`, `on _`, handler
  chains. Mirrors the patterns from §13-§17 of
  [DOCUMENTATION.md](../DOCUMENTATION.md).
- `match_test.zl` — `match` statements including the comma-separated
  multi-arm form `1,2,3 { ... }` and the wildcard arm. Mirrors §9.5.
- `module_test.zl` and `module_alias_test.zl` — the `module` keyword,
  `use <alias>=<module>`, top-level module headers, and the rules around
  `use module` vs `use module as alias`. Mirrors §38.
- `handler_capture_test.zl` and
  `handler_capture_byref_bool_test.zl` — what `on ...` blocks can and
  cannot do with the enclosing scope.
- `preprocessor_define_test.zl` and `define_override_test.zl` — `#define`,
  the `NUM_FIELDS(...)` style macro, and the `-D NAME=VALUE` override flag
  (§42, §47.4).
- `defer_test.zl`, `defer_loop_test.zl`, and `defer_comprehensive_test.zl`
  — the `defer` statement: LIFO ordering, capture by reference,
  scope-boundary semantics, the `on`-handler join-point quirk, and
  defer accumulation across loop iterations. Mirrors §9.6.

## Layout reference

| Path                                         | What it is                                |
| -------------------------------------------- | ----------------------------------------- |
| `examples/*.zl`                              | 29 standalone demo programs               |
| `examples/chip8/`                            | Full CHIP-8 emulator, with its own README |
| `examples/multi_file/`                       | Two-file ZLang project                    |
| `examples/multi_file_c/`                     | Multi-file project + C source             |
| `examples/stbds/`                            | Integration of a single-header C library  |
| `examples/source_map_plugin/`                | C plugin implementing a syntax block      |
| `examples/tests/`                            | Integration test suite (positive)         |
| `examples/tests/compile_fail/`               | Diagnostic-positive tests                 |
| `examples/tests/warning/`                    | Warning-positive tests                    |
| `examples/tests/stdlib/`                     | One smoke test per `std.*` module         |
| `examples/tests/wrap_tests/`                 | C wraps used by other tests               |

If you add a new example, keep it small, self-contained, and
copy-paste-runnable. If it has more than one file or needs special CLI
flags, give it its own directory with a `build.sh` and a brief README,
the way `multi_file_c/` and `source_map_plugin/` do.
