# 🚀 ZLang

**A fast, clean systems programming language that compiles to LLVM IR**

ZLang combines the simplicity of C with modern features like SIMD vectors, optional types, and even embedded Brainfuck support. Write efficient, low-level code without the ceremony.

```zl
fun main() >> i32 {
    @printf("Empire starts here 😈\n");
    return 0;
}
```

📖 **[Full Documentation](DOCUMENTATION.md)** | 🔧 [Examples](examples/)

## ✨ Features

- **🎯 Simple & Clean Syntax** - Familiar C-like syntax with modern improvements
- **⚡ SIMD Built-in** - First-class SIMD vector support for high-performance computing
- **🔧 Direct C Interop** - Call C functions with `@` prefix, seamless libc integration
- **🏗️ Modern Type System** - Structs with default values, enums, generics, and more
- **🔒 Const Pointers** - Compile-time safety with read-only pointer guarantees
- **🔄 Smart Loops** - Flexible `for` loops that work with any expression
- **🛡️ Guard Clauses** - Function-level preconditions with `when (...)`
- **🧩 Expression Blocks** - `<type> { ... }` blocks that return values
- **🚨 Error Flow v2** - `error`, `send`, `solicit`, and call-site `on` handlers
- **🔢 Numeric Error Matching** - Handle by code with `on 1 { ... }` / `on solicit 1 { ... }`
- **🧠 Brainfuck Integration** - Embed Brainfuck code directly or compile pure .bf files!
- **🎨 Type Inference** - Auto-cast with `as _` for cleaner code 
- **📦 Zero Runtime** - Compiles directly to LLVM IR, no runtime overhead

## 🚦 Quick Start

### Installation

```bash
# Build the compiler
zig build

# Install compiler + stdlib to system paths
zig build install

# Remove system install
zig build uninstall

# Compile a ZLang program
./zig-out/bin/zlang examples/hello_world.zl

# Compile with optimizations and specify output name, keep LLVM IR
./zig-out/bin/zlang myprogram.zl -o myprogram -optimize -keepll

# Compile pure Brainfuck programs
./zig-out/bin/zlang -b mandelbrot.bf -o mandelbrot
./zig-out/bin/zlang -b32 program.bf -o output  # 32-bit cells

# Generate wrappers from C headers
./zig-out/bin/zlang wrap mylib.h -o mylib.zl
./zig-out/bin/zlang wrap-clang mylib.h -o mylib_abi.zl
```

### Hello World

```zl
fun main() >> i32 {
    @printf("Hello, World!\n");
    return 0;
}
```

### Factorial Function

```zl
fun factorial(n: i32) >> i32 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

fun main() >> i32 {
    @printf("5! = %d\n", factorial(5));
    return 0;
}
```

## 📖 Language Guide

### Variables & Types

```zl
i32 count = 42;                   ?? Signed integers: i8, i16, i32, i64
u64 big_num = 1000000;            ?? Unsigned: u8, u16, u32, u64
f32 temperature = 98.6;           ?? Floats: f16, f32, f64
bool is_active = true;            ?? Booleans
ptr<i32> my_ptr = &count;         ?? Mutable pointer to mutable data
ptr<const i32> ro_ptr = &count;   ?? Pointer to const data (value can't change)
const ptr<i32> fixed = &count;    ?? Const pointer (pointer can't be reassigned)
arr<i32, 100> numbers;            ?? Fixed-size arrays

const i32 MAX = 100;              ?? Constant variables
```

**Note:** Comments use `??` instead of `//`

### Control Flow

**If Statements**
```zl
if temperature > 100.0 {
    @printf("Too hot!\n");
} else if temperature < 0.0 {
    @printf("Freezing!\n");
} else {
    @printf("Just right\n");
}
```

**Loops - The ZLang Way**
```zl
?? Infinite loop
for {
    @printf("Forever\n");
    break;
}

?? Simple variable (no parentheses!)
i32 countdown = 5;
for countdown {
    @printf("%d...\n", countdown);
    countdown--;
}

?? Complex expressions (parentheses required)
for (i < 10 && i != 5) {
    i++;
}

?? C-style loops
for i32 i = 0; i < 10; i++ {
    @printf("i = %d\n", i);
}
```

### Guard Clauses

```zl
fun divide(a: i32, b: i32) when (b != 0) >> i32 {
    return a / b;
}
```

### Expression Blocks

```zl
i32 score = <i32> {
    i32 base = 40;
    i32 bonus = 2;
    base + bonus
};
```

### Error Flow (`send`, `solicit`, `on`)

```zl
error FileNotFound = 1;
error PermissionDenied = 1;
error FileLocked = _;

fun open_file(path: ptr<u8>, ro: bool) >> i32 {
    if path == "missing" {
        send FileNotFound;
        return -1;
    }
    if path == "protected" && !ro {
        solicit PermissionDenied;
    }
    if path == "locked" {
        send FileLocked;
        return 0;
    }
    return 1;
}

fun main() >> i32 {
    i32 res = open_file("protected", false)
        on 1 { @printf("shared code=1 handler\n"); }
        on solicit PermissionDenied { ro = true; }
        on _ {}
        on solicit _ {};

    @printf("Result: %d\n", res);
    return 0;
}
```

Handler forms:
- `on ErrorName { ... }`
- `on 1 { ... }`
- `on _ { ... }`
- `on solicit ErrorName { ... }`
- `on solicit 1 { ... }`
- `on solicit _ { ... }`

### Structs

```zl
struct Point {
    x i32,
    y i32,
}

struct Person {
    name arr<u8, 50> = "Anonymous",
    age i32 = 0,
    active bool = true
}

fun main() >> i32 {
    Point p = {10, 20};
    Person john = {.name = "John", .age = 30};
    
    @printf("Point: (%d, %d)\n", p.x, p.y);
    @printf("Person: %s, age %d\n", john.name, john.age);
    
    return 0;
}
```

### Enums

```zl
enum Status {
    IDLE,
    RUNNING = 100,
    COMPLETED,
    FAILED = 200
}

fun main() >> i32 {
    i32 current = Status.RUNNING;
    
    ?? Also supports C-style direct access
    if current == RUNNING {
        @printf("Program is running\n");
    }
    
    return 0;
}
```

### SIMD Vectors

```zl
fun main() >> i32 {
    ?? Create SIMD vectors
    simd<f32, 4> v1 = {1.0, 2.0, 3.0, 4.0};
    simd<f32, 4> v2 = {5.0, 6.0, 7.0, 8.0};
    
    ?? Vectorized operations
    simd<f32, 4> sum = v1 + v2;
    simd<f32, 4> product = v1 * v2;
    
    ?? Access elements
    @printf("sum[0] = %.1f\n", sum[0]);
    
    ?? Modify elements
    v1[2] = 100.0;
    
    return 0;
}
```

### Type Casting

```zl
i32 x = 42;
f32 y = x as f32;           ?? Explicit cast
f64 z = x as _;             ?? Auto-infer target type

?? Pointer casting
ptr<void> raw = &x as ptr<void>;
ptr<i32> typed = raw as ptr<i32>;
```

### Pointers & References

```zl
i32 value = 100;
ptr<i32> p = &value;        ?? Take address
i32 deref = *p;             ?? Dereference
*p = 200;                   ?? Modify through pointer

?? Pointer to const - cannot modify value, can reassign pointer
ptr<const i32> readonly = &value;
i32 read = *readonly;       ?? ✅ Reading is OK
*readonly = 50;             ?? ❌ Compile error: Cannot modify through pointer to const
readonly = &other_value;    ?? ✅ Can reassign pointer

?? Const pointer - cannot reassign pointer, can modify value
const ptr<i32> fixed = &value;
*fixed = 50;                ?? ✅ Can modify value
fixed = &other_value;       ?? ❌ Compile error: Cannot reassign const variable

?? Pointer arithmetic
p = p + 1;

?? Pass by reference
fun increment(val: ptr<i32>) >> void {
    *val = *val + 1;
}

?? Const parameters prevent modification
fun read_only(val: ptr<const i32>) >> i32 {
    return *val;  ?? OK to read, cannot modify
}
```

### C Interop

Call any C function with `@` prefix:

```zl
?? External C function declarations
fun @printf(format: ptr<u8>) >> i32;
fun @malloc(size: u64) >> ptr<void>;
fun @free(ptr: ptr<void>) >> void;

fun main() >> i32 {
    ptr<i32> buffer = @malloc(100) as ptr<i32>;
    *buffer = 42;
    @printf("Value: %d\n", *buffer);
    @free(buffer as ptr<void>);
    return 0;
}
```

Use `wrap` for automatic C ABI compatibility:

```zl
wrap @some_c_function(x: i32, y: f32) >> i32;
```

## 📦 Imports and Standard Library

ZLang supports importing other modules using the `use` directive.

- **Modules are declared in source files**: `module net.http;`
- **Imports are by module name**: `use net.http`
- **Prefix imports include submodules**: `use net` imports `net` and `net.*`
- **Standard library modules**: `use std.<module>` loads modules from the standard library.

Example:

```zl
module app.main;
use net.http;

fun main() >> i32 {
    return 0;
}
```

```zl
module net.http;

fun get(url: ptr<u8>) >> i32 {
    return 200;
}
```

### Standard Library Resolution

When you import `std.<module>`, ZLang searches for `<module>.zl` in:

- The directory specified by the `ZSTDPATH` environment variable.
- If `ZSTDPATH` is not set, the `stdlib/` directory located next to the compiler binary.

Example layout:

```
zlang              # compiler binary
stdlib/            # default stdlib directory if ZSTDPATH is not set
  random.zl
```

You can explicitly set `ZSTDPATH`:

```bash
export ZSTDPATH=/path/to/zlang/stdlib
```

If a standard module cannot be found, the compiler prints a clear error indicating it searched `ZSTDPATH` or the default `stdlib/` directory.

### Using std.random

The `std.random` module wraps libc random functions and provides helpers.

```zl
use std.random

fun main() >> i32 {
    srand(42);
    i32 r = rand();
    @printf("rand(): %d\n", r);

    i32 dice = randRange(1, 6);
    @printf("dice: %d\n", dice);
    return 0;
}
```

Provided symbols:

- **srand(seed: u32) >> void**
- **rand() >> i32**
- **randRange(min: i32, max: i32) >> i32**
- **time(t: ptr<i64>) >> i64**

### Using std.math

The `std.math` module wraps common mathematical functions from libm.

```zl
use std.math

fun main() >> i32 {
    f64 x = 16.0;
    f64 root = sqrt(x);
    @printf("sqrt(%.1f) = %.2f\n", x, root);
    
    f64 angle = toRadians(90.0);
    f64 sine = sin(angle);
    @printf("sin(90°) = %.6f\n", sine);
    
    f64 dist = distance(0.0, 0.0, 3.0, 4.0);
    @printf("distance = %.1f\n", dist);
    
    return 0;
}
```

Provided symbols:

- **sqrt(x: f64) >> f64** - Square root
- **pow(base: f64, exp: f64) >> f64** - Power function
- **sin(x: f64) >> f64**, **cos(x: f64) >> f64** - Trigonometric functions
- **fabs(x: f64) >> f64** - Absolute value for floats
- **square(x: f64) >> f64**, **cube(x: f64) >> f64** - Helper functions
- **distance(x1, y1, x2, y2) >> f64** - Euclidean distance
- **toRadians(degrees: f64) >> f64** - Convert degrees to radians

**Note:** Math functions require linking with `-lm`. The test runner handles this automatically.

### Using std.assert

The `std.assert` module provides assertion functions for testing and debugging.

```zl
use std.assert

fun main() >> i32 {
    testStart();
    
    ?? Test your functions
    i32 result = add(5, 3);
    assertEqual(result, 8, "add(5, 3) should equal 8");
    testPass("addition test");
    
    ?? Test ranges and conditions
    assertInRange(result, 0, 100, "result should be valid");
    assertTrue(result > 0, "result should be positive");
    
    ?? Get test summary
    return testSummary();
}
```

Provided symbols:

- **assert(condition, message)** - Basic assertion
- **assertEqual(actual, expected, message)** - Integer equality
- **assertEqualFloat(actual, expected, epsilon, message)** - Float equality with tolerance
- **assertTrue(condition, message)**, **assertFalse(condition, message)** - Boolean assertions
- **assertNull(ptr, message)**, **assertNotNull(ptr, message)** - Pointer assertions
- **assertGreater(actual, threshold, message)**, **assertLess(actual, threshold, message)** - Comparisons
- **assertInRange(actual, min, max, message)** - Range validation
- **testStart()**, **testPass(name)**, **testFail(name, message)**, **testSummary()** - Test framework

Notes:
- Tests run via `./run_tests.sh` automatically set `ZSTDPATH` to the project `stdlib/`.
- Additional std modules can be added by dropping `<name>.zl` into `stdlib/` and importing with `use std.<name>`.

## 🎪 Unique Features

### Embedded Brainfuck

Yes, really. ZLang has native Brainfuck support:

```zl
fun main() >> i32 {
    u16 x = 5;
    u16 y = 3;
    
    brainfuck {
        ?cell_size 16?
        ?load x 0?
        ?load y 1?
        [->+<]        ?? Add x to y
    };
    
    @printf("Result: %d\n", y);  ?? Prints: 8
    return 0;
}
```

The Brainfuck compiler integrates directly into the language with special directives for loading/storing variables!

### Standalone Brainfuck Compilation

ZLang can also compile pure Brainfuck programs directly:

```bash
# Compile with default 8-bit cells
zlang -b program.bf -o program

# Specify cell size (8, 16, 32, or 64 bits)
zlang -b8 program.bf -o program    # 8-bit cells (default)
zlang -b16 program.bf -o program   # 16-bit cells
zlang -b32 program.bf -o program   # 32-bit cells
zlang -b64 program.bf -o program   # 64-bit cells
```

The `-b` flags enable brainfuck-only mode, compiling `.b` or `.bf` files directly to native executables via LLVM. This gives you highly optimized Brainfuck programs with configurable cell sizes! The flag is also compatible with `-optimize` flag, applying extra optimizations. This can take some time, since compiler interprets parts of the code to precompute and optimize program. Applying `-optimize` in brainfuck-only mode will make compiler try to optimize code runnig parts of it during comptime (compile-time PGO), so compilation may take more time.

### Numeric Literal Delimiters

Make large numbers readable:

```zl
i32 million = 1'000'000;
i64 big = 999'999'999'999;
f32 precise = 3.141'592'653;
```

### Multiple Number Bases

```zl
i32 hex = 0xFF;              ?? Hexadecimal
i32 binary = 0b1010;         ?? Binary
i32 octal = 0o755;           ?? Octal
i32 decimal = 1_000_000;     ?? Decimal with separators
```

## 📚 Examples

Check out the `examples/` directory for more:

- **hello_world.zl** - Basic syntax
- **factorial.zl** - Recursion
- **math_demo.zl** - Mathematical functions with std.math
- **assert_demo.zl** - Testing and assertions with std.assert
- **brainfuck.zl** - Full Brainfuck interpreter in ZLang
- **const_pointer_demo.zl** - Const pointer safety demonstration
- **arguments.zl** - Arguments reading
- **tests/simd_test.zl** - Comprehensive SIMD examples
- **tests/struct_test.zl** - Struct features
- **tests/loop_test.zl** - All loop variations
- **tests/cast_test.zl** - Type casting examples
- **tests/pointer_test.zl** - Pointer operations
- **tests/deref_test.zl** - Pointer dereferencing

## 🏗️ Architecture

```
ZLang Source (.zl)
     ↓
Lexer (Flex)
     ↓
Parser (Bison GLR)
     ↓
AST (Zig)
     ↓
LLVM IR Generator
     ↓
LLVM IR (.ll)
     ↓
Clang/LLVM
     ↓
Native Binary
```

- **Frontend**: Flex + Bison GLR parser
- **Backend**: Custom Zig-based LLVM IR generator
- **ABI**: Full System V ABI compatibility for C interop (WIP)

## 🔧 Building from Source

Requirements:
- Zig (0.15.2)
- Flex
- Bison 3.x+
- LLVM/Clang

```bash
git clone <repo>
cd zlang
zig build -Doptimize=ReleaseFast
```

## 📝 Syntax Highlighting

Zed syntax extension is available:

- https://github.com/zlangdevs/zed-zlang

You can install it as a dev extension in Zed.

## 🤝 Contributing

Contributions welcome! ZLang is in active development.

Key areas:
- Standard library functions
- More optimization passes
- Better error messages
- IDE/editor plugins
- Bugs documentation

## 📜 License

ZLang is licensed under the **GNU General Public License v3.0**.

See [LICENSE](LICENSE) for full details.

## 🎯 Why ZLang?

- **For Learning**: Simple enough to understand, powerful enough to be useful
- **For Performance**: SIMD support and direct LLVM compilation
- **For Fun**: Brainfuck integration and clean syntax
- **For Real Work**: Full C interop means you can use any C library

---

**Empire starts here 😈**

Made with ⚡ by the ZLang team
