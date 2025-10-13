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
- **🔄 Smart Loops** - Flexible `for` loops that work with any expression
- **🧠 Brainfuck Integration** - Because why not? Embed Brainfuck code directly!
- **🎨 Type Inference** - Auto-cast with `as _` for cleaner code
- **📦 Zero Runtime** - Compiles directly to LLVM IR, no runtime overhead

## 🚦 Quick Start

### Installation

```bash
# Build the compiler
zig build

# Compile a ZLang program
./zig-out/bin/zlang examples/hello_world.zl

# Compile with optimizations and specify output name
./zig-out/bin/zlang myprogram.zl -o myprogram -optimize

# Compile with optimizations and specify output name, keep LLVM IR
./zig-out/bin/zlang myprogram.zl -keepll -o myprogram -optimize
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
i32 count = 42;              ?? Signed integers: i8, i16, i32, i64
u64 big_num = 1000000;       ?? Unsigned: u8, u16, u32, u64
f32 temperature = 98.6;      ?? Floats: f32, f64
bool is_active = true;       ?? Booleans
ptr<i32> my_ptr = &count;    ?? Pointers
arr<i32, 100> numbers;       ?? Fixed-size arrays

const i32 MAX = 100;         ?? Constants
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

### Structs

```zl
struct Point {
    x i32,
    y i32
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

?? Pointer arithmetic
p = p + 1;

?? Pass by reference
fun increment(val: ptr<i32>) >> void {
    *val = *val + 1;
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

## 🎪 Unique Features

### Embedded Brainfuck

Yes, really. ZLang has native Brainfuck support:

```zl
fun main() >> i32 {
    u16 x = 5;
    u16 y = 3;
    
    brainfuck {
        ?load x 0?
        ?load y 1?
        [->+<]        ?? Add x to y
        ?store y 1?
    };
    
    @printf("Result: %d\n", y);  ?? Prints: 8
    return 0;
}
```

The Brainfuck compiler integrates directly into the language with special directives for loading/storing variables!

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
- **brainfuck.zl** - Full Brainfuck interpreter in ZLang
- **tests/simd_test.zl** - Comprehensive SIMD examples
- **tests/struct_test.zl** - Struct features
- **tests/loop_test.zl** - All loop variations
- **tests/cast_test.zl** - Type casting examples
- **tests/pointer_test.zl** - Pointer operations

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
- **ABI**: Full System V ABI compatibility for C interop

## 🔧 Building from Source

Requirements:
- Zig (latest)
- Flex
- Bison 3.x+
- LLVM/Clang

```bash
git clone <repo>
cd zlang
zig build
```

## 📝 Syntax Highlighting

Syntax highlighting for popular editors coming soon!

For now, treat `.zl` files as C-like for basic highlighting.

## 🤝 Contributing

Contributions welcome! ZLang is in active development.

Key areas:
- Standard library functions
- More optimization passes
- Better error messages
- IDE/editor plugins

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
