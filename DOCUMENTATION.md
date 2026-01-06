# 📘 ZLang Documentation

Complete language reference and advanced usage guide.

## Table of Contents

- [Compiler Usage](#compiler-usage)
- [Language Specification](#language-specification)
- [Type System](#type-system)
- [Functions](#functions)
- [Memory Management](#memory-management)
- [Advanced Features](#advanced-features)
- [C Interoperability](#c-interoperability)
- [Compiler Internals](#compiler-internals)

---

## Compiler Usage

### Basic Compilation
`
```bash
zlang <input.zl>
```

Compiles a ZLang source file to an executable named `output` (default).

### Compiler Flags

**Output Control**

- `-o <name>` - Specify output executable name
  ```bash
  zlang main.zl -o myprogram
  ```

- `-keepll` - Keep the generated LLVM IR file (`output.ll`)
  ```bash
  zlang main.zl -keepll
  ```

**Optimization**

- `-optimize` - Enable LLVM optimization passes (O2 level)
  ```bash
  zlang main.zl -optimize -o fast_program
  ```

**Debugging & Inspection**

- `-dast` - Display the Abstract Syntax Tree
  ```bash
  zlang main.zl -dast
  ```

- `-verbose` - Verbose output (shows AST and compilation messages)
  ```bash
  zlang main.zl -verbose
  ```

**Linking**

- `-link <file.o>` - Link additional object files
  ```bash
  zlang main.zl -link utils.o -o program
  ```

- `-l<library>` or `-l <library>` - Link against a library
  ```bash
  zlang main.zl -lm              # Link math library
  zlang main.zl -lGL -lGLU      # Link OpenGL
  ```

- `-L<path>` or `-L <path>` - Add library search path
  ```bash
  zlang main.zl -L/usr/local/lib -lraylib
  ```

- `-c` - Compile only, don't link (produces `output.o`)
  ```bash
  zlang module.zl -c -o module.o
  ```

- `-Wl,<options>` - Pass options directly to the linker
  ```bash
  zlang main.zl -Wl,-rpath,/usr/local/lib
  ```

**Target Architecture**

- `-arch <target>` - Specify target architecture
  ```bash
  zlang main.zl -arch x86_64-linux-gnu
  ```

**Brainfuck Mode**

- `-b` - Compile pure Brainfuck files (.b/.bf) with 8-bit cells (default)
  ```bash
  zlang -b mandelbrot.bf -o mandelbrot
  ```

- `-b8` - Compile Brainfuck with 8-bit cells
- `-b16` - Compile Brainfuck with 16-bit cells
- `-b32` - Compile Brainfuck with 32-bit cells
- `-b64` - Compile Brainfuck with 64-bit cells
  ```bash
  zlang -b32 program.bf -o output    # 32-bit cell size
  zlang -b64 bignum.b -optimize      # 64-bit optimized
  ```

### Multi-File Compilation

Compile multiple `.zl` files together:

```bash
zlang main.zl utils.zl helpers.zl -o program
```

Or pass a directory to compile all `.zl` files in it:

```bash
zlang src/ -o program
```

### C Header Wrapper Generation

Generate ZLang bindings from C headers:

```bash
zlang wrap <header.h> -o <output.zl>
```

Example:
```bash
zlang wrap /usr/include/GL/gl.h -o gl_bindings.zl
```

This automatically generates `wrap` statements for C functions.

### Complete Examples

**Basic compilation with optimization:**
```bash
zlang main.zl -optimize -o myapp
```

**Keep IR for debugging:**
```bash
zlang main.zl -keepll -dast
cat output.ll  # Inspect generated LLVM IR
```

**Link with raylib:**
```bash
zlang game.zl -lraylib -lm -o game
```

**Compile with custom library path:**
```bash
zlang app.zl -L/opt/lib -lcustomlib -o app
```

**Multi-file project with optimization:**
```bash
zlang src/main.zl src/renderer.zl src/physics.zl \
  -lGL -lGLU -lm \
  -optimize \
  -o game
```

---

## Language Specification

### Lexical Structure

**Comments**
```zl
?? Single-line comment
?? Comments start with ?? (double question marks)
...
multiline comments
start
and end with
three dots
...
```

**Identifiers**
- Start with letter or underscore: `[a-zA-Z_]`
- Followed by letters, digits, or underscores: `[a-zA-Z0-9_]*`
- Case-sensitive

**Keywords**
```
fun     if      else    for     return  break   continue
const   struct  enum    wrap    use     null    as      goto
```

**Note on `const`**
- Applied to variables: prevents reassignment (`const i32 x = 10;`)
- Applied to pointers: prevents modification through pointer (`const ptr<i32> p;`)
- Provides compile-time safety and documents intent

### Operators

**Arithmetic**: `+`, `-`, `*`, `/`, `%`

**Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`

**Logical**: `&&` (AND), `||` (OR), `!` (NOT)

**Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`

**Assignment**: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`, `&=`, `|=`, `^=`

**Increment/Decrement**: `++`, `--`

**Pointer**: `&` (address-of), `*` (dereference)

**Member Access**: `.` (struct member)

**Type Cast**: `as`

---

## Type System

### Primitive Types

**Integers (Signed)**
- `i8` - 8-bit signed (-128 to 127)
- `i16` - 16-bit signed (-32,768 to 32,767)
- `i32` - 32-bit signed (-2³¹ to 2³¹-1)
- `i64` - 64-bit signed (-2⁶³ to 2⁶³-1)

**Integers (Unsigned)**
- `u8` - 8-bit unsigned (0 to 255)
- `u16` - 16-bit unsigned (0 to 65,535)
- `u32` - 32-bit unsigned (0 to 4,294,967,295)
- `u64` - 64-bit unsigned (0 to 18,446,744,073,709,551,615)

**Floating Point**
- `f16` - 16-bit IEEE 754 half precision
- `f32` - 32-bit IEEE 754 single precision
- `f64` - 64-bit IEEE 754 double precision

**Boolean**
- `bool` - `true` or `false`

**Void**
- `void` - No return value

### Composite Types

**Pointers**
```zl
ptr<i32> int_ptr;            ?? Mutable pointer to mutable i32
ptr<const i32> ro_ptr;       ?? Mutable pointer to const i32 (cannot modify value)
const ptr<i32> fixed_ptr;    ?? Const pointer to i32 (cannot reassign pointer)
const ptr<const i32> both;   ?? Const pointer to const i32 (neither can change)
ptr<ptr<u8>> str_ptr;        ?? Pointer to pointer to u8
ptr<void> generic_ptr;       ?? Generic void pointer
```

**Arrays**
```zl
arr<i32, 100> numbers;           ?? Fixed-size array
arr<arr<f32, 10>, 5> matrix;     ?? 2D array (5x10)
```

**SIMD Vectors**
```zl
simd<f32, 4> vec4;      ?? 4-wide float vector (SSE)
simd<f64, 2> vec2d;     ?? 2-wide double vector
simd<i32, 8> vec8i;     ?? 8-wide int vector (AVX2)
```

**Structs**
```zl
struct TypeName {
    field1 type1,
    field2 type2 = default_value,
    field3 type3
}
```

**Unions**
```zl
union UnionName {
    field1 type1,
    field2 type2,
    field3 type3
}
```

Unions share memory between all fields (like C unions). All fields occupy the same memory location, with the size determined by the largest field.

**Enums**
```zl
enum EnumName {
    VALUE1,
    VALUE2 = explicit_value,
    VALUE3
}
```

### Type Casting

**Explicit Cast**
```zl
i32 x = 42;
f32 y = x as f32;              ?? Cast to specific type
```

**Inferred Cast**
```zl
i32 x = 42;
f32 y = x as _;                ?? Compiler infers target type
```

**Valid Conversions**
- Integer → Integer (truncation/extension)
- Integer → Float (conversion)
- Float → Integer (truncation)
- Float → Float (precision change)
- Pointer → Pointer (reinterpret)
- Integer → Pointer (unsafe, for FFI)
- Pointer → Integer (unsafe, for FFI)

### Truthiness

Any type can be used in boolean context:

```zl
i32 count = 5;
for count {          ?? Non-zero = true
    count--;
}

f32 temp = 98.6;
if temp {            ?? Non-zero = true
    @printf("Temperature set\n");
}

ptr<i32> p = &x;
if p {               ?? Non-null = true
    @printf("Valid pointer\n");
}

bool flag = true;
for flag {           ?? Direct boolean
    flag = false;
}
```

---

## Functions

### Function Declaration

```zl
fun function_name(param1: type1, param2: type2) >> return_type {
    ?? Function body
    return value;
}
```

### Function Syntax Variations

```zl
?? No parameters
fun greet() >> void {
    @printf("Hello!\n");
}

?? Multiple parameters
fun add(a: i32, b: i32) >> i32 {
    return a + b;
}

?? Pointer parameters (pass by reference)
fun modify(val: ptr<i32>) >> void {
    *val = 100;
}

?? Struct parameters (pass by value)
fun print_point(p: Point) >> void {
    @printf("(%d, %d)\n", p.x, p.y);
}
```

### C Function Declarations

**External C Functions**
```zl
fun @printf(format: ptr<u8>) >> i32;
fun @malloc(size: u64) >> ptr<void>;
fun @strcpy(dest: ptr<u8>, src: ptr<u8>) >> ptr<u8>;
```

**Wrapped C Functions**
```zl
?? For automatic ABI compatibility
wrap @some_c_lib_function(x: i32, y: f32) >> ptr<void>;
```

The `wrap` keyword ensures proper System V ABI handling for struct parameters and return values.

**Custom wrappers**
```zl
fun @cfunction() >> i32;
fun cfunction() >> i32 {
	...some preparations...
	@cfunctions();
}
```

### Variadic Arguments

**Declaring Variadic Functions**

ZLang supports C-style variadic arguments using the `vararg<_>` syntax. Variadic parameters must be the last parameter in a function signature.

```zl
?? Declare C printf with variadic arguments
wrap @printf(fmt: ptr<u8>, args: vararg<_>) >> i32;

?? Call with any number of arguments
@printf("Hello, %s!\n", "World");
@printf("Number: %d, Float: %.2f\n", 42, 3.14);
```

**Typed Variadic Arguments**

You can specify a type constraint for variadic arguments. Typed varargs (`vararg<T>`) are a pure ZLang feature that work differently from untyped varargs:

**Untyped (`vararg<_>`)**: Uses C-style va_list, requires intrinsics, C-ABI compatible
**Typed (`vararg<T>`)**: Simpler implementation using pointer + count, pure ZLang

```zl
?? Example: Sum function with typed vararg
fun sum_integers(nums: vararg<i32>) >> i32 {
    i32 total = 0;
    i32 count = @vararg_len(nums);
    
    for i32 i = 0; i < count; i++ {
        total += @vararg_get(nums, i);
    }
    
    return total;
}

fun main() >> i32 {
    i32 result = sum_integers(10, 20, 30, 40);  ?? returns 100
    @printf("Sum: %d\n", result);
    return 0;
}
```

**Typed Vararg Intrinsics**

For `vararg<T>` parameters, use these simpler intrinsics:
- `@vararg_len(vararg_param)` - Returns the number of arguments passed (as `i32`)
- `@vararg_get(vararg_param, index)` - Gets the argument at the specified index (returns type `T`)

**Key Differences**

| Feature | `vararg<_>` (Untyped) | `vararg<T>` (Typed) |
|---------|----------------------|---------------------|
| **Compatibility** | C ABI compatible | Pure ZLang |
| **Type Safety** | Runtime (manual type handling) | Compile-time (known type) |
| **Access Method** | `va_list` + `@va_arg` | `@vararg_len` + `@vararg_get` |
| **Setup Required** | `@va_start` / `@va_end` | None (automatic) |
| **Performance** | Complex (ABI rules) | Simple (array access) |
| **Use Case** | C interop (printf, etc.) | Pure ZLang functions |

```zl
?? Comparison: Untyped vs Typed

?? Untyped vararg - for C compatibility
wrap @printf(fmt: ptr<u8>, args: vararg<_>) >> i32;

fun print_values_untyped(args: vararg<_>) >> void {
    va_list vl;
    @va_start(&vl);
    for i32 i = 0; i < 3; i++ {  ?? Must know count somehow
        i32 val = @va_arg(&vl, "i32");
        @printf("%d ", val);
    }
    @va_end(&vl);
}

?? Typed vararg - simpler, ZLang-specific
fun print_values_typed(values: vararg<i32>) >> void {
    for i32 i = 0; i < @vararg_len(values); i++ {
        i32 val = @vararg_get(values, i);
        @printf("%d ", val);
    }
}

fun main() >> i32 {
    print_values_typed(10, 20, 30);  ?? Cleaner!
    return 0;
}
```

**Accessing Variadic Arguments in ZLang Functions**

To define ZLang functions that process variadic arguments, use the built-in intrinsics:

```zl
?? Define va_list struct for x86_64
struct va_list {
    gp_offset u32,
    fp_offset u32,
    overflow_arg_area ptr<void>,
    reg_save_area ptr<void>
}

fun print_integers(count: i32, args: vararg<_>) >> void {
    va_list vl;
    @va_start(&vl);
    
    for i32 i = 0; i < count; i++ {
        i32 value = @va_arg(&vl, "i32");
        @printf("%d ", value);
    }
    @printf("\n");
    @va_end(&vl);
}

fun main() >> i32 {
    print_integers(3, 10, 20, 30);  ?? Prints: 10 20 30
    return 0;
}
```

**Variadic Intrinsics**

- `@va_start(vl_ptr)` - Initialize a `va_list` to process variadic arguments
- `@va_arg(vl_ptr, "type")` - Retrieve the next argument of the specified type
- `@va_end(vl_ptr)` - Clean up the `va_list` after processing

**Supported Types for @va_arg**

The `@va_arg` intrinsic currently supports (on x86_64 Linux):
- Integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- Floats: `f32`, `f64`
- Pointers: `ptr<T>` for any type `T`

**Usage Notes**

- `vararg<_>` accepts any type (untyped variadic)
- `vararg<T>` accepts only arguments of type `T` (typed variadic)
- Variadic parameters must be the last parameter
- Cannot use `vararg` as a variable type
- The `va_list` struct layout is platform-specific (x86_64 shown above)
- Type promotions follow C rules (e.g., `float` → `double`, `char` → `int`)

**Example: Custom Printf Wrapper**

```zl
wrap @printf(fmt: ptr<u8>, args: vararg<_>) >> i32;

struct va_list {
    gp_offset u32,
    fp_offset u32,
    overflow_arg_area ptr<void>,
    reg_save_area ptr<void>
}

fun log_message(level: ptr<u8>, fmt: ptr<u8>, args: vararg<_>) >> void {
    @printf("[%s] ", level);
    
    ?? Unfortunately, can't forward varargs directly
    ?? Need to re-implement formatting or use printf directly
    @printf(fmt);
    @printf("\n");
}
```

### Recursion

```zl
fun fibonacci(n: i32) >> i32 {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

---

## Memory Management

### Stack Allocation

Variables are stack-allocated by default:

```zl
i32 x = 42;
arr<i32, 100> buffer;
Point p = {10, 20};
```

### Heap Allocation

Use C's malloc/free through `@`:

```zl
ptr<i32> heap_int = @malloc(4) as ptr<i32>;
*heap_int = 100;
@free(heap_int as ptr<void>);
```

For arrays:
```zl
ptr<i32> buffer = @malloc(100 * 4) as ptr<i32>;
buffer[0] = 42;
@free(buffer as ptr<void>);
```

### Const Pointers

**Declaring Const Pointers**

ZLang supports C/C++-style const pointer semantics with two orthogonal concepts:
- **`ptr<const T>`** - Pointer to const data (can't modify value, can reassign pointer)
- **`const ptr<T>`** - Const pointer (can't reassign pointer, can modify value)

```zl
i32 x = 42;
i32 y = 100;

?? Mutable pointer to mutable data
ptr<i32> p1 = &x;
*p1 = 50;                   ?? ✅ OK - can modify value
p1 = &y;                    ?? ✅ OK - can reassign pointer

?? Mutable pointer to const data
ptr<const i32> p2 = &x;
i32 val = *p2;              ?? ✅ OK - can read
*p2 = 50;                   ?? ❌ Error: Cannot modify through pointer to const
p2 = &y;                    ?? ✅ OK - can reassign pointer

?? Const pointer to mutable data
const ptr<i32> p3 = &x;
*p3 = 50;                   ?? ✅ OK - can modify value
p3 = &y;                    ?? ❌ Error: Cannot reassign const variable

?? Const pointer to const data
const ptr<const i32> p4 = &x;
i32 val2 = *p4;             ?? ✅ OK - can read
*p4 = 50;                   ?? ❌ Error: Cannot modify through pointer to const
p4 = &y;                    ?? ❌ Error: Cannot reassign const variable
```

**Compound Assignment**

Pointers to const data prevent compound assignment operations:

```zl
ptr<i32> mutable = &x;
ptr<const i32> readonly = &x;

*mutable += 10;             ?? ✅ OK
*mutable *= 2;              ?? ✅ OK

*readonly += 10;            ?? ❌ Compile error!
*readonly *= 2;             ?? ❌ Compile error!
```

**Use Cases**

1. **Function Parameters** - Prevent accidental modification:
   ```zl
   fun calculate_sum(data: ptr<const i32>, size: i32) >> i32 {
       i32 sum = 0;
       for i32 i = 0; i < size; i++ {
           sum += data[i];  ?? Safe: read-only access
       }
       return sum;
   }
   ```

2. **Shared Data** - Multiple readers, no writers:
   ```zl
   ptr<const arr<f32, 1000>> shared_data = &buffer;
   fun process(data: ptr<const arr<f32, 1000>>) >> void {
       ?? Can read but not modify shared data
   }
   ```

3. **API Guarantees** - Document intent in function signatures:
   ```zl
   fun write_to_file(data: ptr<const u8>, size: i32) >> void {
       ?? Caller knows data won't be modified
   }
   ```

**Memory Safety**

Const pointers provide compile-time guarantees:
- ✅ Prevent accidental modification
- ✅ Document read-only intent
- ✅ Enable compiler optimizations
- ✅ Catch errors at compile time, not runtime

### Pointer Arithmetic

```zl
ptr<i32> p = &array[0];
p = p + 1;              ?? Move to next element
i32 val = *(p + 2);     ?? Access element at offset

?? Works with const data pointers (pointer can move, data is read-only)
ptr<const i32> cp = &array[0];
i32 val = *cp;          ?? OK: can read
cp = cp + 1;            ?? OK: pointer address can change  
*cp = 10;               ?? Error: cannot modify through pointer to const
```

### Struct and Union Layout

**Structs** follow C ABI layout rules:
- Fields are laid out in declaration order
- Padding follows platform alignment rules
- Compatible with C structs

```zl
struct Example {
    a i8,        ?? 1 byte + 3 bytes padding
    b i32,       ?? 4 bytes (aligned)
    c i16        ?? 2 bytes + 2 bytes padding
}
?? Total size: 12 bytes (platform dependent)
```

**Unions** share memory between all fields:
- All fields start at offset 0
- Size is determined by the largest field
- Alignment matches the most-aligned field
- Compatible with C unions

```zl
union Value {
    i i32,       ?? 4 bytes
    f f32,       ?? 4 bytes
    b bool       ?? 1 byte (padded to 4)
}
?? Total size: 4 bytes (all fields overlap)

union Data {
    small i32,          ?? 4 bytes
    large arr<u8, 16>   ?? 16 bytes
}
?? Total size: 16 bytes (size of largest field)
```

---

## Advanced Features

### SIMD Programming

**Vector Creation**
```zl
simd<f32, 4> v = {1.0, 2.0, 3.0, 4.0};
```

**Arithmetic Operations**
```zl
simd<f32, 4> a = {1.0, 2.0, 3.0, 4.0};
simd<f32, 4> b = {5.0, 6.0, 7.0, 8.0};

simd<f32, 4> sum = a + b;        ?? {6, 8, 10, 12}
simd<f32, 4> diff = b - a;       ?? {4, 4, 4, 4}
simd<f32, 4> product = a * b;    ?? {5, 12, 21, 32}
simd<f32, 4> quotient = b / a;   ?? {5, 3, 2.33, 2}
```

**Element Access**
```zl
f32 first = v[0];
v[2] = 100.0;
```

**Supported Vector Sizes**
- 2, 4, 8, 16, 32 elements
- Types: i8, i16, i32, i64, u8, u16, u32, u64, f16, f32, f64

### Embedded Brainfuck

**Basic Syntax**
```zl
brainfuck {
    +++       ?? Increment cell
    [->+<]    ?? While loop
    .         ?? Output
}
```

**Variable Integration**
```zl
u16 x = 10;
u16 result = 0;

brainfuck {
    ?cell_size 16?
    ?load x 0?         ?? Load x into cell 0
    ?load result 1?    ?? Load result into cell 1
    [->+<]            ?? Add x to result
}
```

**Directives**
- `?load variable index?` - Load ZLang variable into Brainfuck cell
- `?store variable index?` - Store Brainfuck cell into ZLang variable
- `?cell_size N?` - Set cell size (8, 16, 32, or 64 bits)
- `?len N?` - Set tape length (number of cells)

### Standalone Brainfuck Compilation

ZLang can compile pure Brainfuck programs directly to native executables:

```bash
# Compile .bf or .b files with default 8-bit cells
zlang -b mandelbrot.bf -o mandelbrot

# Specify cell size for larger values
zlang -b8 program.bf -o output     # 8-bit cells (0-255)
zlang -b16 program.bf -o output    # 16-bit cells (0-65535)
zlang -b32 program.bf -o output    # 32-bit cells
zlang -b64 program.bf -o output    # 64-bit cells

# Combine with other flags
zlang -b32 program.bf -o fast_bf -optimize -keepll
```

**How it works:**
- The `-b` flags enable brainfuck-only mode
- Input must be a `.b` or `.bf` file containing pure Brainfuck code
- Compiles to LLVM IR, then to native machine code
- Cell size affects the range of values each cell can hold
- Default tape length is 30,000 cells
- Standard Brainfuck I/O: `,` reads stdin byte, `.` writes stdout byte

**Example:**
```bash
# Classic "Hello World" in Brainfuck
echo '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.' > hello.bf
zlang -b hello.bf -o hello
./hello
# Output: Hello World!
```

### Numeric Literals

**Decimal with Separators**
```zl
i32 million = 1'000'000;
i64 huge = 999'999'999'999;
f32 pi = 3.141'592'653;
```

**Hexadecimal**
```zl
i32 color = 0xFF00AA;
u32 mask = 0xDEAD'BEEF;
```

**Binary**
```zl
i32 flags = 0b1010'1100;
u8 byte = 0b11110000;
```

**Octal**
```zl
i32 perms = 0o755;
```

### Advanced Loop Control

**Break and Continue**
```zl
for i32 i = 0; i < 100; i++ {
    if i == 50 {
        break;        ?? Exit loop
    }
    if i % 2 == 0 {
        continue;     ?? Skip to next iteration
    }
    @printf("%d\n", i);
}
```

**Nested Loop Control**
```zl
for i32 i = 0; i < 10; i++ {
    for i32 j = 0; j < 10; j++ {
        if j == 5 {
            break;    ?? Breaks inner loop only
        }
    }
}
```

### Goto Statements

**Basic Goto**

Goto statements allow unconditional jumps to labeled positions in code. Labels are defined with an identifier followed by a colon.

```zl
fun example() >> i32 {
    i32 x = 10;
    goto skip;
    x = 20;           ?? This code is skipped
    skip:
    return x;         ?? Returns 10
}
```

**Forward Jumps**

Jump ahead to skip code sections:

```zl
fun check_value(n: i32) >> i32 {
    if n > 0 {
        goto positive;
    }
    return 0;
    
    positive:
    return 1;
}
```

**Backward Jumps (Loops)**

Create loops by jumping backward:

```zl
fun count_to_five() >> i32 {
    i32 count = 0;
    start:
    count = count + 1;
    if count < 5 {
        goto start;
    }
    return count;     ?? Returns 5
}
```

**Multiple Gotos to Same Label**

Multiple goto statements can target the same label:

```zl
fun find_special(n: i32) >> i32 {
    if n == 1 {
        goto found;
    }
    if n == 2 {
        goto found;
    }
    if n == 3 {
        goto found;
    }
    return 0;
    
    found:
    return 42;
}
```

**Usage Notes**
- Labels must be unique within a function
- Goto can jump forward or backward within the same function
- Cannot jump into or out of functions
- Useful for error handling and cleanup patterns
- Compatible with C-style control flow

**Example: Error Handling Pattern**

```zl
fun process_file(filename: ptr<u8>) >> i32 {
    ptr<void> file = @fopen(filename, "r");
    if file == null {
        goto error;
    }
    
    ptr<void> buffer = @malloc(1024);
    if buffer == null {
        goto cleanup_file;
    }
    
    ?? Process file...
    
    @free(buffer as ptr<void>);
    cleanup_file:
    @fclose(file);
    return 0;
    
    error:
    return -1;
}
```

---

## C Interoperability

### Calling C Functions

**Standard Library**
```zl
fun @printf(format: ptr<u8>) >> i32;
fun @scanf(format: ptr<u8>) >> i32;
fun @strlen(s: ptr<u8>) >> u64;
```

**Memory Functions**
```zl
fun @malloc(size: u64) >> ptr<void>;
fun @calloc(num: u64, size: u64) >> ptr<void>;
fun @realloc(ptr: ptr<void>, size: u64) >> ptr<void>;
fun @free(ptr: ptr<void>) >> void;
```

**String Functions**
```zl
fun @strcpy(dest: ptr<u8>, src: ptr<u8>) >> ptr<u8>;
fun @strcmp(s1: ptr<u8>, s2: ptr<u8>) >> i32;
fun @strcat(dest: ptr<u8>, src: ptr<u8>) >> ptr<u8>;
```

### System V ABI Compliance

ZLang partly supports System V ABI for Linux/Unix:

**Small Structs** (≤16 bytes)
```zl
struct Small {
    x i32,
    y i32
}

?? Passed in registers (RDI, RSI, etc.)
fun process(s: Small) >> void { }
```

**Large Structs** (>16 bytes)
```zl
struct Large {
    data arr<i32, 10>
}

?? Passed by hidden pointer (sret)
fun @LoadImage(result: ptr<Image>, path: ptr<u8>) >> void;

fun LoadImage(path: ptr<u8>) >> Image {
    Image result;
    @LoadImage(&result, path);
    return result;
}
```

### Using C Libraries

**Example: Using raylib**
```zl
?? Declare C functions
fun @InitWindow(width: i32, height: i32, title: ptr<u8>) >> void;
fun @CloseWindow() >> void;
fun @WindowShouldClose() >> bool;
fun @BeginDrawing() >> void;
fun @EndDrawing() >> void;
fun @ClearBackground(color: Color) >> void;

struct Color {
    r u8,
    g u8,
    b u8,
    a u8
}

fun main() >> i32 {
    @InitWindow(800, 600, "ZLang Game");
    
    Color red = {255, 0, 0, 255};
    
    for !@WindowShouldClose() {
        @BeginDrawing();
        @ClearBackground(red);
        @EndDrawing();
    }
    
    @CloseWindow();
    return 0;
}
```

---

## Compiler Internals

### Compilation Pipeline

1. **Lexical Analysis** (Flex)
   - Tokenizes source code
   - Handles comments, strings, numbers
   - Context-aware `>>` handling for generics

2. **Parsing** (Bison GLR)
   - Builds Abstract Syntax Tree
   - Handles ambiguities with GLR
   - 3 shift/reduce conflicts (intentional)

3. **Code Generation** (Zig + LLVM)
   - Traverses AST
   - Generates LLVM IR
   - Applies optimizations

4. **Linking** (Clang/LLVM)
   - Links with C runtime
   - Produces native binary

### LLVM IR Generation

**Example Transformation**

ZLang:
```zl
fun add(a: i32, b: i32) >> i32 {
    return a + b;
}
```
| ZLang Type | LLVM Type |
|------------|-----------|
| `i8` | `i8` |
| `i32` | `i32` |
| `i64` | `i64` |
| `f16` | `half` |
| `f32` | `float` |
| `f64` | `double` |
| `bool` | `i1` |
| `ptr<T>` | `ptr` (opaque) |
| `arr<T, N>` | `[N x T]` |
| `simd<T, N>` | `<N x T>` |

### Optimization Passes

Compiler applies standard LLVM optimizations:
- Dead code elimination
- Constant folding
- Inline expansion
- Loop unrolling (for small loops)
- SIMD vectorization

### Error Handling

**Parse Errors**
```
Parse error at file.zl:10:5: error.ParseFailed
```

**Type Errors**
```
Type mismatch: expected i32, found f32
```

**Semantic Errors**
- Undefined variables
- Type mismatches
- Invalid casts
- Unmatched break/continue

---

## Best Practices

### Naming Conventions

```zl
?? Functions: snake_case
fun calculate_total() >> i32 { }

?? Types: PascalCase
struct UserProfile { }
enum StatusCode { }

?? Constants: SCREAMING_SNAKE_CASE
const i32 MAX_BUFFER = 1024;

?? Variables: snake_case
i32 user_count = 0;
```

### Memory Safety

```zl
?? Always check malloc results
ptr<i32> buffer = @malloc(size) as ptr<i32>;
if buffer == null {
    @printf("Allocation failed!\n");
    return 1;
}

?? Free allocated memory
@free(buffer as ptr<void>);
```

### Performance Tips

1. **Use SIMD for data-parallel operations**
   ```zl
   simd<f32, 4> v1, v2, result;
   result = v1 + v2;  ?? 4x faster than scalar
   ```

2. **Pass large structs by pointer**
   ```zl
   fun process(data: ptr<LargeStruct>) >> void {
       ?? Avoid copying large data
   }
   ```

3. **Use const for read-only data**
   ```zl
   const i32 TABLE_SIZE = 1000;
   const ptr<i32> readonly = &data;  ?? Prevent modification
   ```

4. **Prefer stack allocation when possible**
   ```zl
   arr<i32, 100> buffer;  ?? Stack (fast)
   ?? vs
   ptr<i32> buffer = @malloc(400);  ?? Heap (slower)
   ```

---

## Common Patterns

### Tagged Unions

Unions can be combined with a tag field to create safe discriminated unions:

```zl
union Value {
    i i32,
    f f32,
    p ptr<u8>
}

struct TaggedValue {
    tag u8,    ?? 0=int, 1=float, 2=pointer
    data Value
}

fun print_value(tv: TaggedValue) >> void {
    if tv.tag == 0 {
        @printf("Integer: %d\n", tv.data.i);
    } else if tv.tag == 1 {
        @printf("Float: %f\n", tv.data.f);
    } else if tv.tag == 2 {
        @printf("Pointer: %p\n", tv.data.p);
    }
}

fun main() >> i32 {
    TaggedValue tv;
    tv.tag = 0;
    tv.data.i = 42;
    print_value(tv);
    
    tv.tag = 1;
    tv.data.f = 3.14;
    print_value(tv);
    
    return 0;
}
```

### Option Type Simulation

```zl
struct Option_i32 {
    has_value bool,
    value i32
}

fun some(val: i32) >> Option_i32 {
    return {true, val};
}

fun none() >> Option_i32 {
    return {false, 0};
}

fun main() >> i32 {
    Option_i32 opt = some(42);
    if opt.has_value {
        @printf("Value: %d\n", opt.value);
    }
    return 0;
}
```

### Iterator Pattern

```zl
struct Iterator {
    current ptr<i32>,
    end ptr<i32>
}

fun next(it: ptr<Iterator>) >> bool {
    if it.current >= it.end {
        return false;
    }
    it.current = it.current + 1;
    return true;
}
```

### Generic-like Functions (via macros or templates)

ZLang doesn't have generics yet, but you can use C preprocessor:

```c
// In a .h file
#define DEFINE_ARRAY(T, N) \
struct Array_##T { \
    arr<T, N> data; \
    i32 length; \
}
```

---

## Troubleshooting

### Common Errors

**"Parse error at line X"**
- Check syntax (missing semicolons, braces)
- Verify `>>` in function signatures
- Ensure comments use `??`

**"Type mismatch"**
- Add explicit cast with `as`
- Check function parameter types
- Verify struct field types

**"Undefined reference"**
- Declare C functions with `@` prefix
- Link required libraries with `-l` flag

### Debugging

**Keep LLVM IR**
```bash
./zig-out/bin/zlang program.zl -keepll
cat output.ll  ?? Inspect generated IR
```

**Compile with debug info**
```bash
clang -g output.ll -o program
gdb ./program
```

---

## Future Roadmap

Planned features:
- True generics/templates
- Standard library documentation
- Package manager
- Better error messages
- IDE integration
- More optimization passes
- Module system improvements

---

**Happy coding with ZLang! 😈⚡**
