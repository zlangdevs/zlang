# Zlang - A Modern Systems Programming Language 🚀

**Empire starts here** 😈

Zlang is a powerful, modern systems programming language with LLVM-based compilation that combines simplicity with performance. Built for developers who want the control of C with the elegance of modern syntax.

## ✨ Features

- 🔥 **LLVM-powered compilation** - Generate optimized native code
- 🎯 **Simple, clean syntax** - Easy to learn, powerful to use
- 🛠️ **C interoperability** - Seamless integration with existing C libraries
- 🚀 **Zero-cost abstractions** - Performance without compromise
- 📦 **Built-in toolchain** - Parser, compiler, and tools in one package
- 🔧 **Cross-platform** - Works on Linux, macOS, and Windows

## 🚀 Quick Start

### Prerequisites

1. **Install LLVM** (required for compilation):

   ```bash
   chmod +x install_llvm.sh
   ./install_llvm.sh
   ```

2. **Install Zig** (version 0.12+):
   - Download from [ziglang.org](https://ziglang.org/download/)

3. **Install build tools**:

   ```bash
   # Ubuntu/Debian
   sudo apt install flex bison gcc

   # macOS
   brew install flex bison gcc

   # Arch Linux
   sudo pacman -S flex bison gcc
   ```

### Building Zlang

```bash
git clone <repository-url>
cd lang
zig build
```

### Your First Zlang Program

Create `hello.zl`:

```zl
fun main() >> i32 {
    @printf("Hello, Zlang! 🚀");
    return 0;
}
```

Compile and run:

```bash
# Compile to object file
./zig-out/bin/lang hello.zl --output hello.o

# Link and create executable
gcc hello.o -o hello

# Run
./hello
```

## 📚 Language Guide

### Functions

Functions are declared with the `fun` keyword:

```zl
fun greet(name: string) >> void {
    @printf("Hello, %s!", name);
    return;
}

fun add(a: i32, b: i32) >> i32 {
    return a + b;
}
```

### Variables

Variables are declared with type annotations:

```zl
fun main() >> i32 {
    i32 age = 25;
    i32 year;  // Uninitialized

    year = 2024;
    @printf("Age: %d, Year: %d", age, year);

    return 0;
}
```

### Function Calls

- **User functions**: `helper()`
- **C library functions**: `@printf()`, `@puts()`

```zl
fun helper() >> void {
    @puts("Helper called!");
    return;
}

fun main() >> i32 {
    helper();           // Call user function
    @printf("%d", 42);  // Call libc function
    return 0;
}
```

### Types

- `i32` - 32-bit signed integer
- `void` - No return value
- String literals: `"Hello, World!"`

## 🔧 Compiler Usage

The Zlang compiler supports various compilation modes:

### Basic Compilation

```bash
./zig-out/bin/lang program.zl
```

### Options

- `--ast-only` - Show AST without compilation
- `--ir-only` - Show LLVM IR without object generation
- `--output <file>` - Specify output object file name

### Examples

```bash
# View the Abstract Syntax Tree
./zig-out/bin/lang program.zl --ast-only

# Generate and view LLVM IR
./zig-out/bin/lang program.zl --ir-only

# Compile to specific output file
./zig-out/bin/lang program.zl --output my_program.o

# Create executable
gcc my_program.o -o my_program
```

## 🧪 Testing

Run the comprehensive test suite:

```bash
chmod +x run_tests.sh
./run_tests.sh
```

This will test:

- ✅ AST generation
- 🔧 LLVM IR compilation
- 🎯 Object file generation
- 🔗 Full compilation pipeline

## 📁 Project Structure

```
lang/
├── src/
│   ├── main.zig           # Main compiler entry point
│   ├── compiler.zig       # LLVM IR code generator
│   ├── parser.zig         # AST parser integration
│   ├── ast.zig           # AST definitions and utilities
│   ├── lexer/            # Lexical analysis
│   └── parser/           # Grammar and parsing
├── examples/             # Example Zlang programs
├── tests/               # Test cases
├── install_llvm.sh      # LLVM installation script
├── run_tests.sh         # Test runner
└── README.md           # This file
```

## 🎯 Example Programs

### Simple Hello World

```zl
fun main() >> i32 {
    @printf("Hello, World! 🌍");
    return 0;
}
```

### Variables and Math

```zl
fun main() >> i32 {
    i32 x = 42;
    i32 y = 13;
    i32 result = x + y;

    @printf("Result: %d", result);
    return result;
}
```

### Multiple Functions

```zl
fun calculate(value: i32) >> i32 {
    return value * 2;
}

fun main() >> i32 {
    i32 input = 21;
    i32 output = calculate(input);

    @printf("Input: %d, Output: %d", input, output);
    return 0;
}
```

## 🛠️ Development

### Building from Source

1. Clone the repository
2. Install dependencies (LLVM, Zig, Flex, Bison)
3. Run `zig build`

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests with `./run_tests.sh`
5. Submit a pull request

## 🔍 Troubleshooting

### LLVM Not Found

```bash
# Install LLVM using our script
./install_llvm.sh

# Or manually install for your system
sudo apt install llvm-dev  # Ubuntu/Debian
brew install llvm          # macOS
sudo pacman -S llvm        # Arch Linux
```

### Build Errors

- Ensure Zig version 0.12+ is installed
- Check that flex and bison are available
- Verify LLVM development headers are installed

### Runtime Errors

- Make sure to link with `gcc` after compilation
- Check that all referenced functions are declared

## 📄 License

GNU General Public License v3.0 - see LICENSE file for details.

## 🤝 Acknowledgments

- Built with [Zig](https://ziglang.org/) and [LLVM](https://llvm.org/)
- Inspired by modern systems programming languages
- Lexer/Parser built with Flex and Bison

---

**Ready to build the empire?** 😈 Start coding with Zlang today!
