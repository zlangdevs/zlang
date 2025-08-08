#!/bin/bash

# LLVM Installation Script for Zlang Compiler
# Supports Ubuntu/Debian, CentOS/RHEL/Fedora, macOS, and Arch Linux

set -e

echo "🔧 Installing LLVM for Zlang Compiler..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Detect OS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    if command -v apt-get >/dev/null 2>&1; then
        # Ubuntu/Debian
        echo "📦 Detected Ubuntu/Debian system"
        echo "Installing LLVM development packages..."

        sudo apt-get update
        sudo apt-get install -y \
            llvm-16 \
            llvm-16-dev \
            llvm-16-runtime \
            libllvm16 \
            clang-16 \
            libclang-16-dev \
            build-essential \
            cmake \
            pkg-config

        # Create symlinks if needed
        sudo ln -sf /usr/bin/llvm-config-16 /usr/bin/llvm-config || true
        sudo ln -sf /usr/bin/clang-16 /usr/bin/clang || true

        echo "✅ Ubuntu/Debian LLVM installation complete!"

    elif command -v yum >/dev/null 2>&1; then
        # CentOS/RHEL
        echo "📦 Detected CentOS/RHEL system"
        echo "Installing LLVM development packages..."

        sudo yum groupinstall -y "Development Tools"
        sudo yum install -y \
            llvm \
            llvm-devel \
            clang \
            clang-devel \
            cmake \
            pkgconfig

        echo "✅ CentOS/RHEL LLVM installation complete!"

    elif command -v dnf >/dev/null 2>&1; then
        # Fedora
        echo "📦 Detected Fedora system"
        echo "Installing LLVM development packages..."

        sudo dnf groupinstall -y "C Development Tools and Libraries"
        sudo dnf install -y \
            llvm \
            llvm-devel \
            clang \
            clang-devel \
            cmake \
            pkgconf-pkg-config

        echo "✅ Fedora LLVM installation complete!"

    elif command -v pacman >/dev/null 2>&1; then
        # Arch Linux
        echo "📦 Detected Arch Linux system"
        echo "Installing LLVM development packages..."

        sudo pacman -Sy --needed \
            llvm \
            clang \
            cmake \
            base-devel \
            pkgconf

        echo "✅ Arch Linux LLVM installation complete!"

    else
        echo "❌ Unsupported Linux distribution"
        echo "Please install LLVM manually with your package manager"
        exit 1
    fi

elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    echo "📦 Detected macOS system"

    if command -v brew >/dev/null 2>&1; then
        echo "Installing LLVM via Homebrew..."

        brew install llvm cmake pkg-config

        # Add LLVM to PATH
        LLVM_PATH="$(brew --prefix llvm)"
        echo "export PATH=\"$LLVM_PATH/bin:\$PATH\"" >> ~/.zshrc
        echo "export LDFLAGS=\"-L$LLVM_PATH/lib\"" >> ~/.zshrc
        echo "export CPPFLAGS=\"-I$LLVM_PATH/include\"" >> ~/.zshrc

        echo "✅ macOS LLVM installation complete!"
        echo "🔄 Please run 'source ~/.zshrc' or restart your terminal"

    else
        echo "❌ Homebrew not found. Installing Homebrew first..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        echo "Now installing LLVM..."
        brew install llvm cmake pkg-config

        # Add LLVM to PATH
        LLVM_PATH="$(brew --prefix llvm)"
        echo "export PATH=\"$LLVM_PATH/bin:\$PATH\"" >> ~/.zshrc
        echo "export LDFLAGS=\"-L$LLVM_PATH/lib\"" >> ~/.zshrc
        echo "export CPPFLAGS=\"-I$LLVM_PATH/include\"" >> ~/.zshrc

        echo "✅ macOS LLVM installation complete!"
        echo "🔄 Please run 'source ~/.zshrc' or restart your terminal"
    fi

else
    echo "❌ Unsupported operating system: $OSTYPE"
    echo "Please install LLVM manually:"
    echo "  - LLVM development libraries"
    echo "  - Clang compiler"
    echo "  - CMake"
    echo "  - Build tools"
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🎉 LLVM installation completed!"
echo ""
echo "🔍 Verifying installation..."

# Verify LLVM installation
if command -v llvm-config >/dev/null 2>&1; then
    LLVM_VERSION=$(llvm-config --version 2>/dev/null || echo "unknown")
    echo "✅ LLVM version: $LLVM_VERSION"
else
    echo "⚠️  llvm-config not found in PATH"
fi

if command -v clang >/dev/null 2>&1; then
    CLANG_VERSION=$(clang --version | head -n1 || echo "unknown")
    echo "✅ Clang: $CLANG_VERSION"
else
    echo "⚠️  clang not found in PATH"
fi

echo ""
echo "🚀 You can now build Zlang with: zig build"
echo "📚 For more information, see README.md"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
