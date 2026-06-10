# Installing ZLang

## Option 1: AppImage (no build required)

Download the latest AppImage from the [releases page](https://github.com/zlangdevs/zlang/releases):

```bash
chmod +x zlang-x86_64.AppImage
./zlang-x86_64.AppImage -version
```

To use it as `zlang` system-wide:

```bash
sudo mv zlang-x86_64.AppImage /usr/local/bin/zlang
```

The AppImage bundles the compiler and stdlib — no LLVM or other dependencies needed at runtime.

## Option 2: Build from source

### Dependencies

You need Zig 0.16.0 (exact), LLVM 22, Flex, and Bison. Example for Arch Linux:

```bash
sudo pacman -S llvm lld flex bison
```

<details>
<summary>Other distributions</summary>

**Ubuntu / Debian**
```bash
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | sudo gpg --dearmor \
    -o /etc/apt/trusted.gpg.d/llvm-snapshot.gpg
echo "deb http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-22 main" \
    | sudo tee /etc/apt/sources.list.d/llvm.list
sudo apt-get update
sudo apt-get install llvm-22 lld-22 llvm-22-dev flex bison
```

**Fedora**
```bash
sudo microdnf install llvm lld llvm-devel flex bison
```

**Alpine**
```bash
sudo apk add llvm-dev lld flex bison musl-dev
```

**openSUSE Tumbleweed**
```bash
sudo zypper install llvm lld llvm-devel flex bison
```

**Void Linux**
```bash
sudo xbps-install llvm22 llvm22-devel libllvm22 lld flex bison
```

</details>

Install Zig 0.16.0:

```bash
wget https://ziglang.org/download/0.16.0/zig-x86_64-linux-0.16.0.tar.xz
tar -xf zig-x86_64-linux-0.16.0.tar.xz -C /usr/local/
sudo ln -sf /usr/local/zig-x86_64-linux-0.16.0/zig /usr/local/bin/zig
```

### Build and install

```bash
git clone https://github.com/zlangdevs/zlang.git
cd zlang
zig build system-install
```

This installs the compiler to `/usr/local/lib/zlang/` and creates a symlink at `/usr/bin/zlang`.

Custom paths:

```bash
zig build system-install \
    -Dsystem-prefix=/opt/zlang \
    -Dsystem-symlink=/usr/local/bin/zlang
```

### Uninstall

```bash
zig build system-uninstall
```

### Verify

```bash
zlang -version
zlang examples/hello_world.zl
```

## Verified distributions

Build and test suite pass is verified on:

| Distribution | Architectures |
|---|---|
| Ubuntu 24.04 | x86\_64, arm64 |
| Debian (trixie) | x86\_64, arm64 |
| Fedora 40 | x86\_64, arm64 |
| Arch Linux | x86\_64 |
| Alpine (edge) | x86\_64, arm64, riscv64 |
| openSUSE Tumbleweed | x86\_64, arm64, riscv64 |
| Void Linux | x86\_64 |

Verification is automated via `tests/distro_install_test.sh`.

## LLVM detection

`build.zig` locates LLVM automatically via `llvm-config`. If detection fails (multiple LLVM versions installed, non-standard paths), override manually:

```bash
zig build system-install -Dllvm-lib=LLVM-22 -Dllvm-version-major=22
```
