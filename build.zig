const std = @import("std");

fn makeNoOp(_: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const system_prefix = b.option([]const u8, "system-prefix", "Install root for zlang binary and stdlib") orelse "/usr/local/lib/zlang";
    const system_symlink = b.option([]const u8, "system-symlink", "Symlink path for zlang executable") orelse "/usr/bin/zlang";

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zlang",
        .root_module = exe_mod,
    });

    const build_step = b.step("build", "Build zlang compiler");
    b.default_step = build_step;
    build_step.dependOn(&exe.step);

    const appimage_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        \\set -eu
        \\BIN="$1"
        \\ARCH="$(uname -m)"
        \\case "$ARCH" in
        \\  x86_64|amd64) TOOL_ARCH="x86_64" ;;
        \\  aarch64|arm64) TOOL_ARCH="aarch64" ;;
        \\  *)
        \\    echo "Unsupported architecture for appimagetool: $ARCH" >&2
        \\    exit 1
        \\    ;;
        \\esac
        \\WORK_DIR="$(mktemp -d)"
        \\cleanup() {
        \\  rm -rf "$WORK_DIR"
        \\}
        \\trap cleanup EXIT INT TERM
        \\APPDIR="$WORK_DIR/AppDir"
        \\TOOLS_DIR="$WORK_DIR/tools"
        \\APPIMAGE_OUT="zig-out/zlang-$ARCH.AppImage"
        \\APPIMAGETOOL="$TOOLS_DIR/appimagetool.AppImage"
        \\mkdir -p "$APPDIR/usr/bin" "$APPDIR/usr/share/zlang" "$APPDIR/usr/lib" "$TOOLS_DIR" "zig-out"
        \\install -m 755 "$BIN" "$APPDIR/usr/bin/zlang"
        \\cp -a "stdlib" "$APPDIR/usr/share/zlang/stdlib"
        \\find_tool() {
        \\  for name in "$@"; do
        \\    if command -v "$name" >/dev/null 2>&1; then
        \\      command -v "$name"
        \\      return 0
        \\    fi
        \\  done
        \\  return 1
        \\}
        \\copy_tool() {
        \\  tool_target="$1"
        \\  shift
        \\  tool_path="$(find_tool "$@" || true)"
        \\  if [ -n "$tool_path" ]; then
        \\    cp -L "$tool_path" "$APPDIR/usr/bin/$tool_target"
        \\    chmod 755 "$APPDIR/usr/bin/$tool_target"
        \\  fi
        \\}
        \\copy_tool "llc" llc-20 llc-19 llc-18 llc
        \\copy_tool "opt" opt-20 opt-19 opt-18 opt
        \\copy_tool "clang" clang-20 clang-19 clang-18 clang
        \\copy_tool "lli" lli-20 lli-19 lli-18 lli
        \\copy_tool "ld.lld" ld.lld-20 ld.lld-19 ld.lld-18 ld.lld
        \\collect_libs() {
        \\  bin_path="$1"
        \\  [ -x "$bin_path" ] || return 0
        \\  ldd "$bin_path" 2>/dev/null | while IFS= read -r line; do
        \\    lib_path=""
        \\    case "$line" in
        \\      *" => "*)
        \\        lib_path="$(printf "%s\n" "$line" | cut -d' ' -f3)"
        \\        ;;
        \\      /*)
        \\        lib_path="$(printf "%s\n" "$line" | cut -d' ' -f1)"
        \\        ;;
        \\    esac
        \\    if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
        \\      cp -nL "$lib_path" "$APPDIR/usr/lib/" 2>/dev/null || true
        \\    fi
        \\  done
        \\}
        \\collect_libs "$APPDIR/usr/bin/zlang"
        \\for bundled in "$APPDIR/usr/bin/"*; do
        \\  collect_libs "$bundled"
        \\done
        \\cat > "$APPDIR/AppRun" <<'EOF'
        \\#!/bin/sh
        \\HERE="$(dirname "$(readlink -f "$0")")"
        \\export ZSTDPATH="$HERE/usr/share/zlang/stdlib"
        \\export PATH="$HERE/usr/bin:$PATH"
        \\export LD_LIBRARY_PATH="$HERE/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
        \\exec "$HERE/usr/bin/zlang" "$@"
        \\EOF
        \\chmod 755 "$APPDIR/AppRun"
        \\cat > "$APPDIR/zlang.desktop" <<'EOF'
        \\[Desktop Entry]
        \\Type=Application
        \\Name=ZLang
        \\Exec=zlang
        \\Icon=zlang
        \\Terminal=true
        \\Categories=Development;
        \\EOF
        \\cat > "$APPDIR/zlang.xpm" <<'EOF'
        \\/* XPM */
        \\static char * zlang_xpm[] = {
        \\"16 16 2 1",
        \\"  c None",
        \\". c #2D7A3E",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................",
        \\"................"};
        \\EOF
        \\if command -v curl >/dev/null 2>&1; then
        \\  curl -fsSL "https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-$TOOL_ARCH.AppImage" -o "$APPIMAGETOOL"
        \\elif command -v wget >/dev/null 2>&1; then
        \\  wget -qO "$APPIMAGETOOL" "https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-$TOOL_ARCH.AppImage"
        \\else
        \\  echo "Need curl or wget to download appimagetool" >&2
        \\  exit 1
        \\fi
        \\chmod 755 "$APPIMAGETOOL"
        \\rm -f "$APPIMAGE_OUT"
        \\ARCH="$TOOL_ARCH" APPIMAGE_EXTRACT_AND_RUN=1 "$APPIMAGETOOL" "$APPDIR" "$APPIMAGE_OUT"
        \\chmod 755 "$APPIMAGE_OUT"
        \\printf 'AppImage created: %s\n' "$APPIMAGE_OUT"
        ,
        "zlang-appimage",
    });
    appimage_cmd.addFileArg(exe.getEmittedBin());
    appimage_cmd.step.dependOn(&exe.step);

    const appimage_step = b.step("appimage", "Build portable AppImage bundle");
    appimage_step.dependOn(&appimage_cmd.step);

    // generating and linking parser (must be first to generate parser.h)
    const bison_cmd = b.addSystemCommand(&[_][]const u8{ "bison", "-d", "-o", "src/parser/parser.c", "src/parser/parser.y" });

    // generating and linking lexer (depends on parser.h)
    const flex_cmd = b.addSystemCommand(&[_][]const u8{ "flex", "-o", "src/lexer/lexer.c", "src/lexer/lexer.l" });
    flex_cmd.step.dependOn(&bison_cmd.step);
    exe.addCSourceFile(.{
        .file = b.path("src/lexer/lexer.c"),
        .flags = &[_][]const u8{
            "-std=gnu99",
            "-I./src/lexer",
            "-I./src/parser",
            "-D_GNU_SOURCE",
            "-Wall",
            "-Wno-unused-function",
        },
    });
    exe.addCSourceFile(.{
        .file = b.path("src/parser/parser.c"),
        .flags = &[_][]const u8{
            "-std=gnu99",
            "-I./src/parser",
            "-I./src/lexer",
            "-D_GNU_SOURCE",
            "-Wall",
            "-Wno-unused-function",
            "-Wno-unused-variable",
        },
    });

    //exe.linkSystemLibrary("fl");
    exe.linkLibC();
    exe.linkSystemLibrary("LLVM-20");
    exe.step.dependOn(&flex_cmd.step);
    exe.step.dependOn(&bison_cmd.step);

    const install_system_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        b.fmt(
            "set -e; if [ \"$(id -u)\" -ne 0 ]; then echo \"Error: zig build install requires root (use sudo).\"; exit 1; fi; install -d \"{0s}\"; rm -f \"{0s}/zlang\"; rm -rf \"{0s}/stdlib\"; install -m 755 \"$1\" \"{0s}/zlang\"; cp -a \"stdlib\" \"{0s}/stdlib\"; ln -sfn \"{0s}/zlang\" \"{1s}\"",
            .{ system_prefix, system_symlink },
        ),
        "zlang-install",
    });
    install_system_cmd.addFileArg(exe.getEmittedBin());
    install_system_cmd.step.dependOn(&exe.step);

    b.getInstallStep().dependOn(&install_system_cmd.step);

    const uninstall_system_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        b.fmt(
            "set -e; if [ \"$(id -u)\" -ne 0 ]; then echo \"Error: zig build uninstall requires root (use sudo).\"; exit 1; fi; if [ -e \"{1s}\" ] && [ ! -L \"{1s}\" ]; then echo \"Refusing to remove non-symlink: {1s}\"; exit 1; fi; rm -f \"{1s}\"; rm -rf \"{0s}\"",
            .{ system_prefix, system_symlink },
        ),
    });
    const uninstall_step = b.getUninstallStep();
    uninstall_step.makeFn = makeNoOp;
    uninstall_step.dependOn(&uninstall_system_cmd.step);

    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
