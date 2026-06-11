const std = @import("std");
const builtin = @import("builtin");

fn makeNoOp(_: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {}

const LlvmLinkInfo = struct {
    lib_name: []const u8,
    version_major: u32,
    include_dir: ?[]const u8 = null,
    lib_dir: ?[]const u8 = null,
};

fn parseLlvmMajor(version: []const u8) ?u32 {
    const trimmed = std.mem.trim(u8, version, " \t\r\n");
    const dot = std.mem.indexOfScalar(u8, trimmed, '.') orelse trimmed.len;
    if (dot == 0) return null;
    return std.fmt.parseInt(u32, trimmed[0..dot], 10) catch null;
}

fn llvmLibNameFromArg(allocator: std.mem.Allocator, arg: []const u8) ?[]const u8 {
    const trimmed = std.mem.trim(u8, arg, " \t\r\n");
    if (!std.mem.startsWith(u8, trimmed, "-l")) return null;
    if (!std.mem.startsWith(u8, trimmed[2..], "LLVM")) return null;
    return allocator.dupe(u8, trimmed[2..]) catch null;
}

fn llvmLibNameFromFile(allocator: std.mem.Allocator, name: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, name, "libLLVM")) return null;
    const without_prefix = name[3..];
    const suffixes = [_][]const u8{ ".so", ".a", ".dylib" };
    for (suffixes) |suffix| {
        if (std.mem.endsWith(u8, without_prefix, suffix)) {
            return allocator.dupe(u8, without_prefix[0 .. without_prefix.len - suffix.len]) catch null;
        }
    }
    if (std.mem.indexOf(u8, without_prefix, ".so.")) |idx| {
        return allocator.dupe(u8, without_prefix[0..idx]) catch null;
    }
    return null;
}

fn majorFromLlvmLibName(lib_name: []const u8) u32 {
    const dash = std.mem.lastIndexOfScalar(u8, lib_name, '-') orelse return 0;
    return std.fmt.parseInt(u32, lib_name[dash + 1 ..], 10) catch 0;
}

fn detectLlvmLinkInfo(b: *std.Build) LlvmLinkInfo {
    const allocator = b.allocator;
    var candidates: std.ArrayList([]const u8) = .empty;
    defer candidates.deinit(allocator);

    candidates.append(allocator, "llvm-config") catch {};
    var version: u32 = 40;
    while (version >= 14) : (version -= 1) {
        candidates.append(allocator, std.fmt.allocPrint(allocator, "llvm-config-{d}", .{version}) catch continue) catch {};
        candidates.append(allocator, std.fmt.allocPrint(allocator, "llvm-config{d}", .{version}) catch continue) catch {};
    }

    for (candidates.items) |exe| {
        var out_code: u8 = 0;
        const stdout = b.runAllowFail(&[_][]const u8{ exe, "--libs" }, &out_code, .ignore) catch continue;

        var lib_name: ?[]const u8 = null;
        var it = std.mem.tokenizeAny(u8, stdout, " \t\r\n");
        while (it.next()) |arg| {
            lib_name = llvmLibNameFromArg(allocator, arg);
            if (lib_name != null) break;
        }
        if (lib_name == null) continue;

        var detected_major = majorFromLlvmLibName(lib_name.?);
        if (detected_major == 0) {
            const version_stdout = b.runAllowFail(&[_][]const u8{ exe, "--version" }, &out_code, .ignore) catch return .{ .lib_name = lib_name.?, .version_major = 0 };
            detected_major = parseLlvmMajor(version_stdout) orelse 0;
        }

        const inc_raw = b.runAllowFail(&[_][]const u8{ exe, "--includedir" }, &out_code, .ignore) catch "";
        const lib_raw = b.runAllowFail(&[_][]const u8{ exe, "--libdir" }, &out_code, .ignore) catch "";
        const inc_trimmed = std.mem.trim(u8, inc_raw, " \t\r\n");
        const lib_trimmed = std.mem.trim(u8, lib_raw, " \t\r\n");
        const include_dir = if (inc_trimmed.len > 0) allocator.dupe(u8, inc_trimmed) catch null else null;
        const lib_dir = if (lib_trimmed.len > 0) allocator.dupe(u8, lib_trimmed) catch null else null;

        return .{ .lib_name = lib_name.?, .version_major = detected_major, .include_dir = include_dir, .lib_dir = lib_dir };
    }

    var best_name: ?[]const u8 = null;
    var best_major: u32 = 0;
    var best_lib_dir: ?[]const u8 = null;
    var best_inc_dir: ?[]const u8 = null;
    var out_code: u8 = 0;
    const lib_names = b.runAllowFail(&[_][]const u8{
        "sh",
        "-c",
        \\for d in /usr/local/lib /lib64 /lib /usr/lib64 /usr/lib $(echo /usr/lib/llvm/*/lib 2>/dev/null); do
        \\  for f in "$d"/libLLVM*.so "$d"/libLLVM*.so.* "$d"/libLLVM*.a "$d"/libLLVM*.dylib; do
        \\    [ -e "$f" ] && printf '%s\t%s\n' "$(basename "$f")" "$d"
        \\  done
        \\done
    }, &out_code, .ignore) catch "";
    var lib_it = std.mem.tokenizeAny(u8, lib_names, "\r\n");
    while (lib_it.next()) |line| {
        const tab = std.mem.indexOfScalar(u8, line, '\t') orelse continue;
        const file_name = line[0..tab];
        const dir = line[tab + 1 ..];
        const lib_name = llvmLibNameFromFile(allocator, file_name) orelse continue;
        const major = majorFromLlvmLibName(lib_name);
        if (major >= best_major) {
            best_name = lib_name;
            best_major = major;
            best_lib_dir = allocator.dupe(u8, dir) catch null;
            if (std.fs.path.dirname(dir)) |p|
                best_inc_dir = std.fs.path.join(allocator, &[_][]const u8{ p, "include" }) catch null;
        }
    }
    if (best_name) |lib_name| return .{ .lib_name = lib_name, .version_major = best_major, .lib_dir = best_lib_dir, .include_dir = best_inc_dir };

    return .{ .lib_name = "LLVM", .version_major = 0 };
}

fn addCommonSystemLibraryPaths(mod: *std.Build.Module, llvm_info: LlvmLinkInfo) void {
    const paths = [_][]const u8{ "/usr/local/lib", "/lib64", "/lib", "/usr/lib64", "/usr/lib" };
    for (paths) |path| {
        mod.addLibraryPath(.{ .cwd_relative = path });
    }
    if (llvm_info.lib_dir) |dir| mod.addLibraryPath(.{ .cwd_relative = dir });
}

fn addCommonSystemIncludePaths(mod: *std.Build.Module, llvm_info: LlvmLinkInfo) void {
    const paths = [_][]const u8{ "/usr/local/include", "/usr/include" };
    for (paths) |path| {
        mod.addIncludePath(.{ .cwd_relative = path });
    }
    if (llvm_info.include_dir) |dir| mod.addIncludePath(.{ .cwd_relative = dir });
}

pub fn build(b: *std.Build) void {
    const default_target: std.Target.Query = if (builtin.os.tag == .linux and builtin.cpu.arch == .x86_64)
        .{
            .cpu_arch = .x86_64,
            .os_tag = .linux,
            .abi = .gnu,
            .glibc_version = .{ .major = 2, .minor = 17, .patch = 0 },
        }
    else
        .{};
    const target = b.standardTargetOptions(.{ .default_target = default_target });
    const optimize = b.standardOptimizeOption(.{});
    const appimage_optimize: std.builtin.OptimizeMode = .ReleaseFast;
    const system_prefix = b.option([]const u8, "system-prefix", "Install root for zlang binary and stdlib") orelse "/usr/local/lib/zlang";
    const system_symlink = b.option([]const u8, "system-symlink", "Symlink path for zlang executable") orelse "/usr/bin/zlang";
    const llvm_lib_option = b.option([]const u8, "llvm-lib", "LLVM library name to link against (e.g. LLVM-22, LLVM)");
    const llvm_version_option = b.option(u32, "llvm-version-major", "LLVM major version to show in -stats");
    const detected_llvm = if (llvm_lib_option == null or llvm_version_option == null) detectLlvmLinkInfo(b) else LlvmLinkInfo{ .lib_name = llvm_lib_option.?, .version_major = llvm_version_option.? };
    const llvm_lib = llvm_lib_option orelse detected_llvm.lib_name;
    const llvm_version_major = llvm_version_option orelse blk: {
        const major = majorFromLlvmLibName(llvm_lib);
        break :blk if (major != 0) major else detected_llvm.version_major;
    };

    const build_options = b.addOptions();
    build_options.addOption(u32, "llvm_version_major", llvm_version_major);

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addOptions("build_options", build_options);

    const exe = b.addExecutable(.{
        .name = "zlang",
        .root_module = exe_mod,
    });

    const appimage_exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = appimage_optimize,
    });
    appimage_exe_mod.addOptions("build_options", build_options);

    const appimage_exe = b.addExecutable(.{
        .name = "zlang-appimage",
        .root_module = appimage_exe_mod,
    });

    const install_exe = b.addInstallArtifact(exe, .{});
    b.getInstallStep().dependOn(&install_exe.step);

    const build_step = b.step("build", "Build zlang compiler");
    b.default_step = build_step;
    build_step.dependOn(&install_exe.step);

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
        \\copy_tool "llc" llc-22 llc22 llc-21 llc21 llc-20 llc20 llc-19 llc19 llc-18 llc18 llc
        \\copy_tool "opt" opt-22 opt22 opt-21 opt21 opt-20 opt20 opt-19 opt19 opt-18 opt18 opt
        \\copy_tool "clang" clang-22 clang22 clang-21 clang21 clang-20 clang20 clang-19 clang19 clang-18 clang18 clang
        \\copy_tool "lli" lli-22 lli22 lli-21 lli21 lli-20 lli20 lli-19 lli19 lli-18 lli18 lli
        \\copy_tool "ld.lld" ld.lld-22 ld.lld22 ld.lld-21 ld.lld21 ld.lld-20 ld.lld20 ld.lld-19 ld.lld19 ld.lld-18 ld.lld18 ld.lld
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
        \\      case "$(basename "$lib_path")" in
        \\        libc.so*|libpthread.so*|libm.so*|libdl.so*|librt.so*|ld-linux*.so*|ld-*.so*) continue ;;
        \\      esac
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
    appimage_cmd.addFileArg(appimage_exe.getEmittedBin());
    appimage_cmd.step.dependOn(&appimage_exe.step);

    const appimage_step = b.step("appimage", "Build portable AppImage bundle");
    appimage_step.dependOn(&appimage_cmd.step);

    // generating and linking parser (must be first to generate parser.h)
    const bison_cmd = b.addSystemCommand(&[_][]const u8{ "bison", "-d", "-o", "src/parser/parser.c", "src/parser/parser.y" });

    // generating and linking lexer (depends on parser.h)
    const flex_cmd = b.addSystemCommand(&[_][]const u8{ "flex", "-o", "src/lexer/lexer.c", "src/lexer/lexer.l" });
    flex_cmd.step.dependOn(&bison_cmd.step);
    exe.root_module.addCSourceFile(.{
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
    exe.root_module.addCSourceFile(.{
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
    exe.root_module.link_libc = true;
    addCommonSystemIncludePaths(exe.root_module, detected_llvm);
    addCommonSystemLibraryPaths(exe.root_module, detected_llvm);
    exe.root_module.linkSystemLibrary(llvm_lib, .{});
    exe.step.dependOn(&flex_cmd.step);
    exe.step.dependOn(&bison_cmd.step);

    appimage_exe.root_module.addCSourceFile(.{
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
    appimage_exe.root_module.addCSourceFile(.{
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

    appimage_exe.root_module.link_libc = true;
    addCommonSystemIncludePaths(appimage_exe.root_module, detected_llvm);
    addCommonSystemLibraryPaths(appimage_exe.root_module, detected_llvm);
    appimage_exe.root_module.linkSystemLibrary(llvm_lib, .{});
    appimage_exe.step.dependOn(&flex_cmd.step);
    appimage_exe.step.dependOn(&bison_cmd.step);

    const install_system_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        b.fmt(
            "set -e; install -d \"{0s}\" \"$(dirname \"{1s}\")\"; rm -f \"{0s}/zlang\"; rm -rf \"{0s}/stdlib\"; install -m 755 \"$1\" \"{0s}/zlang\"; cp -a \"stdlib\" \"{0s}/stdlib\"; ln -sfn \"{0s}/zlang\" \"{1s}\"",
            .{ system_prefix, system_symlink },
        ),
        "zlang-install",
    });
    install_system_cmd.addFileArg(exe.getEmittedBin());
    install_system_cmd.step.dependOn(&exe.step);

    b.getInstallStep().dependOn(&install_system_cmd.step);

    const system_install_step = b.step("system-install", "Install zlang and stdlib into system paths");
    system_install_step.dependOn(&install_system_cmd.step);

    const uninstall_system_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        b.fmt(
            "set -e; if [ -e \"{1s}\" ] && [ ! -L \"{1s}\" ]; then echo \"Refusing to remove non-symlink: {1s}\"; exit 1; fi; rm -f \"{1s}\"; rm -rf \"{0s}\"",
            .{ system_prefix, system_symlink },
        ),
    });
    const uninstall_step = b.getUninstallStep();
    uninstall_step.makeFn = makeNoOp;
    uninstall_step.dependOn(&uninstall_system_cmd.step);

    const system_uninstall_step = b.step("system-uninstall", "Remove zlang from system paths");
    system_uninstall_step.dependOn(&uninstall_system_cmd.step);

    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    const unit_test_mod = b.createModule(.{
        .root_source_file = b.path("src/unit_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_test_mod.link_libc = true;
    const unit_tests = b.addTest(.{
        .root_module = unit_test_mod,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
