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

    const install_exe = b.addInstallArtifact(exe, .{});

    const install_system_cmd = b.addSystemCommand(&[_][]const u8{
        "sh",
        "-c",
        b.fmt(
            "set -e; install -d \"{0s}\"; rm -f \"{0s}/zlang\"; rm -rf \"{0s}/stdlib\"; install -m 755 \"zig-out/bin/zlang\" \"{0s}/zlang\"; cp -a \"stdlib\" \"{0s}/stdlib\"; ln -sfn \"{0s}/zlang\" \"{1s}\"",
            .{ system_prefix, system_symlink },
        ),
    });
    install_system_cmd.step.dependOn(&install_exe.step);

    b.getInstallStep().dependOn(&install_system_cmd.step);

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
