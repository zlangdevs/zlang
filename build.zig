const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zlang",
        .root_module = exe_mod,
    });

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

    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
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
