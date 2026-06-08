const std = @import("std");
const llvm_tools = @import("../llvm_tools.zig");
const utils = @import("utils.zig");
const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;
const codegen = @import("llvm.zig");
const CodeGenerator = codegen.CodeGenerator;
const BackendTiming = CodeGenerator.BackendTiming;

fn nanoTimestamp() i128 {
    var ts: std.c.timespec = undefined;
    if (std.c.clock_gettime(.REALTIME, &ts) != 0) return 0;
    return @as(i128, ts.sec) * std.time.ns_per_s + @as(i128, ts.nsec);
}

fn runCommandOk(allocator: std.mem.Allocator, argv: []const []const u8) bool {
    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const result = std.process.run(allocator, threaded.io(), .{ .argv = argv }) catch return false;
    return result.term == .exited and result.term.exited == 0;
}

// Like runCommandOk, but on failure echoes the command and its captured
// stderr/stdout so the underlying linker error is visible (e.g. in CI).
fn runCommandReport(allocator: std.mem.Allocator, argv: []const []const u8) bool {
    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const result = std.process.run(allocator, threaded.io(), .{ .argv = argv }) catch |err| {
        std.debug.print("Link command failed to start: {s}\n", .{@errorName(err)});
        return false;
    };
    if (result.term == .exited and result.term.exited == 0) return true;
    std.debug.print("Link command failed:", .{});
    for (argv) |a| std.debug.print(" {s}", .{a});
    std.debug.print("\n", .{});
    if (result.stderr.len != 0) std.debug.print("{s}\n", .{result.stderr});
    if (result.stdout.len != 0) std.debug.print("{s}\n", .{result.stdout});
    return false;
}

fn detectGccLibDir(allocator: std.mem.Allocator) ?[]const u8 {
    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const result = std.process.run(allocator, threaded.io(), .{ .argv = &[_][]const u8{ "/usr/bin/cc", "-print-libgcc-file-name" } }) catch return null;
    if (result.term != .exited or result.term.exited != 0) return null;
    const libgcc_path = std.mem.trim(u8, result.stdout, " \t\r\n");
    const dir = std.fs.path.dirname(libgcc_path) orelse return null;
    return allocator.dupe(u8, dir) catch null;
}

fn normalizeClangTarget(alloc: std.mem.Allocator, arch: []const u8) ![]const u8 {
    if (arch.len == 0) return "";
    if (std.mem.indexOfScalar(u8, arch, '-') != null) return arch;
    if (std.mem.eql(u8, arch, "x86_64")) return "x86_64-unknown-linux-gnu";
    if (std.mem.eql(u8, arch, "aarch64")) return "aarch64-unknown-linux-gnu";
    if (std.mem.eql(u8, arch, "arm64")) return "aarch64-unknown-linux-gnu";
    return try std.fmt.allocPrint(alloc, "{s}-unknown-linux-gnu", .{arch});
}

fn normalizeLlcTriple(alloc: std.mem.Allocator, clang_target: []const u8) ![]const u8 {
    if (clang_target.len == 0) return "";
    const gnu_idx = std.mem.indexOf(u8, clang_target, "-gnu") orelse return clang_target;
    const after_gnu = gnu_idx + 4;
    if (after_gnu < clang_target.len and clang_target[after_gnu] == '.') {
        return try alloc.dupe(u8, clang_target[0..after_gnu]);
    }
    return clang_target;
}

fn isVersionedGlibcTarget(target: []const u8) bool {
    return std.mem.indexOf(u8, target, "-gnu.") != null;
}

fn toolVersionHintFromPath(tool_path: []const u8) i16 {
    const base = std.fs.path.basename(tool_path);
    var i: usize = base.len;
    while (i > 0 and base[i - 1] >= '0' and base[i - 1] <= '9') : (i -= 1) {}
    if (i == base.len) return 0;
    const digits = base[i..];
    return std.fmt.parseInt(i16, digits, 10) catch 0;
}

fn sameParentDir(a: []const u8, b: []const u8) bool {
    const da = std.fs.path.dirname(a) orelse "";
    const db = std.fs.path.dirname(b) orelse "";
    return std.mem.eql(u8, da, db);
}

pub fn writeToFile(self: *CodeGenerator, filename: []const u8) !void {
    const filename_z = utils.dupeZ(self.allocator, filename);
    defer self.allocator.free(filename_z);

    var error_msg: [*c]u8 = null;
    const result = c.LLVMPrintModuleToFile(self.module, filename_z.ptr, &error_msg);

    if (result != 0) {
        if (error_msg != null) {
            c.LLVMDisposeMessage(error_msg);
        }
        return error.WriteFailed;
    }
}

pub fn writeBitcodeToFile(self: *CodeGenerator, filename: []const u8) !void {
    const filename_z = utils.dupeZ(self.allocator, filename);
    defer self.allocator.free(filename_z);

    const result = c.LLVMWriteBitcodeToFile(self.module, filename_z.ptr);
    if (result != 0) return error.WriteFailed;
}

pub fn verifyModule(self: *CodeGenerator) !void {
    var error_msg: [*c]u8 = null;
    const verify_result = c.LLVMVerifyModule(self.module, c.LLVMReturnStatusAction, &error_msg);
    if (verify_result != 0) {
        if (error_msg != null) {
            defer c.LLVMDisposeMessage(error_msg);
            std.debug.print("IR verification failed:\n{s}\n", .{std.mem.span(error_msg)});
        } else {
            std.debug.print("IR verification failed with unknown LLVM error\n", .{});
        }
        return error.VerificationFailed;
    }
}

pub fn emitLLVMIR(self: *CodeGenerator, base_name: []const u8, optimize: bool) ![]const u8 {
    const ir_file = try std.fmt.allocPrint(self.allocator, "{s}.ll", .{base_name});
    errdefer self.allocator.free(ir_file);

    try writeToFile(self, ir_file);

    if (optimize) {
        var opt_args_list: std.ArrayList([]const u8) = .empty;
        defer opt_args_list.deinit(self.allocator);

        const use_fast_pipeline = shouldUseFastOptimizePipeline(self);

        if (use_fast_pipeline) {
            try opt_args_list.appendSlice(self.allocator, &[_][]const u8{
                "opt",
                "-passes=globaldce,sroa,mem2reg,instcombine,simplifycfg,gvn,dse,instcombine,simplifycfg,globaldce",
                ir_file,
                "-o",
                ir_file,
            });
        } else {
            try opt_args_list.appendSlice(self.allocator, &[_][]const u8{
                "opt",
                "-O3",
                ir_file,
                "-o",
                ir_file,
            });
        }

        if (!runCommandOk(self.allocator, opt_args_list.items)) {
            return error.OptimizationFailed;
        }
    }

    return ir_file;
}

fn shouldUseFastOptimizePipeline(self: *CodeGenerator) bool {
    var defined_functions: usize = 0;
    var instruction_count: usize = 0;

    var func = c.LLVMGetFirstFunction(self.module);
    while (func != null) : (func = c.LLVMGetNextFunction(func)) {
        var bb = c.LLVMGetFirstBasicBlock(func);
        if (bb == null) continue;

        defined_functions += 1;
        while (bb != null) : (bb = c.LLVMGetNextBasicBlock(bb)) {
            var inst = c.LLVMGetFirstInstruction(bb);
            while (inst != null) : (inst = c.LLVMGetNextInstruction(inst)) {
                instruction_count += 1;
            }
        }
    }

    return defined_functions >= 700 or instruction_count >= 90000;
}

fn runLlcForInput(arena_alloc: std.mem.Allocator, llc_tool: []const u8, llc_input_file: []const u8, obj_file: []const u8, llc_triple: []const u8, optimize: bool, max_threads: usize) !void {
    var llc_args_list: std.ArrayList([]const u8) = .empty;
    try llc_args_list.appendSlice(arena_alloc, &[_][]const u8{
        llc_tool,
        "-filetype=obj",
        "-relocation-model=pic",
        llc_input_file,
        "-o",
        obj_file,
    });

    if (optimize) {
        try llc_args_list.append(arena_alloc, "-O3");
    }

    if (max_threads > 0) {
        const llc_threads = try std.fmt.allocPrint(arena_alloc, "--threads={d}", .{max_threads});
        try llc_args_list.append(arena_alloc, llc_threads);
    }

    if (llc_triple.len != 0) {
        const march_flag: []const u8 = try std.fmt.allocPrint(arena_alloc, "-mtriple={s}", .{llc_triple});
        try llc_args_list.append(arena_alloc, march_flag);
    }

    var threaded: std.Io.Threaded = .init(arena_alloc, .{});
    defer threaded.deinit();
    const llc_result = std.process.run(arena_alloc, threaded.io(), .{ .argv = llc_args_list.items }) catch return error.CompilationFailed;
    if (llc_result.term != .exited or llc_result.term.exited != 0) return error.CompilationFailed;
}

pub fn linkObjectsToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, generated_objects: []const []const u8, link_objects: []const []const u8, extra_flags: []const []const u8, max_threads: usize, timing: ?*BackendTiming) !void {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const clang_target = try normalizeClangTarget(arena_alloc, arch);

    var tool_cache = llvm_tools.ToolCache.init(arena_alloc);
    defer tool_cache.deinit();

    const llc_tool = try tool_cache.get(.llc);
    const lld_tool = try tool_cache.get(.ld_lld);
    var clang_tool: ?[]const u8 = null;
    const zig_tool = if (arch.len != 0) try tool_cache.get(.zig) else null;

    if (timing) |t| {
        var llc_major: i16 = 0;
        if (llc_tool) |tool| {
            llc_major = toolVersionHintFromPath(tool);
            if (llc_major == 0) {
                llc_major = @intCast((try llvm_tools.detectToolVersionMajor(arena_alloc, tool)) orelse 0);
            }
            t.llc_version_major = llc_major;
        }
        const opt_tool = try tool_cache.get(.opt);
        if (opt_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.opt_version_major = major;
        }
        if (lld_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.lld_version_major = major;
        }
        const detected_clang_tool = try tool_cache.get(.clang);
        if (detected_clang_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.clang_version_major = major;
        }
    }

    if (arch.len != 0 and zig_tool == null) {
        std.debug.print("Error: -arch requires zig in PATH for sysroot-compatible linking.\n", .{});
        std.debug.print("Please install Zig or remove -arch.\n", .{});
        return error.CompilationFailed;
    }

    var lld_success = false;

    if (isVersionedGlibcTarget(clang_target)) {
        const link_start = nanoTimestamp();
        var zig_cc_args: std.ArrayList([]const u8) = .empty;
        try zig_cc_args.appendSlice(arena_alloc, &[_][]const u8{ zig_tool.?, "cc", "-target", clang_target });
        for (generated_objects) |obj| try zig_cc_args.append(arena_alloc, obj);
        try zig_cc_args.appendSlice(arena_alloc, &[_][]const u8{ "-o", output, "-lc", "-L/usr/lib", "-L/lib", "-L/lib64" });

        for (link_objects) |obj| {
            try zig_cc_args.append(arena_alloc, obj);
        }

        if (self.uses_float_modulo) {
            try zig_cc_args.append(arena_alloc, "-lm");
        }

        for (extra_flags) |ef| {
            try zig_cc_args.append(arena_alloc, ef);
        }

        if (!runCommandOk(arena_alloc, zig_cc_args.items)) {
            std.debug.print("Error: -arch {s} requires 'zig cc' for portable glibc linking, but zig was not found.\n", .{clang_target});
            return error.CompilationFailed;
        } else {
            lld_success = true;
            if (timing) |t| t.link_time_ns = @intCast(nanoTimestamp() - link_start);
            if (timing) |t| t.link_backend = .zig_cc;
        }
    }

    // crt object files live in different directories across distros:
    // /usr/lib on Arch, /usr/lib/x86_64-linux-gnu on Debian/Ubuntu multiarch,
    // /usr/lib64 on some RPM distros. Probe for the directory that actually
    // holds crt1.o so the lld fast path works everywhere, not just Arch.
    const crt_dir: ?[]const u8 = blk: {
        var threaded: std.Io.Threaded = .init(arena_alloc, .{});
        defer threaded.deinit();
        const candidates = [_][]const u8{
            "/usr/lib",
            "/usr/lib/x86_64-linux-gnu",
            "/usr/lib64",
            "/lib/x86_64-linux-gnu",
        };
        for (candidates) |dir| {
            const probe = std.fmt.allocPrint(arena_alloc, "{s}/crt1.o", .{dir}) catch continue;
            std.Io.Dir.cwd().access(threaded.io(), probe, .{}) catch continue;
            break :blk dir;
        }
        break :blk null;
    };

    if (crt_dir != null and arch.len == 0 and lld_tool != null) {
        const cdir = crt_dir.?;
        const link_start = nanoTimestamp();
        var lld_args_list: std.ArrayList([]const u8) = .empty;
        try lld_args_list.appendSlice(arena_alloc, &[_][]const u8{
            lld_tool.?,
            "-o",
            output,
            "-dynamic-linker",
            "/lib64/ld-linux-x86-64.so.2",
            "-L/usr/lib",
            "-L/lib64",
        });
        try lld_args_list.append(arena_alloc, try std.fmt.allocPrint(arena_alloc, "-L{s}", .{cdir}));
        try lld_args_list.append(arena_alloc, try std.fmt.allocPrint(arena_alloc, "{s}/crt1.o", .{cdir}));
        try lld_args_list.append(arena_alloc, try std.fmt.allocPrint(arena_alloc, "{s}/crti.o", .{cdir}));

        if (detectGccLibDir(arena_alloc)) |gcc_lib_dir| {
            const gcc_lib_arg = try std.fmt.allocPrint(arena_alloc, "-L{s}", .{gcc_lib_dir});
            try lld_args_list.append(arena_alloc, gcc_lib_arg);
        }

        for (generated_objects) |obj| try lld_args_list.append(arena_alloc, obj);

        if (max_threads > 0) {
            const lld_threads = try std.fmt.allocPrint(arena_alloc, "--threads={d}", .{max_threads});
            try lld_args_list.append(arena_alloc, lld_threads);
        }

        for (link_objects) |obj| {
            try lld_args_list.append(arena_alloc, obj);
        }

        for (extra_flags) |ef| {
            try lld_args_list.append(arena_alloc, ef);
        }

        try lld_args_list.append(arena_alloc, "-lc");
        try lld_args_list.append(arena_alloc, "-lgcc");

        if (self.uses_float_modulo) {
            try lld_args_list.append(arena_alloc, "-lm");
        }

        try lld_args_list.append(arena_alloc, try std.fmt.allocPrint(arena_alloc, "{s}/crtn.o", .{cdir}));

        if (runCommandOk(arena_alloc, lld_args_list.items)) {
            lld_success = true;
            if (timing) |t| t.link_time_ns = @intCast(nanoTimestamp() - link_start);
            if (timing) |t| t.link_backend = .lld;
        }
    }

    if (!lld_success) {
        const link_start = nanoTimestamp();
        if (clang_tool == null) {
            clang_tool = try tool_cache.get(.clang);
        }
        if (clang_tool == null) {
            std.debug.print("Error: clang not found for linking. Please install clang.\n", .{});
            std.debug.print("Tried: clang-21/clang21, clang-20/clang20, ..., clang (plus common absolute paths and ZLANG_LLVM_BIN)\n", .{});
            return error.CompilationFailed;
        }

        var clang_args_list: std.ArrayList([]const u8) = .empty;
        try clang_args_list.append(arena_alloc, clang_tool.?);
        for (generated_objects) |obj| try clang_args_list.append(arena_alloc, obj);
        try clang_args_list.appendSlice(arena_alloc, &[_][]const u8{ "-o", output, "-lc" });

        if (clang_target.len != 0) {
            const target_arg = try std.fmt.allocPrint(arena_alloc, "--target={s}", .{clang_target});
            try clang_args_list.append(arena_alloc, target_arg);
        }

        for (link_objects) |obj| {
            try clang_args_list.append(arena_alloc, obj);
        }

        if (self.uses_float_modulo) {
            try clang_args_list.append(arena_alloc, "-lm");
        }

        for (extra_flags) |ef| {
            try clang_args_list.append(arena_alloc, ef);
        }

        if (!runCommandReport(arena_alloc, clang_args_list.items)) {
            return error.CompilationFailed;
        }
        if (timing) |t| t.link_time_ns = @intCast(nanoTimestamp() - link_start);
        if (timing) |t| t.link_backend = .clang;
    }
}

pub fn compileIRToObjectFile(self: *CodeGenerator, ir_input_file: []const u8, obj_output_file: []const u8, arch: []const u8, optimize: bool, max_threads: usize, llc_time_ns: ?*u64) !void {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const clang_target = try normalizeClangTarget(arena_alloc, arch);
    const llc_triple = try normalizeLlcTriple(arena_alloc, clang_target);

    var tool_cache = llvm_tools.ToolCache.init(arena_alloc);
    defer tool_cache.deinit();

    const llc_tool = try tool_cache.get(.llc);
    if (llc_tool == null) {
        std.debug.print("Error: llc not found. Please install LLVM tools.\n", .{});
        std.debug.print("Tried: llc-21/llc21, llc-20/llc20, ..., llc (plus common absolute paths and ZLANG_LLVM_BIN)\n", .{});
        return error.CompilationFailed;
    }

    const llc_start = nanoTimestamp();
    try runLlcForInput(arena_alloc, llc_tool.?, ir_input_file, obj_output_file, llc_triple, optimize, max_threads);
    if (llc_time_ns) |t| t.* = @intCast(nanoTimestamp() - llc_start);
}

pub fn splitBitcodeIntoUnits(self: *CodeGenerator, bitcode_file: []const u8, output_prefix: []const u8, unit_count: usize, arch: []const u8, out_units: *std.ArrayList([]const u8)) !bool {
    if (unit_count <= 1) return false;
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tool_cache = llvm_tools.ToolCache.init(arena_alloc);
    defer tool_cache.deinit();

    const split_tool = try tool_cache.get(.llvm_split);
    if (split_tool == null) return false;

    var args: std.ArrayList([]const u8) = .empty;
    try args.appendSlice(arena_alloc, &[_][]const u8{ split_tool.?, "--round-robin", "-j" });
    const jobs = try std.fmt.allocPrint(arena_alloc, "{d}", .{unit_count});
    try args.append(arena_alloc, jobs);

    const clang_target = try normalizeClangTarget(arena_alloc, arch);
    if (clang_target.len != 0) {
        const llc_triple = try normalizeLlcTriple(arena_alloc, clang_target);
        if (llc_triple.len != 0) {
            const triple_arg = try std.fmt.allocPrint(arena_alloc, "--mtriple={s}", .{llc_triple});
            try args.append(arena_alloc, triple_arg);
        }
    }

    try args.appendSlice(arena_alloc, &[_][]const u8{ bitcode_file, "-o", output_prefix });

    var threaded: std.Io.Threaded = .init(arena_alloc, .{});
    defer threaded.deinit();
    if (!runCommandOk(arena_alloc, args.items)) return false;

    var idx: usize = 0;
    while (idx < unit_count) : (idx += 1) {
        const unit_path = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ output_prefix, idx });
        std.Io.Dir.cwd().access(threaded.io(), unit_path, .{}) catch {
            self.allocator.free(unit_path);
            break;
        };
        try out_units.append(self.allocator, unit_path);
    }

    return out_units.items.len > 1;
}

pub fn linkObjectFilesToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, generated_objects: []const []const u8, link_objects: []const []const u8, extra_flags: []const []const u8, max_threads: usize, timing: ?*BackendTiming) !void {
    try linkObjectsToExecutable(self, output, arch, generated_objects, link_objects, extra_flags, max_threads, timing);
}

pub fn compileToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, link_objects: []const []const u8, keep_ll: bool, optimize: bool, extra_flags: []const []const u8, max_threads: usize, timing: ?*BackendTiming) !void {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const ir_ext = if (keep_ll) ".ll" else ".bc";
    const ir_file = try std.fmt.allocPrint(arena_alloc, "{s}{s}", .{ output, ir_ext });
    defer arena_alloc.free(ir_file);
    if (keep_ll) {
        try writeToFile(self, ir_file);
    } else {
        try writeBitcodeToFile(self, ir_file);
    }

    const obj_file = try std.fmt.allocPrint(arena_alloc, "{s}.o", .{output});
    defer arena_alloc.free(obj_file);

    const clang_target = try normalizeClangTarget(arena_alloc, arch);
    const llc_triple = try normalizeLlcTriple(arena_alloc, clang_target);

    var tool_cache = llvm_tools.ToolCache.init(arena_alloc);
    defer tool_cache.deinit();

    const opt_tool = if (optimize) try tool_cache.get(.opt) else null;
    const llc_tool = try tool_cache.get(.llc);
    const lld_tool = try tool_cache.get(.ld_lld);

    if (timing) |t| {
        var llc_major: i16 = 0;
        if (llc_tool) |tool| {
            llc_major = toolVersionHintFromPath(tool);
            if (llc_major == 0) {
                llc_major = @intCast((try llvm_tools.detectToolVersionMajor(arena_alloc, tool)) orelse 0);
            }
            t.llc_version_major = llc_major;
        }
        if (opt_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.opt_version_major = major;
        }
        if (lld_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.lld_version_major = major;
        }
        const detected_clang_tool = try tool_cache.get(.clang);
        if (detected_clang_tool) |tool| {
            var major = toolVersionHintFromPath(tool);
            if (major == 0 and llc_tool != null and llc_major > 0 and sameParentDir(tool, llc_tool.?)) major = llc_major;
            t.clang_version_major = major;
        }
    }

    if (llc_tool == null) {
        std.debug.print("Error: llc not found. Please install LLVM tools.\n", .{});
        std.debug.print("Tried: llc-21/llc21, llc-20/llc20, ..., llc (plus common absolute paths and ZLANG_LLVM_BIN)\n", .{});
        return error.CompilationFailed;
    }

    var llc_input_file = ir_file;
    var optimized_ir_file: ?[]const u8 = null;

    if (optimize) {
        if (opt_tool == null) {
            std.debug.print("Warning: opt not found. Skipping optimization pass.\n", .{});
            std.debug.print("Tried: opt-21/opt21, opt-20/opt20, ..., opt (plus common absolute paths and ZLANG_LLVM_BIN)\n", .{});
        } else {
            const opt_start = nanoTimestamp();
            const opt_ext = if (keep_ll) ".ll" else ".bc";
            const opt_output_file = try std.fmt.allocPrint(arena_alloc, "{s}.opt{s}", .{ output, opt_ext });
            optimized_ir_file = opt_output_file;
            var opt_args_list: std.ArrayList([]const u8) = .empty;

            if (shouldUseFastOptimizePipeline(self)) {
                try opt_args_list.appendSlice(arena_alloc, &[_][]const u8{
                    opt_tool.?,
                    "-passes=globaldce,sroa,mem2reg,instcombine,simplifycfg,gvn,dse,instcombine,simplifycfg,globaldce",
                    ir_file,
                    "-o",
                    opt_output_file,
                });
            } else {
                try opt_args_list.appendSlice(arena_alloc, &[_][]const u8{
                    opt_tool.?,
                    "-O3",
                    ir_file,
                    "-o",
                    opt_output_file,
                });
            }

            if (max_threads > 0) {
                const opt_threads = try std.fmt.allocPrint(arena_alloc, "--threads={d}", .{max_threads});
                try opt_args_list.append(arena_alloc, opt_threads);
            }

            var threaded: std.Io.Threaded = .init(arena_alloc, .{});
            defer threaded.deinit();
            const result = std.process.run(arena_alloc, threaded.io(), .{ .argv = opt_args_list.items }) catch return error.CompilationFailed;
            if (result.term != .exited or result.term.exited != 0) return error.CompilationFailed;
            llc_input_file = opt_output_file;
            if (timing) |t| t.opt_time_ns = @intCast(nanoTimestamp() - opt_start);
        }
    }

    const llc_start = nanoTimestamp();
    try runLlcForInput(arena_alloc, llc_tool.?, llc_input_file, obj_file, llc_triple, optimize, max_threads);
    if (timing) |t| t.llc_time_ns = @intCast(nanoTimestamp() - llc_start);

    const generated_objects = [_][]const u8{obj_file};
    try linkObjectsToExecutable(self, output, arch, &generated_objects, link_objects, extra_flags, max_threads, timing);

    var threaded_cleanup: std.Io.Threaded = .init(arena_alloc, .{});
    defer threaded_cleanup.deinit();
    if (!keep_ll) std.Io.Dir.cwd().deleteFile(threaded_cleanup.io(), ir_file) catch {};
    if (optimized_ir_file) |opt_file| {
        std.Io.Dir.cwd().deleteFile(threaded_cleanup.io(), opt_file) catch {};
    }
    std.Io.Dir.cwd().deleteFile(threaded_cleanup.io(), obj_file) catch {};
}
