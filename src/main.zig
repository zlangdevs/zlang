const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const parser = @import("parser/parser.zig");
const ast = @import("parser/ast.zig");
const semantic = @import("semantic.zig");
const wrapgen = @import("wrapgen.zig");
const codegen = @import("codegen/llvm.zig");
const utils = @import("codegen/utils.zig");
const diagnostics = @import("diagnostics.zig");
const help = @import("help.zig");
const interpreter = @import("interpreter.zig");
const preprocessor = @import("preprocessor/preprocessor.zig");
const llvm_tools = @import("llvm_tools.zig");
const report = @import("stats.zig");
const zlx_commands = @import("zlx/commands.zig");
const zlx_host = @import("zlx/host.zig");
const zlx_abi = @import("zlx/abi.zig");
const zlx_runtime = @import("zlx/runtime.zig");
const zlx_store = @import("zlx/store.zig");
const zlx_preprocess = @import("zlx/preprocess.zig");

const allocator = std.heap.page_allocator;
var process_io: std.Io = undefined;
var plugin_module_paths: ?std.StringHashMap([]const u8) = null;
var plugin_host_ptr: ?*zlx_host.Host = null;
var zlx_source_maps: std.ArrayList(ZlxSourceMap) = .empty;

const ZlxSourceMapEntry = struct {
    generated_offset: usize,
    original_line: u32,
    original_column: u32,
};

const ZlxSourceMap = struct {
    generated_path: []u8,
    original_file: []u8,
    entries: std.ArrayList(ZlxSourceMapEntry),

    fn deinit(self: *ZlxSourceMap, alloc: std.mem.Allocator) void {
        alloc.free(self.generated_path);
        alloc.free(self.original_file);
        self.entries.deinit(alloc);
    }
};

const DiagnosticLocation = struct {
    file_path: []const u8,
    line: usize,
    column: usize,
};

fn nanoTimestamp() i96 {
    return std.Io.Timestamp.now(process_io, .real).toNanoseconds();
}

fn getEnvVarOwned(alloc: std.mem.Allocator, name: [:0]const u8) !?[]u8 {
    const value = std.c.getenv(name) orelse return null;
    return try alloc.dupe(u8, std.mem.span(value));
}

const ModuleInfo = struct {
    name: []const u8,
    path: []const u8,
    line_count: usize,
    ast: *ast.Node,
    dependencies: std.ArrayList([]const u8),
    linker_flags: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator, name: []const u8, path: []const u8, line_count: usize, ast_node: *ast.Node) ModuleInfo {
        return ModuleInfo{
            .name = utils.dupe(u8, alloc, name),
            .path = utils.dupe(u8, alloc, path),
            .line_count = line_count,
            .ast = ast_node,
            .dependencies = .empty,
            .linker_flags = .empty,
        };
    }

    pub fn deinit(self: *ModuleInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        alloc.free(self.path);
        self.dependencies.deinit(alloc);
        self.linker_flags.deinit(alloc);
    }
};

const ModuleRegistration = struct {
    module_name: []const u8,
    module_path: []const u8,
    line_count: usize,
    dependencies: std.ArrayList([]const u8),

    pub fn deinit(self: *ModuleRegistration, alloc: std.mem.Allocator) void {
        alloc.free(self.module_name);
        alloc.free(self.module_path);
        for (self.dependencies.items) |dep| alloc.free(dep);
        self.dependencies.deinit(alloc);
    }
};

const SymbolModuleBinding = struct {
    symbol_name: []const u8,
    module_name: []const u8,

    pub fn deinit(self: *SymbolModuleBinding, alloc: std.mem.Allocator) void {
        alloc.free(self.symbol_name);
        alloc.free(self.module_name);
    }
};

const ParseTiming = struct {
    load_time_ns: u64 = 0,
    std_time_ns: u64 = 0,
    resolve_time_ns: u64 = 0,
    rewrite_time_ns: u64 = 0,
    merge_time_ns: u64 = 0,
};

const BackendObjectTask = struct {
    cg: *codegen.CodeGenerator,
    ir_file: []const u8,
    obj_file: []const u8,
    arch: []const u8,
    optimize: bool,
    max_threads: usize,
    llc_time_ns: u64 = 0,
    err: ?anyerror = null,
};

const BackendUnit = struct {
    ir_file: []const u8,
    obj_file: []const u8,
    keep_ir: bool,
};

const RewriteTask = struct {
    module: *ModuleInfo,
    modules: []const ModuleInfo,
    reachable_modules: *const std.StringHashMap(void),
    alloc: std.mem.Allocator,
    err: ?anyerror = null,
};

const ResolvePrecomputeTask = struct {
    module: *const ModuleInfo,
    module_names: []const []const u8,
    module_name_indices: []const usize,
    allocator: std.mem.Allocator,
    matched_dep_indices: std.ArrayList(usize),
    unmatched_dep_index: ?usize = null,
    err: ?anyerror = null,

    fn init(module: *const ModuleInfo, module_names: []const []const u8, module_name_indices: []const usize, alloc: std.mem.Allocator) ResolvePrecomputeTask {
        return .{
            .module = module,
            .module_names = module_names,
            .module_name_indices = module_name_indices,
            .allocator = alloc,
            .matched_dep_indices = .empty,
        };
    }

    fn deinit(self: *ResolvePrecomputeTask) void {
        self.matched_dep_indices.deinit(self.allocator);
    }
};

const MergeScanTask = struct {
    module: *const ModuleInfo,
    function_count: usize = 0,
    global_count: usize = 0,
};

fn runBackendObjectTask(task: *BackendObjectTask) void {
    task.cg.compileIRToObjectFile(task.ir_file, task.obj_file, task.arch, task.optimize, task.max_threads, &task.llc_time_ns) catch |err| {
        task.err = err;
    };
}

fn compileBackendObjects(tasks: []BackendObjectTask, max_threads: usize, alloc: std.mem.Allocator) !u64 {
    if (tasks.len == 0) return 0;

    if (max_threads <= 1 or tasks.len == 1) {
        for (tasks) |*task| runBackendObjectTask(task);
    } else {
        const worker_count = @min(max_threads, tasks.len);
        var next_task_idx: usize = 0;
        while (next_task_idx < tasks.len) {
            const batch_count = @min(worker_count, tasks.len - next_task_idx);
            var threads = try alloc.alloc(std.Thread, batch_count);
            defer alloc.free(threads);

            for (0..batch_count) |j| {
                threads[j] = try std.Thread.spawn(.{}, runBackendObjectTask, .{&tasks[next_task_idx + j]});
            }
            for (threads) |thread| thread.join();
            next_task_idx += batch_count;
        }
    }

    var llc_total_ns: u64 = 0;
    for (tasks) |task| {
        if (task.err) |err| return err;
        llc_total_ns += task.llc_time_ns;
    }
    return llc_total_ns;
}

fn cleanupBackendUnits(units: []const BackendUnit) void {
    for (units) |unit| {
        std.Io.Dir.cwd().deleteFile(process_io, unit.obj_file) catch {};
        if (!unit.keep_ir) std.Io.Dir.cwd().deleteFile(process_io, unit.ir_file) catch {};
    }
}

fn findClangTool(alloc: std.mem.Allocator) ?[]const u8 {
    return llvm_tools.getLLVMToolPath(alloc, .clang) catch null;
}

fn compileLLViaClang(alloc: std.mem.Allocator, clang_tool: []const u8, ir_path: []const u8, obj_path: []const u8, optimize: bool) !void {
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(alloc);
    try args.append(alloc, clang_tool);
    try args.append(alloc, "-x");
    try args.append(alloc, "ir");
    try args.append(alloc, "-c");
    if (optimize) try args.append(alloc, "-O3");
    try args.append(alloc, "-fPIC");
    try args.append(alloc, ir_path);
    try args.append(alloc, "-o");
    try args.append(alloc, obj_path);
    var threaded: std.Io.Threaded = .init(alloc, .{});
    defer threaded.deinit();
    const r = std.process.run(alloc, threaded.io(), .{ .argv = args.items }) catch return error.CompilationFailed;
    if (r.term != .exited or r.term.exited != 0) {
        std.debug.print("clang -x ir failed: {s}\n", .{r.stderr});
        return error.CompilationFailed;
    }
}

fn compilePrebuiltLLVMIR(
    alloc: std.mem.Allocator,
    ctx: *const Context,
    ir_paths: []const []const u8,
) !u8 {
    var code_generator = try codegen.CodeGenerator.init(alloc);
    defer code_generator.deinit();

    var obj_files: std.ArrayList([]const u8) = .empty;
    defer {
        for (obj_files.items) |o| {
            std.Io.Dir.cwd().deleteFile(process_io, o) catch {};
            alloc.free(o);
        }
        obj_files.deinit(alloc);
    }

    const clang_tool_opt = findClangTool(alloc);
    const nonce: u64 = @intCast(nanoTimestamp());
    for (ir_paths, 0..) |ir, idx| {
        const obj = try std.fmt.allocPrint(alloc, "{s}.prebuilt.{d}.{d}.o", .{ ctx.output, nonce, idx });
        try obj_files.append(alloc, obj);
        if (clang_tool_opt) |clang_tool| {
            try compileLLViaClang(alloc, clang_tool, ir, obj, ctx.optimize);
        } else {
            var llc_ns: u64 = 0;
            try code_generator.compileIRToObjectFile(ir, obj, ctx.arch, ctx.optimize, ctx.max_threads, &llc_ns);
        }
    }

    var timing = codegen.CodeGenerator.BackendTiming{};
    try code_generator.linkObjectFilesToExecutable(
        ctx.output,
        ctx.arch,
        obj_files.items,
        ctx.link_objects.items,
        ctx.extra_args.items,
        ctx.max_threads,
        &timing,
    );

    if (ctx.verbose and !ctx.quiet) {
        std.debug.print("Executable compiled to {s} (pre-built LLVM IR fast path)\n", .{ctx.output});
    }
    return 0;
}

fn backendSplitUnitCount(max_threads: usize) usize {
    if (max_threads <= 1) return 1;
    return max_threads;
}

fn runRewriteTask(task: *RewriteTask) void {
    var use_specs: std.ArrayList(UseSpec) = .empty;
    defer use_specs.deinit(task.alloc);
    collectUseSpecs(task.module, &use_specs, task.alloc) catch |err| {
        task.err = err;
        return;
    };

    var rewrite_ctx = RewriteContext.init(task.module, use_specs.items, task.modules, task.reachable_modules, task.alloc);
    defer rewrite_ctx.deinit();
    rewriteModuleImportsInNode(&rewrite_ctx, task.module.ast);
}

fn rewriteReachableModulesParallel(modules: []ModuleInfo, reachable_modules: *const std.StringHashMap(void), max_threads: usize, alloc: std.mem.Allocator) !void {
    var rewrite_count: usize = 0;
    for (modules) |module| {
        if (reachable_modules.contains(module.name)) rewrite_count += 1;
    }
    if (rewrite_count == 0) return;

    var tasks = try alloc.alloc(RewriteTask, rewrite_count);
    defer alloc.free(tasks);

    var idx: usize = 0;
    for (modules) |*module| {
        if (!reachable_modules.contains(module.name)) continue;
        tasks[idx] = .{
            .module = module,
            .modules = modules,
            .reachable_modules = reachable_modules,
            .alloc = alloc,
        };
        idx += 1;
    }

    if (max_threads <= 1 or tasks.len == 1) {
        for (tasks) |*task| runRewriteTask(task);
    } else {
        const worker_count = @min(max_threads, tasks.len);
        var next_task_idx: usize = 0;
        while (next_task_idx < tasks.len) {
            const batch_count = @min(worker_count, tasks.len - next_task_idx);
            var threads = try alloc.alloc(std.Thread, batch_count);
            defer alloc.free(threads);

            for (0..batch_count) |j| {
                threads[j] = try std.Thread.spawn(.{}, runRewriteTask, .{&tasks[next_task_idx + j]});
            }
            for (threads) |thread| thread.join();
            next_task_idx += batch_count;
        }
    }

    for (tasks) |task| {
        if (task.err) |err| return err;
    }
}

fn runResolvePrecomputeTask(task: *ResolvePrecomputeTask) void {
    for (task.module.dependencies.items, 0..) |dep, dep_idx| {
        var matched_any = false;
        for (task.module_names, 0..) |candidate, candidate_idx| {
            if (!moduleMatchesImport(dep, candidate)) continue;
            matched_any = true;
            task.matched_dep_indices.append(task.allocator, task.module_name_indices[candidate_idx]) catch |err| {
                task.err = err;
                return;
            };
        }
        if (!matched_any) {
            task.unmatched_dep_index = dep_idx;
            return;
        }
    }
}

fn runMergeScanTask(task: *MergeScanTask) void {
    switch (task.module.ast.data) {
        .program => |prog| {
            for (prog.functions.items) |func| {
                if (func.data != .use_stmt) task.function_count += 1;
            }
            task.global_count = prog.globals.items.len;
        },
        else => {
            task.function_count = 1;
            task.global_count = 0;
        },
    }
}

fn runTasksParallel(comptime T: type, tasks: []T, max_threads: usize, alloc: std.mem.Allocator, comptime runner: fn (*T) void) !void {
    if (tasks.len == 0) return;
    if (max_threads <= 1 or tasks.len == 1) {
        for (tasks) |*task| runner(task);
        return;
    }

    const worker_count = @min(max_threads, tasks.len);
    var next_task_idx: usize = 0;
    while (next_task_idx < tasks.len) {
        const batch_count = @min(worker_count, tasks.len - next_task_idx);
        var threads = try alloc.alloc(std.Thread, batch_count);
        defer alloc.free(threads);

        for (0..batch_count) |j| {
            threads[j] = try std.Thread.spawn(.{}, runner, .{&tasks[next_task_idx + j]});
        }
        for (threads) |thread| thread.join();
        next_task_idx += batch_count;
    }
}

pub const Context = struct {
    input_files: std.ArrayList([]const u8),
    link_objects: std.ArrayList([]const u8),
    extra_args: std.ArrayList([]const u8),
    program_args: std.ArrayList([]const u8),
    output: []const u8,
    arch: []const u8,
    max_threads: usize,
    keepll: bool,
    show_ast: bool,
    optimize: bool,
    verify_ir: bool,
    verbose: bool,
    quiet: bool,
    stats: bool,
    run_mode: bool,
    define_overrides: std.ArrayList(preprocessor.DefineOverride),
    seen_define_names: std.ArrayList([]const u8),
    module_registrations: std.ArrayList(ModuleRegistration),
    function_module_bindings: std.ArrayList(SymbolModuleBinding),
    global_module_bindings: std.ArrayList(SymbolModuleBinding),
    plugin_flags: std.ArrayList([]const u8),
    no_extensions: bool = false,
    isolated: bool = false,

    pub fn init() Context {
        return Context{
            .input_files = .empty,
            .link_objects = .empty,
            .extra_args = .empty,
            .program_args = .empty,
            .output = consts.DEFAULT_OUTPUT_NAME,
            .arch = "",
            .max_threads = defaultMaxThreads(),
            .keepll = false,
            .show_ast = false,
            .optimize = false,
            .verify_ir = false,
            .verbose = false,
            .quiet = false,
            .stats = false,
            .run_mode = false,
            .define_overrides = .empty,
            .seen_define_names = .empty,
            .module_registrations = .empty,
            .function_module_bindings = .empty,
            .global_module_bindings = .empty,
            .plugin_flags = .empty,
        };
    }

    pub fn deinit(self: *Context, alloc: std.mem.Allocator) void {
        self.input_files.deinit(alloc);
        self.link_objects.deinit(alloc);
        self.extra_args.deinit(alloc);
        self.program_args.deinit(alloc);
        for (self.define_overrides.items) |entry| {
            alloc.free(entry.name);
            alloc.free(entry.value);
        }
        self.define_overrides.deinit(alloc);
        for (self.seen_define_names.items) |name| {
            alloc.free(name);
        }
        self.seen_define_names.deinit(alloc);
        for (self.module_registrations.items) |*entry| entry.deinit(alloc);
        self.module_registrations.deinit(alloc);
        for (self.function_module_bindings.items) |*entry| entry.deinit(alloc);
        self.function_module_bindings.deinit(alloc);
        for (self.global_module_bindings.items) |*entry| entry.deinit(alloc);
        self.global_module_bindings.deinit(alloc);
        for (self.plugin_flags.items) |flag| alloc.free(flag);
        self.plugin_flags.deinit(alloc);
    }

    pub fn print(self: *const Context) void {
        if (self.quiet) return;
        std.debug.print("========Compilation context=======\n", .{});
        std.debug.print("Input files: ", .{});
        for (self.input_files.items, 0..) |file, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{file});
        }
        std.debug.print("\n", .{});
        std.debug.print("Output path: {s}\n", .{self.output});
        std.debug.print("Architecture: {s}\n", .{self.arch});
        std.debug.print("Max threads: {d}\n", .{self.max_threads});
        std.debug.print("Keep ll: {s}\n", .{if (self.keepll) "yes" else "no"});
        std.debug.print("Optimize: {s}\n", .{if (self.optimize) "yes" else "no"});
        std.debug.print("Link objects: ", .{});
        for (self.link_objects.items, 0..) |obj, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{obj});
        }
        std.debug.print("\n", .{});
        std.debug.print("==================================\n", .{});
    }
};

fn defaultMaxThreads() usize {
    return std.Thread.getCpuCount() catch 1;
}

fn collectUseStatements(node: *ast.Node, dependencies: *std.ArrayList([]const u8)) void {
    switch (node.data) {
        .program => |prog| {
            for (prog.functions.items) |func| {
                collectUseStatements(func, dependencies);
            }
            for (prog.globals.items) |glob| {
                collectUseStatements(glob, dependencies);
            }
        },
        .function => |func| {
            if (func.guard) |guard| {
                collectUseStatements(guard, dependencies);
            }
            for (func.body.items) |stmt| {
                collectUseStatements(stmt, dependencies);
            }
        },
        .use_stmt => |use_stmt| {
            dependencies.append(allocator, use_stmt.module_path) catch {};
        },
        else => {
            switch (node.data) {
                .assignment => |as| collectUseStatements(as.value, dependencies),
                .var_decl => |decl| if (decl.initializer) |init| collectUseStatements(init, dependencies),
                .function_call => |call| for (call.args.items) |arg| collectUseStatements(arg, dependencies),
                .method_call => |method| {
                    collectUseStatements(method.object, dependencies);
                    for (method.args.items) |arg| collectUseStatements(arg, dependencies);
                },
                .return_stmt => |ret| if (ret.expression) |expr| collectUseStatements(expr, dependencies),
                .if_stmt => |if_stmt| {
                    collectUseStatements(if_stmt.condition, dependencies);
                    for (if_stmt.then_body.items) |stmt| collectUseStatements(stmt, dependencies);
                    if (if_stmt.else_body) |else_body| {
                        for (else_body.items) |stmt| collectUseStatements(stmt, dependencies);
                    }
                },
                .for_stmt => |for_stmt| {
                    if (for_stmt.condition) |cond| collectUseStatements(cond, dependencies);
                    for (for_stmt.body.items) |stmt| collectUseStatements(stmt, dependencies);
                },
                .c_for_stmt => |c_for| {
                    if (c_for.init) |init| collectUseStatements(init, dependencies);
                    if (c_for.condition) |cond| collectUseStatements(cond, dependencies);
                    if (c_for.increment) |inc| collectUseStatements(inc, dependencies);
                    for (c_for.body.items) |stmt| collectUseStatements(stmt, dependencies);
                },
                .array_initializer => |arr_init| for (arr_init.elements.items) |elem| collectUseStatements(elem, dependencies),
                .array_index => |arr_idx| collectUseStatements(arr_idx.index, dependencies),
                .array_assignment => |arr_ass| {
                    collectUseStatements(arr_ass.index, dependencies);
                    collectUseStatements(arr_ass.value, dependencies);
                },
                .comparison => |comp| {
                    collectUseStatements(comp.lhs, dependencies);
                    collectUseStatements(comp.rhs, dependencies);
                },
                .binary_op => |bop| {
                    collectUseStatements(bop.lhs, dependencies);
                    collectUseStatements(bop.rhs, dependencies);
                },
                .unary_op => |un| collectUseStatements(un.operand, dependencies),
                .expression_block => |block| {
                    for (block.statements.items) |stmt| collectUseStatements(stmt, dependencies);
                    collectUseStatements(block.result, dependencies);
                },
                .handled_call_stmt => |handled| {
                    collectUseStatements(handled.call, dependencies);
                    for (handled.handlers.items) |handler| {
                        for (handler.body.items) |stmt| collectUseStatements(stmt, dependencies);
                    }
                },
                else => {},
            }
        },
    }
}

fn getStdlibPath(alloc: std.mem.Allocator) ![]const u8 {
    if ((try getEnvVarOwned(alloc, "ZSTDPATH"))) |zstdpath| {
        return zstdpath;
    } else {
        const exe_path = try std.process.executablePathAlloc(process_io, alloc);
        defer alloc.free(exe_path);
        const exe_dir = std.fs.path.dirname(exe_path) orelse ".";
        return std.fs.path.join(alloc, &[_][]const u8{ exe_dir, "stdlib" });
    }
}

fn resolveStdModule(module_name: []const u8, alloc: std.mem.Allocator, has_zstdpath: *bool) ?[]const u8 {
    if (!std.mem.startsWith(u8, module_name, "std.")) {
        return null;
    }
    const module_part = module_name[4..];
    has_zstdpath.* = if ((getEnvVarOwned(alloc, "ZSTDPATH") catch null)) |path| blk: {
        alloc.free(path);
        break :blk true;
    } else false;

    const stdlib_path = getStdlibPath(alloc) catch return null;
    defer alloc.free(stdlib_path);

    const module_file = std.fmt.allocPrint(alloc, "{s}.zl", .{module_part}) catch return null;
    defer alloc.free(module_file);

    const full_path = std.fs.path.join(alloc, &[_][]const u8{ stdlib_path, module_file }) catch return null;

    std.Io.Dir.cwd().access(process_io, full_path, .{}) catch {
        alloc.free(full_path);
        return null;
    };

    return full_path;
}

fn resolveModulePath(base_path: []const u8, module_name: []const u8, alloc: std.mem.Allocator) anyerror!?[]const u8 {
    if (std.mem.startsWith(u8, module_name, "std.")) {
        var has_zstdpath: bool = false;
        if (resolveStdModule(module_name, alloc, &has_zstdpath)) |std_path| {
            return std_path;
        }

        std.debug.print("\x1b[31mError:\x1b[0m Standard library module '\x1b[33m{s}\x1b[0m' not found.\n", .{module_name});

        if (!has_zstdpath) {
            std.debug.print("\x1b[33mHint:\x1b[0m The ZSTDPATH environment variable is not set.\n", .{});
            std.debug.print("  Standard library modules are searched in the following locations:\n", .{});
            std.debug.print("    1. The directory specified by ZSTDPATH (if set)\n", .{});
            std.debug.print("    2. The stdlib/ directory next to the compiler binary\n", .{});
            std.debug.print("\n", .{});
            std.debug.print("  To fix this, set ZSTDPATH to point to your stdlib directory:\n", .{});
            std.debug.print("    \x1b[36mexport ZSTDPATH=/path/to/zlang/stdlib\x1b[0m\n", .{});
        } else {
            std.debug.print("  Searched in ZSTDPATH or stdlib/ directory.\n", .{});
            const stdlib_path = getStdlibPath(alloc) catch {
                std.debug.print("  Could not determine stdlib path.\n", .{});
                return null;
            };
            defer alloc.free(stdlib_path);
            std.debug.print("  Searched in: \x1b[36m{s}\x1b[0m\n", .{stdlib_path});
        }
        return null;
    }

    if (std.mem.endsWith(u8, module_name, ".zl")) {
        return utils.dupe(u8, alloc, module_name);
    }
    const base_dir = std.fs.path.dirname(base_path) orelse ".";
    const module_file = try std.fmt.allocPrint(alloc, "{s}.zl", .{module_name});
    defer alloc.free(module_file);
    const full_path = try std.fs.path.join(alloc, &[_][]const u8{ base_dir, module_file });
    defer alloc.free(full_path);
    if (std.Io.Dir.cwd().access(process_io, full_path, .{})) {
        return utils.dupe(u8, alloc, full_path);
    } else |_| {}

    if (plugin_module_paths) |*map| {
        if (map.get(module_name)) |abs_path| {
            return utils.dupe(u8, alloc, abs_path);
        }
    }
    return null;
}

fn tryFindMoreErrors(alloc: std.mem.Allocator, file_path: []const u8, input: []const u8) void {
    var lines = std.mem.splitScalar(u8, input, '\n');
    var line_num: usize = 1;
    var brace_depth: i32 = 0;
    var in_function = false;
    while (lines.next()) |line| : (line_num += 1) {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        if (std.mem.startsWith(u8, trimmed, "//")) continue;
        for (trimmed) |ch| {
            if (ch == '{') brace_depth += 1;
            if (ch == '}') brace_depth -= 1;
        }
        if (std.mem.indexOf(u8, trimmed, "fun ") != null) {
            in_function = true;
        }
        if (in_function and !std.mem.endsWith(u8, trimmed, ";") and
            !std.mem.endsWith(u8, trimmed, "{") and
            !std.mem.endsWith(u8, trimmed, "}") and
            trimmed.len > 0 and trimmed[0] != '}')
        {
            var has_control_keyword = false;
            const control_keywords = [_][]const u8{ "if", "for", "while", "return", "break", "continue" };
            for (control_keywords) |kw| {
                if (std.mem.indexOf(u8, trimmed, kw) != null) {
                    has_control_keyword = true;
                    break;
                }
            }
            if (!has_control_keyword and std.mem.indexOf(u8, trimmed, "=") != null) {
                diagnostics.printDiagnostic(alloc, .{
                    .file_path = file_path,
                    .line = line_num,
                    .column = trimmed.len,
                    .message = "missing semicolon at end of statement",
                    .severity = .Error,
                    .hint = "add ';' at the end",
                });
            }
        }
        if (brace_depth == 0 and in_function) {
            in_function = false;
        }
    }
    if (brace_depth > 0) {
        diagnostics.printDiagnostic(alloc, .{
            .file_path = file_path,
            .line = line_num - 1,
            .column = 0,
            .message = "unclosed braces in file",
            .severity = .Error,
            .hint = "missing closing brace(s)",
        });
    }
}

fn parseErrorHint(message: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, message, "expected ';'") != null) return "add ';' before this token";
    if (std.mem.indexOf(u8, message, "expected ')'") != null) return "close the expression with ')'";
    if (std.mem.indexOf(u8, message, "expected '}'") != null) return "close the current block with '}'";
    if (std.mem.indexOf(u8, message, "expected ']'") != null) return "close the index or type list with ']'";
    if (std.mem.indexOf(u8, message, "expected '>'") != null) return "close the generic type with '>'";
    if (std.mem.indexOf(u8, message, "unexpected 'else'") != null) return "check braces before else and the matching if block";
    if (std.mem.indexOf(u8, message, "unexpected end of") != null) return "file ended early; check missing braces or parentheses";
    return null;
}

fn printPluginDiagnostics(alloc: std.mem.Allocator, host: *const zlx_host.Host) void {
    for (host.diagnostics.items) |diag| {
        const severity: diagnostics.Severity = switch (diag.level) {
            .err => .Error,
            .warning => .Warning,
            .note => .Note,
        };
        if (diag.file) |file| {
            diagnostics.printDiagnostic(alloc, .{
                .file_path = file,
                .line = if (diag.line == 0) 1 else diag.line,
                .column = if (diag.column == 0) 1 else diag.column,
                .message = diag.message,
                .hint = diag.hint,
                .severity = severity,
            });
        } else {
            const label = switch (severity) {
                .Error => "error",
                .Warning => "warning",
                .Note => "note",
            };
            std.debug.print("zlx plugin {s}: {s}\n", .{ label, diag.message });
            if (diag.hint) |hint| std.debug.print("  hint: {s}\n", .{hint});
        }
    }
}

fn clearZlxSourceMaps(alloc: std.mem.Allocator) void {
    for (zlx_source_maps.items) |*map| map.deinit(alloc);
    zlx_source_maps.clearRetainingCapacity();
}

fn sourceOffsetAtLineColumn(source: []const u8, line: usize, column: usize) usize {
    var current_line: usize = 1;
    var current_column: usize = 1;
    for (source, 0..) |ch, idx| {
        if (current_line == line and current_column == column) return idx;
        if (ch == '\n') {
            current_line += 1;
            current_column = 1;
        } else {
            current_column += 1;
        }
    }
    return source.len;
}

fn remapZlxDiagnosticLocation(file_path: []const u8, line: usize, column: usize, parser_text: []const u8) DiagnosticLocation {
    const generated_offset = sourceOffsetAtLineColumn(parser_text, line, column);
    for (zlx_source_maps.items) |map| {
        if (!std.mem.eql(u8, map.generated_path, file_path)) continue;
        var best: ?ZlxSourceMapEntry = null;
        for (map.entries.items) |entry| {
            if (entry.generated_offset > generated_offset) continue;
            if (best == null or entry.generated_offset >= best.?.generated_offset) best = entry;
        }
        if (best) |entry| {
            return .{
                .file_path = map.original_file,
                .line = entry.original_line,
                .column = entry.original_column,
            };
        }
    }
    return .{ .file_path = file_path, .line = line, .column = column };
}

fn countLinesInText(text: []const u8) usize {
    if (text.len == 0) return 0;
    var count: usize = 1;
    for (text) |ch| {
        if (ch == '\n') count += 1;
    }
    return count;
}

const PreparsedModule = struct {
    module_name: []const u8,
    parser_text: []const u8,
    line_count: usize,
    flags: std.ArrayList([]const u8),
    defined_names: std.ArrayList([]const u8),

    fn deinit(self: *PreparsedModule, alloc: std.mem.Allocator) void {
        alloc.free(self.module_name);
        alloc.free(self.parser_text);
        for (self.flags.items) |flag| alloc.free(flag);
        self.flags.deinit(alloc);
        for (self.defined_names.items) |name| alloc.free(name);
        self.defined_names.deinit(alloc);
    }
};

const InitialLoadTask = struct {
    file_path: []const u8,
    ctx: *Context,
    alloc: std.mem.Allocator,
    result: ?PreparsedModule = null,
    err: ?anyerror = null,
};

fn preprocessModuleFile(file_path: []const u8, alloc: std.mem.Allocator, ctx: *Context) !PreparsedModule {
    const input = read_file(file_path) catch |err| {
        std.debug.print("Error reading file {s}: {}\n", .{ file_path, err });
        return err;
    };
    defer allocator.free(input);

    var preprocessed = preprocessor.preprocessWithFlagsAndDefines(alloc, input, ctx.define_overrides.items) catch |err| {
        const msg = switch (err) {
            errors.PreprocessError.InvalidDirective => "Invalid preprocessor directive",
            errors.PreprocessError.InvalidDefine => "Invalid #define",
            errors.PreprocessError.ExpansionLimit => "Macro expansion limit reached",
            errors.PreprocessError.OutOfMemory => "Out of memory during preprocessing",
        };
        diagnostics.printDiagnostic(alloc, .{
            .file_path = file_path,
            .line = 1,
            .column = 1,
            .message = msg,
            .severity = .Error,
        });
        return err;
    };
    defer preprocessed.deinitFlags(alloc);

    const parsed_header = parseModuleHeader(alloc, preprocessed.text) catch |err| {
        if (err == error.InvalidModuleHeader) {
            const header_start = findModuleHeaderStart(preprocessed.text);
            const header_pos = sourcePosAtOffset(preprocessed.text, header_start);
            diagnostics.printDiagnostic(alloc, .{
                .file_path = file_path,
                .line = header_pos.line,
                .column = header_pos.column,
                .message = "Invalid module header. Expected: module a.b.c;",
                .severity = .Error,
                .hint = "Use a dotted module path and terminate it with ';'",
            });
        }
        return err;
    };

    const module_name = if (parsed_header.module_name) |name|
        name
    else
        try alloc.dupe(u8, defaultModuleNameFromPath(file_path));
    const line_count = countLinesInText(input);

    var flags: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (flags.items) |flag| alloc.free(flag);
        flags.deinit(alloc);
    }
    for (preprocessed.flags.items) |flag| {
        try flags.append(alloc, utils.dupe(u8, alloc, flag));
    }

    var defined_names: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (defined_names.items) |name| alloc.free(name);
        defined_names.deinit(alloc);
    }
    for (preprocessed.defined_names.items) |name| {
        try defined_names.append(alloc, utils.dupe(u8, alloc, name));
    }

    return .{
        .module_name = module_name,
        .parser_text = parsed_header.text_for_parser,
        .line_count = line_count,
        .flags = flags,
        .defined_names = defined_names,
    };
}

fn parseModuleFromPrepared(file_path: []const u8, arena: std.mem.Allocator, backing_alloc: std.mem.Allocator, ctx: *Context, prep: *PreparsedModule) !ModuleInfo {
    for (prep.defined_names.items) |name| {
        if (!containsString(ctx.seen_define_names.items, name)) {
            try ctx.seen_define_names.append(backing_alloc, utils.dupe(u8, backing_alloc, name));
        }
    }

    const ast_root = parser.parse(arena, prep.parser_text) catch |err| {
        const parse_errors = parser.getParseErrors();
        if (parse_errors.len > 0) {
            for (parse_errors) |parse_err| {
                const remapped = remapZlxDiagnosticLocation(file_path, parse_err.line, if (parse_err.column == 0) 1 else parse_err.column, prep.parser_text);
                diagnostics.printDiagnostic(backing_alloc, .{
                    .file_path = remapped.file_path,
                    .line = remapped.line,
                    .column = remapped.column,
                    .message = parse_err.message,
                    .severity = .Error,
                    .hint = parseErrorHint(parse_err.message),
                });
            }
        } else {
            const loc = parser.lastParseErrorLocation();
            if (loc) |l| {
                const remapped = remapZlxDiagnosticLocation(file_path, l.line, l.column, prep.parser_text);
                diagnostics.printDiagnostic(backing_alloc, .{
                    .file_path = remapped.file_path,
                    .line = remapped.line,
                    .column = remapped.column,
                    .message = "Parse error",
                    .severity = .Error,
                });
            } else {
                std.debug.print("Error parsing file {s}: {}\n", .{ file_path, err });
            }
        }

        tryFindMoreErrors(backing_alloc, file_path, prep.parser_text);
        return err;
    };
    if (ast_root) |root| {
        var module = ModuleInfo.init(backing_alloc, prep.module_name, file_path, prep.line_count, root);
        collectUseStatements(root, &module.dependencies);
        for (prep.flags.items) |flag| {
            try module.linker_flags.append(backing_alloc, utils.dupe(u8, backing_alloc, flag));
        }
        return module;
    }
    return error.ParseFailed;
}

fn parseModuleFile(file_path: []const u8, arena: std.mem.Allocator, backing_alloc: std.mem.Allocator, ctx: *Context) !ModuleInfo {
    var prep = try preprocessModuleFile(file_path, backing_alloc, ctx);
    defer prep.deinit(backing_alloc);
    return parseModuleFromPrepared(file_path, arena, backing_alloc, ctx, &prep);
}

fn runInitialLoadTask(task: *InitialLoadTask) void {
    task.result = preprocessModuleFile(task.file_path, task.alloc, task.ctx) catch |err| {
        task.err = err;
        return;
    };
}

fn containsString(list: []const []const u8, value: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, value)) return true;
    }
    return false;
}

fn emitUnknownDefineOverrideWarnings(ctx: *const Context) void {
    for (ctx.define_overrides.items) |entry| {
        if (!containsString(ctx.seen_define_names.items, entry.name)) {
            std.debug.print("Warning: -D{s}=... was provided but #define {s} was not found in parsed .zl files\n", .{ entry.name, entry.name });
        }
    }
}

fn resolvePathRelativeToModule(alloc: std.mem.Allocator, module_path: []const u8, raw_path: []const u8) ![]const u8 {
    if (std.fs.path.isAbsolute(raw_path)) {
        return utils.dupe(u8, alloc, raw_path);
    }

    const base_dir = std.fs.path.dirname(module_path) orelse ".";
    const joined = try std.fs.path.join(alloc, &[_][]const u8{ base_dir, raw_path });
    defer alloc.free(joined);
    return utils.dupe(u8, alloc, joined);
}

const ParsedModuleHeader = struct {
    module_name: ?[]const u8,
    text_for_parser: []const u8,
};

const SourcePos = struct {
    line: usize,
    column: usize,
};

fn sourcePosAtOffset(input: []const u8, offset_raw: usize) SourcePos {
    const offset = @min(offset_raw, input.len);
    var line: usize = 1;
    var column: usize = 1;
    var i: usize = 0;
    while (i < offset) : (i += 1) {
        if (input[i] == '\n') {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    return .{ .line = line, .column = column };
}

fn findModuleHeaderStart(input: []const u8) usize {
    var i: usize = 0;
    while (i < input.len and isSpace(input[i])) : (i += 1) {}
    return i;
}

fn isModulePathChar(c: u8) bool {
    return isIdentChar(c) or c == '.';
}

fn defaultModuleNameFromPath(file_path: []const u8) []const u8 {
    const base = std.fs.path.basename(file_path);
    if (std.mem.endsWith(u8, base, ".zl")) {
        return base[0 .. base.len - 3];
    }
    return base;
}

fn parseModuleHeader(alloc: std.mem.Allocator, input: []const u8) !ParsedModuleHeader {
    var i: usize = 0;
    while (i < input.len and isSpace(input[i])) : (i += 1) {}

    if (i + 6 > input.len or !std.mem.eql(u8, input[i .. i + 6], "module")) {
        return .{
            .module_name = null,
            .text_for_parser = utils.dupe(u8, alloc, input),
        };
    }
    if (i + 6 < input.len and isIdentChar(input[i + 6])) {
        return .{
            .module_name = null,
            .text_for_parser = utils.dupe(u8, alloc, input),
        };
    }

    var p = i + 6;
    while (p < input.len and isSpace(input[p])) : (p += 1) {}
    const name_start = p;
    while (p < input.len and isModulePathChar(input[p])) : (p += 1) {}
    const name_end = p;

    if (name_start == name_end) return error.InvalidModuleHeader;
    const module_name = trimSpaces(input[name_start..name_end]);
    if (module_name.len == 0) return error.InvalidModuleHeader;

    while (p < input.len and isSpace(input[p])) : (p += 1) {}
    if (p >= input.len or input[p] != ';') return error.InvalidModuleHeader;
    const header_end = p + 1;

    var stripped = utils.dupe(u8, alloc, input);
    var s: usize = i;
    while (s < header_end) : (s += 1) {
        if (stripped[s] != '\n') stripped[s] = ' ';
    }

    return .{
        .module_name = utils.dupe(u8, alloc, module_name),
        .text_for_parser = stripped,
    };
}

fn appendModuleFlagsToContext(ctx: *Context, module: *const ModuleInfo, alloc: std.mem.Allocator) !void {
    var i: usize = 0;
    while (i < module.linker_flags.items.len) {
        const flag = module.linker_flags.items[i];

        if (std.mem.eql(u8, flag, "-link")) {
            if (i + 1 < module.linker_flags.items.len) {
                const raw_obj = module.linker_flags.items[i + 1];
                const resolved_obj = try resolvePathRelativeToModule(alloc, module.path, raw_obj);
                if (!containsString(ctx.link_objects.items, resolved_obj)) {
                    try ctx.link_objects.append(alloc, resolved_obj);
                } else {
                    alloc.free(resolved_obj);
                }
                i += 2;
                continue;
            }
            i += 1;
            continue;
        }

        if (std.mem.eql(u8, flag, "-L")) {
            if (i + 1 < module.linker_flags.items.len) {
                const raw_path = module.linker_flags.items[i + 1];
                const resolved_path = try resolvePathRelativeToModule(alloc, module.path, raw_path);
                defer alloc.free(resolved_path);

                const combined = try std.fmt.allocPrint(alloc, "-L{s}", .{resolved_path});
                if (!containsString(ctx.extra_args.items, combined)) {
                    try ctx.extra_args.append(alloc, combined);
                } else {
                    alloc.free(combined);
                }
                i += 2;
                continue;
            }
            i += 1;
            continue;
        }

        if (std.mem.startsWith(u8, flag, "-L") and flag.len > 2) {
            const raw_path = flag[2..];
            const resolved_path = try resolvePathRelativeToModule(alloc, module.path, raw_path);
            defer alloc.free(resolved_path);

            const combined = try std.fmt.allocPrint(alloc, "-L{s}", .{resolved_path});
            if (!containsString(ctx.extra_args.items, combined)) {
                try ctx.extra_args.append(alloc, combined);
            } else {
                alloc.free(combined);
            }
            i += 1;
            continue;
        }

        const flag_copy = utils.dupe(u8, alloc, flag);
        if (!containsString(ctx.extra_args.items, flag_copy)) {
            try ctx.extra_args.append(alloc, flag_copy);
        } else {
            alloc.free(flag_copy);
        }
        i += 1;
    }
}

fn moduleContainsMain(module: *const ModuleInfo) bool {
    if (module.ast.data != .program) return false;
    const prog = module.ast.data.program;
    for (prog.functions.items) |func| {
        if (func.data == .function and std.mem.eql(u8, func.data.function.name, "main")) {
            return true;
        }
    }
    return false;
}

fn moduleMatchesImport(import_path: []const u8, module_name: []const u8) bool {
    if (std.mem.eql(u8, import_path, module_name)) return true;
    if (module_name.len <= import_path.len) return false;
    return std.mem.startsWith(u8, module_name, import_path) and module_name[import_path.len] == '.';
}

fn hasModuleByName(modules: []const ModuleInfo, module_name: []const u8) bool {
    for (modules) |module| {
        if (std.mem.eql(u8, module.name, module_name)) return true;
    }
    return false;
}

const UseSpec = struct {
    module_path: []const u8,
    alias_name: ?[]const u8,
    alias_is_underscore: bool,
};

fn moduleHasFunctionByName(module: *const ModuleInfo, function_name: []const u8) bool {
    if (module.ast.data != .program) return false;
    const prog = module.ast.data.program;
    for (prog.functions.items) |node| {
        switch (node.data) {
            .function => |func| {
                if (std.mem.eql(u8, func.name, function_name)) return true;
            },
            .c_function_decl => |decl| {
                if (std.mem.eql(u8, decl.name, function_name)) return true;
            },
            else => {},
        }
    }
    return false;
}

fn collectUseSpecs(module: *const ModuleInfo, use_specs: *std.ArrayList(UseSpec), alloc: std.mem.Allocator) !void {
    if (module.ast.data != .program) return;
    const prog = module.ast.data.program;
    for (prog.functions.items) |node| {
        if (node.data != .use_stmt) continue;
        const use_stmt = node.data.use_stmt;
        try use_specs.append(alloc, .{
            .module_path = use_stmt.module_path,
            .alias_name = use_stmt.alias_name,
            .alias_is_underscore = use_stmt.alias_is_underscore,
        });
    }
}

fn isModuleHiddenByAlias(module_name: []const u8, use_specs: []const UseSpec) bool {
    for (use_specs) |spec| {
        if (spec.alias_name == null and !spec.alias_is_underscore) continue;
        if (moduleMatchesImport(spec.module_path, module_name)) return true;
    }
    return false;
}

fn isModuleVisibleUnprefixed(module_name: []const u8, use_specs: []const UseSpec) bool {
    var visible_plain = false;
    for (use_specs) |spec| {
        if (spec.alias_name != null or spec.alias_is_underscore) continue;
        if (moduleMatchesImport(spec.module_path, module_name)) {
            visible_plain = true;
            break;
        }
    }
    if (!visible_plain) return false;
    return !isModuleHiddenByAlias(module_name, use_specs);
}

fn hasFunctionInReachableImport(modules: []const ModuleInfo, reachable_modules: *const std.StringHashMap(void), import_path: []const u8, function_name: []const u8) bool {
    for (modules) |module| {
        if (!reachable_modules.contains(module.name)) continue;
        if (!moduleMatchesImport(import_path, module.name)) continue;
        if (moduleHasFunctionByName(&module, function_name)) return true;
    }
    return false;
}

fn isFunctionHiddenByImportOverrides(current_module: *const ModuleInfo, use_specs: []const UseSpec, modules: []const ModuleInfo, reachable_modules: *const std.StringHashMap(void), function_name: []const u8) bool {
    if (moduleHasFunctionByName(current_module, function_name)) return false;

    var visible_any = false;
    var hidden_any = false;

    for (modules) |module| {
        if (!reachable_modules.contains(module.name)) continue;
        if (!moduleHasFunctionByName(&module, function_name)) continue;

        if (isModuleVisibleUnprefixed(module.name, use_specs)) {
            visible_any = true;
            continue;
        }
        if (isModuleHiddenByAlias(module.name, use_specs)) {
            hidden_any = true;
        }
    }

    return hidden_any and !visible_any;
}

fn moduleHasTypeByName(module: *const ModuleInfo, type_name: []const u8) bool {
    if (module.ast.data != .program) return false;
    const prog = module.ast.data.program;
    for (prog.functions.items) |node| {
        switch (node.data) {
            .struct_decl => |decl| {
                if (std.mem.eql(u8, decl.name, type_name)) return true;
            },
            .enum_decl => |decl| {
                if (std.mem.eql(u8, decl.name, type_name)) return true;
            },
            else => {},
        }
    }
    return false;
}

fn hasTypeInReachableImport(modules: []const ModuleInfo, reachable_modules: *const std.StringHashMap(void), import_path: []const u8, type_name: []const u8) bool {
    for (modules) |module| {
        if (!reachable_modules.contains(module.name)) continue;
        if (!moduleMatchesImport(import_path, module.name)) continue;
        if (moduleHasTypeByName(&module, type_name)) return true;
    }
    return false;
}

fn isTypeHiddenByImportOverrides(current_module: *const ModuleInfo, use_specs: []const UseSpec, modules: []const ModuleInfo, reachable_modules: *const std.StringHashMap(void), type_name: []const u8) bool {
    if (moduleHasTypeByName(current_module, type_name)) return false;

    var visible_any = false;
    var hidden_any = false;

    for (modules) |module| {
        if (!reachable_modules.contains(module.name)) continue;
        if (!moduleHasTypeByName(&module, type_name)) continue;

        if (isModuleVisibleUnprefixed(module.name, use_specs)) {
            visible_any = true;
            continue;
        }
        if (isModuleHiddenByAlias(module.name, use_specs)) {
            hidden_any = true;
        }
    }

    return hidden_any and !visible_any;
}

fn isTypeTokenChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.';
}

fn isAllDigits(text: []const u8) bool {
    if (text.len == 0) return false;
    for (text) |ch| {
        if (!std.ascii.isDigit(ch)) return false;
    }
    return true;
}

fn isBuiltinTypeToken(token: []const u8) bool {
    return std.mem.eql(u8, token, "i8") or
        std.mem.eql(u8, token, "i16") or
        std.mem.eql(u8, token, "i32") or
        std.mem.eql(u8, token, "i64") or
        std.mem.eql(u8, token, "u8") or
        std.mem.eql(u8, token, "u16") or
        std.mem.eql(u8, token, "u32") or
        std.mem.eql(u8, token, "u64") or
        std.mem.eql(u8, token, "f16") or
        std.mem.eql(u8, token, "f32") or
        std.mem.eql(u8, token, "f64") or
        std.mem.eql(u8, token, "bool") or
        std.mem.eql(u8, token, "void") or
        std.mem.eql(u8, token, "error") or
        std.mem.eql(u8, token, "arr") or
        std.mem.eql(u8, token, "ptr") or
        std.mem.eql(u8, token, "simd") or
        std.mem.eql(u8, token, "vararg") or
        std.mem.eql(u8, token, "const") or
        std.mem.eql(u8, token, "_");
}

fn resolveNamespacedTypeToken(type_path: []const u8, rewrite_ctx: *RewriteContext, owner_alloc: std.mem.Allocator) ?[]const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, type_path, '.') orelse return null;
    if (dot == 0 or dot + 1 >= type_path.len) return null;

    const namespace_path = type_path[0..dot];
    const symbol_name = type_path[dot + 1 ..];

    const resolved = resolveNamespacePath(namespace_path, rewrite_ctx.use_specs, owner_alloc) orelse return null;
    defer owner_alloc.free(resolved.import_path);

    if (!cachedHasTypeInReachableImport(rewrite_ctx, resolved.import_path, symbol_name)) return null;
    return utils.dupe(u8, owner_alloc, symbol_name);
}

fn rewriteTypeNameForImports(type_name: []const u8, rewrite_ctx: *RewriteContext, owner_alloc: std.mem.Allocator) []const u8 {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(owner_alloc);

    var changed = false;
    var i: usize = 0;
    while (i < type_name.len) {
        if (isTypeTokenChar(type_name[i])) {
            const start = i;
            i += 1;
            while (i < type_name.len and isTypeTokenChar(type_name[i])) : (i += 1) {}
            const token = type_name[start..i];

            var replacement = token;
            var owned_replacement: ?[]const u8 = null;
            defer {
                if (owned_replacement) |owned| {
                    owner_alloc.free(owned);
                }
            }

            if (std.mem.indexOfScalar(u8, token, '.')) |_| {
                if (resolveNamespacedTypeToken(token, rewrite_ctx, owner_alloc)) |resolved_name| {
                    replacement = resolved_name;
                    owned_replacement = resolved_name;
                    changed = true;
                }
            } else if (!isAllDigits(token) and !isBuiltinTypeToken(token)) {
                if (cachedIsTypeHidden(rewrite_ctx, token)) {
                    if (std.fmt.allocPrint(owner_alloc, "__zlang_hidden_import__.{s}", .{token}) catch null) |hidden_name| {
                        replacement = hidden_name;
                        owned_replacement = hidden_name;
                        changed = true;
                    }
                }
            }

            for (replacement) |ch| {
                out.append(owner_alloc, ch) catch return type_name;
            }
            continue;
        }

        out.append(owner_alloc, type_name[i]) catch return type_name;
        i += 1;
    }

    if (!changed) return type_name;
    return out.toOwnedSlice(owner_alloc) catch type_name;
}

fn rewriteNodeTypeFields(rewrite_ctx: *RewriteContext, node: *ast.Node) void {
    switch (node.data) {
        .function => |*func| {
            func.return_type = rewriteTypeNameForImports(func.return_type, rewrite_ctx, node.allocator);
            for (func.parameters.items) |*param| {
                param.type_name = rewriteTypeNameForImports(param.type_name, rewrite_ctx, node.allocator);
            }
        },
        .c_function_decl => |*decl| {
            decl.return_type = rewriteTypeNameForImports(decl.return_type, rewrite_ctx, node.allocator);
            for (decl.parameters.items) |*param| {
                param.type_name = rewriteTypeNameForImports(param.type_name, rewrite_ctx, node.allocator);
            }
        },
        .var_decl => |*decl| {
            decl.type_name = rewriteTypeNameForImports(decl.type_name, rewrite_ctx, node.allocator);
        },
        .struct_decl => |*decl| {
            for (decl.fields.items) |*field| {
                field.type_name = rewriteTypeNameForImports(field.type_name, rewrite_ctx, node.allocator);
            }
        },
        .cast => |*cast_node| {
            if (cast_node.type_name) |type_name| {
                cast_node.type_name = rewriteTypeNameForImports(type_name, rewrite_ctx, node.allocator);
            }
        },
        .expression_block => |*block| {
            block.type_name = rewriteTypeNameForImports(block.type_name, rewrite_ctx, node.allocator);
        },
        .struct_initializer => |*struct_init| {
            struct_init.struct_name = rewriteTypeNameForImports(struct_init.struct_name, rewrite_ctx, node.allocator);
        },
        else => {},
    }
}

fn pathPrefixSuffix(path: []const u8, prefix: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, path, prefix)) return "";
    if (path.len <= prefix.len) return null;
    if (!std.mem.startsWith(u8, path, prefix)) return null;
    if (path[prefix.len] != '.') return null;
    return path[prefix.len + 1 ..];
}

fn appendRefPath(buf: *std.ArrayList(u8), node: *const ast.Node, alloc: std.mem.Allocator) !bool {
    switch (node.data) {
        .identifier => |ident| {
            try buf.appendSlice(alloc, ident.name);
            return true;
        },
        .qualified_identifier => |qual| {
            const ok = try appendRefPath(buf, qual.base, alloc);
            if (!ok) return false;
            try buf.append(alloc, '.');
            try buf.appendSlice(alloc, qual.field);
            return true;
        },
        else => return false,
    }
}

fn extractRefPath(node: *const ast.Node, alloc: std.mem.Allocator) ?[]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(alloc);
    const ok = appendRefPath(&buf, node, alloc) catch return null;
    if (!ok) {
        buf.deinit(alloc);
        return null;
    }
    return buf.toOwnedSlice(alloc) catch null;
}

const NamespaceResolution = struct {
    import_path: []const u8,
};

const RewriteContext = struct {
    current_module: *const ModuleInfo,
    use_specs: []const UseSpec,
    modules: []const ModuleInfo,
    reachable_modules: *const std.StringHashMap(void),
    alloc: std.mem.Allocator,
    hidden_functions: std.StringHashMap(bool),
    hidden_types: std.StringHashMap(bool),
    import_functions: std.StringHashMap(bool),
    import_types: std.StringHashMap(bool),

    fn init(current_module: *const ModuleInfo, use_specs: []const UseSpec, modules: []const ModuleInfo, reachable_modules: *const std.StringHashMap(void), alloc: std.mem.Allocator) RewriteContext {
        return .{
            .current_module = current_module,
            .use_specs = use_specs,
            .modules = modules,
            .reachable_modules = reachable_modules,
            .alloc = alloc,
            .hidden_functions = std.StringHashMap(bool).init(alloc),
            .hidden_types = std.StringHashMap(bool).init(alloc),
            .import_functions = std.StringHashMap(bool).init(alloc),
            .import_types = std.StringHashMap(bool).init(alloc),
        };
    }

    fn deinit(self: *RewriteContext) void {
        var function_it = self.import_functions.iterator();
        while (function_it.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
        }
        var type_it = self.import_types.iterator();
        while (type_it.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
        }
        self.hidden_functions.deinit();
        self.hidden_types.deinit();
        self.import_functions.deinit();
        self.import_types.deinit();
    }
};

fn cacheKey2(alloc: std.mem.Allocator, a: []const u8, b: []const u8) ![]const u8 {
    return std.fmt.allocPrint(alloc, "{s}\x00{s}", .{ a, b });
}

fn cachedHasFunctionInReachableImport(ctx: *RewriteContext, import_path: []const u8, function_name: []const u8) bool {
    const key = cacheKey2(ctx.alloc, import_path, function_name) catch {
        return hasFunctionInReachableImport(ctx.modules, ctx.reachable_modules, import_path, function_name);
    };
    if (ctx.import_functions.get(key)) |cached| {
        ctx.alloc.free(key);
        return cached;
    }
    const result = hasFunctionInReachableImport(ctx.modules, ctx.reachable_modules, import_path, function_name);
    ctx.import_functions.put(key, result) catch {
        ctx.alloc.free(key);
    };
    return result;
}

fn cachedHasTypeInReachableImport(ctx: *RewriteContext, import_path: []const u8, type_name: []const u8) bool {
    const key = cacheKey2(ctx.alloc, import_path, type_name) catch {
        return hasTypeInReachableImport(ctx.modules, ctx.reachable_modules, import_path, type_name);
    };
    if (ctx.import_types.get(key)) |cached| {
        ctx.alloc.free(key);
        return cached;
    }
    const result = hasTypeInReachableImport(ctx.modules, ctx.reachable_modules, import_path, type_name);
    ctx.import_types.put(key, result) catch {
        ctx.alloc.free(key);
    };
    return result;
}

fn cachedIsFunctionHidden(ctx: *RewriteContext, function_name: []const u8) bool {
    if (moduleHasFunctionByName(ctx.current_module, function_name)) return false;
    if (ctx.hidden_functions.get(function_name)) |cached| return cached;
    const result = isFunctionHiddenByImportOverrides(ctx.current_module, ctx.use_specs, ctx.modules, ctx.reachable_modules, function_name);
    ctx.hidden_functions.put(function_name, result) catch {};
    return result;
}

fn cachedIsTypeHidden(ctx: *RewriteContext, type_name: []const u8) bool {
    if (moduleHasTypeByName(ctx.current_module, type_name)) return false;
    if (ctx.hidden_types.get(type_name)) |cached| return cached;
    const result = isTypeHiddenByImportOverrides(ctx.current_module, ctx.use_specs, ctx.modules, ctx.reachable_modules, type_name);
    ctx.hidden_types.put(type_name, result) catch {};
    return result;
}

fn resolveNamespacePath(object_path: []const u8, use_specs: []const UseSpec, alloc: std.mem.Allocator) ?NamespaceResolution {
    var best_rank: u8 = 255;
    var best_prefix_len: usize = 0;
    var best_base: ?[]const u8 = null;
    var best_suffix: []const u8 = "";

    for (use_specs) |spec| {
        if (spec.alias_name) |alias| {
            if (pathPrefixSuffix(object_path, alias)) |suffix| {
                const rank: u8 = 0;
                if (rank < best_rank or (rank == best_rank and alias.len > best_prefix_len)) {
                    best_rank = rank;
                    best_prefix_len = alias.len;
                    best_base = spec.module_path;
                    best_suffix = suffix;
                }
            }
            continue;
        }

        if (spec.alias_is_underscore) {
            if (pathPrefixSuffix(object_path, spec.module_path)) |suffix| {
                const rank: u8 = 1;
                if (rank < best_rank or (rank == best_rank and spec.module_path.len > best_prefix_len)) {
                    best_rank = rank;
                    best_prefix_len = spec.module_path.len;
                    best_base = spec.module_path;
                    best_suffix = suffix;
                }
            }
            continue;
        }

        if (pathPrefixSuffix(object_path, spec.module_path)) |suffix| {
            const rank: u8 = 2;
            if (rank < best_rank or (rank == best_rank and spec.module_path.len > best_prefix_len)) {
                best_rank = rank;
                best_prefix_len = spec.module_path.len;
                best_base = spec.module_path;
                best_suffix = suffix;
            }
        }
    }

    const base = best_base orelse return null;
    if (best_suffix.len == 0) {
        return .{ .import_path = utils.dupe(u8, alloc, base) };
    }

    const import_path = std.fmt.allocPrint(alloc, "{s}.{s}", .{ base, best_suffix }) catch return null;
    return .{ .import_path = import_path };
}

fn convertMethodCallToFunctionCall(node: *ast.Node) void {
    if (node.data != .method_call) return;
    const method = node.data.method_call;
    method.object.destroy();
    node.data = .{ .function_call = .{
        .name = method.method_name,
        .is_libc = false,
        .args = method.args,
    } };
}

fn hideFunctionCallName(call_node: *ast.Node) void {
    if (call_node.data != .function_call) return;
    const call = &call_node.data.function_call;
    const hidden_name = std.fmt.allocPrint(call_node.allocator, "__zlang_hidden_import__.{s}", .{call.name}) catch return;
    call.name = hidden_name;
}

fn rewriteModuleImportsInNode(rewrite_ctx: *RewriteContext, node: *ast.Node) void {
    const alloc = rewrite_ctx.alloc;
    rewriteNodeTypeFields(rewrite_ctx, node);

    switch (node.data) {
        .program => |prog| {
            for (prog.globals.items) |glob| {
                rewriteModuleImportsInNode(rewrite_ctx, glob);
            }
            for (prog.functions.items) |func| {
                rewriteModuleImportsInNode(rewrite_ctx, func);
            }
        },
        .function => |func| {
            if (func.guard) |guard| {
                rewriteModuleImportsInNode(rewrite_ctx, guard);
            }
            for (func.body.items) |stmt| {
                rewriteModuleImportsInNode(rewrite_ctx, stmt);
            }
        },
        .var_decl => |decl| {
            if (decl.initializer) |init| {
                rewriteModuleImportsInNode(rewrite_ctx, init);
            }
        },
        .assignment => |as| {
            rewriteModuleImportsInNode(rewrite_ctx, as.target);
            rewriteModuleImportsInNode(rewrite_ctx, as.value);
        },
        .compound_assignment => |as| {
            rewriteModuleImportsInNode(rewrite_ctx, as.target);
            rewriteModuleImportsInNode(rewrite_ctx, as.value);
        },
        .function_call => |*call| {
            for (call.args.items) |arg| {
                rewriteModuleImportsInNode(rewrite_ctx, arg);
            }

            if (!call.is_libc and cachedIsFunctionHidden(rewrite_ctx, call.name)) {
                hideFunctionCallName(node);
            }
        },
        .method_call => |method| {
            rewriteModuleImportsInNode(rewrite_ctx, method.object);
            for (method.args.items) |arg| {
                rewriteModuleImportsInNode(rewrite_ctx, arg);
            }

            const object_path = extractRefPath(method.object, alloc) orelse return;
            defer alloc.free(object_path);

            const resolved = resolveNamespacePath(object_path, rewrite_ctx.use_specs, alloc) orelse return;
            defer alloc.free(resolved.import_path);

            if (!cachedHasFunctionInReachableImport(rewrite_ctx, resolved.import_path, method.method_name)) return;
            convertMethodCallToFunctionCall(node);
        },
        .return_stmt => |ret| {
            if (ret.expression) |expr| {
                rewriteModuleImportsInNode(rewrite_ctx, expr);
            }
        },
        .defer_stmt => |defer_stmt| rewriteModuleImportsInNode(rewrite_ctx, defer_stmt.expression),
        .if_stmt => |if_stmt| {
            rewriteModuleImportsInNode(rewrite_ctx, if_stmt.condition);
            for (if_stmt.then_body.items) |stmt| {
                rewriteModuleImportsInNode(rewrite_ctx, stmt);
            }
            if (if_stmt.else_body) |else_body| {
                for (else_body.items) |stmt| {
                    rewriteModuleImportsInNode(rewrite_ctx, stmt);
                }
            }
        },
        .for_stmt => |for_stmt| {
            if (for_stmt.condition) |cond| {
                rewriteModuleImportsInNode(rewrite_ctx, cond);
            }
            for (for_stmt.body.items) |stmt| {
                rewriteModuleImportsInNode(rewrite_ctx, stmt);
            }
        },
        .c_for_stmt => |c_for| {
            if (c_for.init) |init| rewriteModuleImportsInNode(rewrite_ctx, init);
            if (c_for.condition) |cond| rewriteModuleImportsInNode(rewrite_ctx, cond);
            if (c_for.increment) |inc| rewriteModuleImportsInNode(rewrite_ctx, inc);
            for (c_for.body.items) |stmt| {
                rewriteModuleImportsInNode(rewrite_ctx, stmt);
            }
        },
        .array_initializer => |arr| for (arr.elements.items) |elem| rewriteModuleImportsInNode(rewrite_ctx, elem),
        .array_index => |arr| {
            rewriteModuleImportsInNode(rewrite_ctx, arr.array);
            rewriteModuleImportsInNode(rewrite_ctx, arr.index);
        },
        .array_assignment => |arr| {
            rewriteModuleImportsInNode(rewrite_ctx, arr.array);
            rewriteModuleImportsInNode(rewrite_ctx, arr.index);
            rewriteModuleImportsInNode(rewrite_ctx, arr.value);
        },
        .array_compound_assignment => |arr| {
            rewriteModuleImportsInNode(rewrite_ctx, arr.array);
            rewriteModuleImportsInNode(rewrite_ctx, arr.index);
            rewriteModuleImportsInNode(rewrite_ctx, arr.value);
        },
        .comparison => |cmp| {
            rewriteModuleImportsInNode(rewrite_ctx, cmp.lhs);
            rewriteModuleImportsInNode(rewrite_ctx, cmp.rhs);
        },
        .binary_op => |bin| {
            rewriteModuleImportsInNode(rewrite_ctx, bin.lhs);
            rewriteModuleImportsInNode(rewrite_ctx, bin.rhs);
        },
        .unary_op => |un| rewriteModuleImportsInNode(rewrite_ctx, un.operand),
        .cast => |cast| rewriteModuleImportsInNode(rewrite_ctx, cast.expr),
        .expression_block => |block| {
            for (block.statements.items) |stmt| {
                rewriteModuleImportsInNode(rewrite_ctx, stmt);
            }
            rewriteModuleImportsInNode(rewrite_ctx, block.result);
        },
        .handled_call_stmt => |handled| {
            rewriteModuleImportsInNode(rewrite_ctx, handled.call);
            for (handled.handlers.items) |handler| {
                for (handler.body.items) |stmt| {
                    rewriteModuleImportsInNode(rewrite_ctx, stmt);
                }
            }
        },
        .match_stmt => |match_stmt| {
            rewriteModuleImportsInNode(rewrite_ctx, match_stmt.condition);
            for (match_stmt.cases.items) |match_case| {
                for (match_case.values.items) |value| {
                    rewriteModuleImportsInNode(rewrite_ctx, value);
                }
                for (match_case.body.items) |stmt| {
                    rewriteModuleImportsInNode(rewrite_ctx, stmt);
                }
            }
        },
        .struct_initializer => |struct_init| {
            for (struct_init.field_values.items) |field_val| {
                rewriteModuleImportsInNode(rewrite_ctx, field_val.value);
            }
        },
        .qualified_identifier => |qual| rewriteModuleImportsInNode(rewrite_ctx, qual.base),
        .simd_initializer => |simd_init| for (simd_init.elements.items) |elem| rewriteModuleImportsInNode(rewrite_ctx, elem),
        .simd_index => |simd_idx| {
            rewriteModuleImportsInNode(rewrite_ctx, simd_idx.simd);
            rewriteModuleImportsInNode(rewrite_ctx, simd_idx.index);
        },
        .simd_assignment => |simd_ass| {
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.simd);
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.index);
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.value);
        },
        .simd_compound_assignment => |simd_ass| {
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.simd);
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.index);
            rewriteModuleImportsInNode(rewrite_ctx, simd_ass.value);
        },
        .simd_method_call => |simd_method| {
            rewriteModuleImportsInNode(rewrite_ctx, simd_method.simd);
            for (simd_method.args.items) |arg| {
                rewriteModuleImportsInNode(rewrite_ctx, arg);
            }
        },
        else => {},
    }
}

fn loadStdModulesForDep(dep: []const u8, modules: *std.ArrayList(ModuleInfo), loaded_paths: *std.StringHashMap(void), arena: std.mem.Allocator, alloc: std.mem.Allocator, ctx: *Context) !void {
    if (std.mem.eql(u8, dep, "std")) {
        const stdlib_path = try getStdlibPath(alloc);
        defer alloc.free(stdlib_path);

        var dir = try std.Io.Dir.cwd().openDir(process_io, stdlib_path, .{ .iterate = true });
        defer dir.close(process_io);

        var it = dir.iterate();
        while (try it.next(process_io)) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".zl")) continue;

            const full = try std.fs.path.join(alloc, &[_][]const u8{ stdlib_path, entry.name });
            defer alloc.free(full);

            if (loaded_paths.contains(full)) continue;

            const module = try parseModuleFile(full, arena, alloc, ctx);
            try loaded_paths.put(utils.dupe(u8, alloc, full), {});
            try modules.append(alloc, module);
        }
        return;
    }

    var has_zstdpath: bool = false;
    const std_path = resolveStdModule(dep, alloc, &has_zstdpath) orelse return error.ModuleNotFound;
    defer alloc.free(std_path);

    if (loaded_paths.contains(std_path)) return;

    const std_module = try parseModuleFile(std_path, arena, alloc, ctx);
    try loaded_paths.put(utils.dupe(u8, alloc, std_path), {});
    try modules.append(alloc, std_module);
}

fn collectStdModulePathsForDep(dep: []const u8, pending_paths: *std.ArrayList([]const u8), loaded_paths: *std.StringHashMap(void), alloc: std.mem.Allocator) !void {
    if (std.mem.eql(u8, dep, "std")) {
        const stdlib_path = try getStdlibPath(alloc);
        defer alloc.free(stdlib_path);

        var dir = try std.Io.Dir.cwd().openDir(process_io, stdlib_path, .{ .iterate = true });
        defer dir.close(process_io);

        var it = dir.iterate();
        while (try it.next(process_io)) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".zl")) continue;

            const full = try std.fs.path.join(alloc, &[_][]const u8{ stdlib_path, entry.name });
            errdefer alloc.free(full);
            if (loaded_paths.contains(full) or containsString(pending_paths.items, full)) {
                alloc.free(full);
                continue;
            }
            try pending_paths.append(alloc, full);
        }
        return;
    }

    var has_zstdpath: bool = false;
    const std_path = resolveStdModule(dep, alloc, &has_zstdpath) orelse return error.ModuleNotFound;
    errdefer alloc.free(std_path);
    if (loaded_paths.contains(std_path) or containsString(pending_paths.items, std_path)) {
        alloc.free(std_path);
        return;
    }
    try pending_paths.append(alloc, std_path);
}

const ExpandedPath = struct { path: []const u8, expansions: usize };

fn expandPathIfNeeded(original: []const u8, idx: usize, alloc: std.mem.Allocator) !?ExpandedPath {
    const host = plugin_host_ptr orelse return null;
    if (host.syntax_blocks.items.len == 0) return null;
    const bytes = std.Io.Dir.cwd().readFileAlloc(process_io, original, alloc, .limited(64 * 1024 * 1024)) catch return null;
    defer alloc.free(bytes);
    var result = try zlx_preprocess.expandExtensionBlocks(alloc, host, original, bytes);
    defer result.report.deinit(alloc);
    if (result.report.expansions == 0) {
        alloc.free(result.source);
        return null;
    }
    const tmp_path = try std.fmt.allocPrint(alloc, "/tmp/zlang-zlx-{d}-{d}-{s}", .{ @as(u64, @intCast(nanoTimestamp())), idx, std.fs.path.basename(original) });
    var tmp = std.Io.Dir.cwd().createFile(process_io, tmp_path, .{ .truncate = true }) catch {
        alloc.free(tmp_path);
        alloc.free(result.source);
        return null;
    };
    defer tmp.close(process_io);
    var buf: [4096]u8 = undefined;
    var writer = tmp.writer(process_io, &buf);
    writer.interface.writeAll(result.source) catch {};
    writer.interface.flush() catch {};
    alloc.free(result.source);
    if (result.report.source_map.items.len != 0) {
        var entries: std.ArrayList(ZlxSourceMapEntry) = .empty;
        errdefer entries.deinit(alloc);
        for (result.report.source_map.items) |entry| {
            try entries.append(alloc, .{
                .generated_offset = entry.generated_offset,
                .original_line = entry.original_line,
                .original_column = entry.original_column,
            });
        }
        try zlx_source_maps.append(alloc, .{
            .generated_path = try alloc.dupe(u8, tmp_path),
            .original_file = try alloc.dupe(u8, original),
            .entries = entries,
        });
    }
    return .{ .path = tmp_path, .expansions = result.report.expansions };
}

fn loadModulesFromPaths(paths: []const []const u8, modules: *std.ArrayList(ModuleInfo), loaded_paths: *std.StringHashMap(void), arena: std.mem.Allocator, alloc: std.mem.Allocator, ctx: *Context) !void {
    if (paths.len == 0) return;

    var effective_paths = try alloc.alloc([]const u8, paths.len);
    defer alloc.free(effective_paths);
    for (paths, 0..) |path, i| {
        effective_paths[i] = path;
        if (expandPathIfNeeded(path, i, alloc) catch null) |rewritten| {
            effective_paths[i] = rewritten.path;
        }
    }
    for (paths) |path| {
        try loaded_paths.put(utils.dupe(u8, alloc, path), {});
    }

    if (ctx.max_threads > 1 and paths.len > 1) {
        var tasks = try alloc.alloc(InitialLoadTask, paths.len);
        defer alloc.free(tasks);

        for (effective_paths, 0..) |path, i| {
            tasks[i] = .{
                .file_path = path,
                .ctx = ctx,
                .alloc = alloc,
            };
        }

        const worker_count = @min(ctx.max_threads, paths.len);
        var next_task_idx: usize = 0;
        while (next_task_idx < tasks.len) {
            const batch_count = @min(worker_count, tasks.len - next_task_idx);
            var threads = try alloc.alloc(std.Thread, batch_count);
            defer alloc.free(threads);

            for (0..batch_count) |j| {
                threads[j] = try std.Thread.spawn(.{}, runInitialLoadTask, .{&tasks[next_task_idx + j]});
            }
            for (threads) |thread| thread.join();
            next_task_idx += batch_count;
        }

        for (tasks) |*task| {
            if (task.err) |err| return err;
            var prep = task.result.?;
            defer prep.deinit(alloc);
            const module = try parseModuleFromPrepared(task.file_path, arena, alloc, ctx, &prep);
            try modules.append(alloc, module);
        }
        return;
    }

    for (effective_paths) |path| {
        const module = try parseModuleFile(path, arena, alloc, ctx);
        try modules.append(alloc, module);
    }
}

fn clearModuleBindings(ctx: *Context, alloc: std.mem.Allocator) void {
    for (ctx.module_registrations.items) |*entry| entry.deinit(alloc);
    ctx.module_registrations.clearRetainingCapacity();
    for (ctx.function_module_bindings.items) |*entry| entry.deinit(alloc);
    ctx.function_module_bindings.clearRetainingCapacity();
    for (ctx.global_module_bindings.items) |*entry| entry.deinit(alloc);
    ctx.global_module_bindings.clearRetainingCapacity();
}

fn modulePathByName(module_registrations: []const ModuleRegistration, module_name: []const u8) ?[]const u8 {
    for (module_registrations) |entry| {
        if (std.mem.eql(u8, entry.module_name, module_name)) return entry.module_path;
    }
    return null;
}

fn parseMultiFile(ctx: *Context, alloc: std.mem.Allocator, timing: ?*ParseTiming) !ast.ArenaAST {
    var arena_ast = ast.ArenaAST.init(alloc);
    const arena = arena_ast.allocator();
    clearModuleBindings(ctx, alloc);

    var modules: std.ArrayList(ModuleInfo) = .empty;
    defer {
        for (modules.items) |*module| {
            module.deinit(alloc);
        }
        modules.deinit(alloc);
    }

    var loaded_paths = std.StringHashMap(void).init(alloc);
    defer loaded_paths.deinit();

    const load_start = nanoTimestamp();
    if (ctx.max_threads > 1 and ctx.input_files.items.len > 1) {
        var unique_inputs: std.ArrayList([]const u8) = .empty;
        defer unique_inputs.deinit(alloc);

        for (ctx.input_files.items) |input_file| {
            if (loaded_paths.contains(input_file)) continue;
            try loaded_paths.put(utils.dupe(u8, alloc, input_file), {});
            try unique_inputs.append(alloc, input_file);
        }

        var tasks = try alloc.alloc(InitialLoadTask, unique_inputs.items.len);
        defer alloc.free(tasks);

        for (unique_inputs.items, 0..) |input_file, i| {
            tasks[i] = .{
                .file_path = input_file,
                .ctx = ctx,
                .alloc = alloc,
            };
        }

        const worker_count = @min(ctx.max_threads, unique_inputs.items.len);
        var next_task_idx: usize = 0;
        while (next_task_idx < tasks.len) {
            const batch_count = @min(worker_count, tasks.len - next_task_idx);
            var threads = try alloc.alloc(std.Thread, batch_count);
            defer alloc.free(threads);

            for (0..batch_count) |j| {
                threads[j] = try std.Thread.spawn(.{}, runInitialLoadTask, .{&tasks[next_task_idx + j]});
            }
            for (threads) |thread| thread.join();
            next_task_idx += batch_count;
        }

        for (tasks) |*task| {
            if (task.err) |err| return err;
            var prep = task.result.?;
            defer prep.deinit(alloc);
            const module = try parseModuleFromPrepared(task.file_path, arena, alloc, ctx, &prep);
            try modules.append(alloc, module);
        }
    } else {
        for (ctx.input_files.items) |input_file| {
            if (loaded_paths.contains(input_file)) continue;
            const module = try parseModuleFile(input_file, arena, alloc, ctx);
            try loaded_paths.put(utils.dupe(u8, alloc, input_file), {});
            try modules.append(alloc, module);
        }
    }
    if (timing) |t| t.load_time_ns = @intCast(nanoTimestamp() - load_start);

    const std_start = nanoTimestamp();
    var scan_idx: usize = 0;
    while (scan_idx < modules.items.len) : (scan_idx += 1) {
        const module = modules.items[scan_idx];
        var pending_std_paths: std.ArrayList([]const u8) = .empty;
        defer {
            for (pending_std_paths.items) |path| alloc.free(path);
            pending_std_paths.deinit(alloc);
        }

        for (module.dependencies.items) |dep| {
            if (!(std.mem.eql(u8, dep, "std") or std.mem.startsWith(u8, dep, "std."))) continue;
            if (hasModuleByName(modules.items, dep)) continue;

            collectStdModulePathsForDep(dep, &pending_std_paths, &loaded_paths, alloc) catch {
                std.debug.print("\x1b[31mError:\x1b[0m Cannot resolve module '\x1b[33m{s}\x1b[0m' imported from {s}\n", .{ dep, module.path });
                arena_ast.deinit();
                return error.ModuleNotFound;
            };
        }

        for (module.dependencies.items) |dep| {
            if (std.mem.eql(u8, dep, "std") or std.mem.startsWith(u8, dep, "std.")) continue;
            if (hasModuleByName(modules.items, dep)) continue;
            const map = if (plugin_module_paths) |*m| m else continue;
            const abs = map.get(dep) orelse continue;
            if (loaded_paths.contains(abs) or containsString(pending_std_paths.items, abs)) continue;
            const dup = utils.dupe(u8, alloc, abs);
            try pending_std_paths.append(alloc, dup);
        }

        loadModulesFromPaths(pending_std_paths.items, &modules, &loaded_paths, arena, alloc, ctx) catch {
            std.debug.print("\x1b[31mError:\x1b[0m Failed to load std dependencies imported from {s}\n", .{module.path});
            arena_ast.deinit();
            return error.ModuleNotFound;
        };
    }
    if (timing) |t| t.std_time_ns = @intCast(nanoTimestamp() - std_start);

    const resolve_start = nanoTimestamp();
    var module_names = std.StringHashMap(void).init(alloc);
    defer module_names.deinit();
    for (modules.items) |module| {
        try module_names.put(module.name, {});
    }

    var module_name_list: std.ArrayList([]const u8) = .empty;
    defer module_name_list.deinit(alloc);
    var module_name_indices: std.ArrayList(usize) = .empty;
    defer module_name_indices.deinit(alloc);
    for (modules.items, 0..) |module, i| {
        try module_name_list.append(alloc, module.name);
        try module_name_indices.append(alloc, i);
    }

    var resolve_tasks = try alloc.alloc(ResolvePrecomputeTask, modules.items.len);
    defer {
        for (resolve_tasks) |*task| task.deinit();
        alloc.free(resolve_tasks);
    }
    for (modules.items, 0..) |*module, i| {
        resolve_tasks[i] = ResolvePrecomputeTask.init(module, module_name_list.items, module_name_indices.items, alloc);
    }
    try runTasksParallel(ResolvePrecomputeTask, resolve_tasks, ctx.max_threads, alloc, runResolvePrecomputeTask);

    var root_modules = std.StringHashMap(void).init(alloc);
    defer root_modules.deinit();
    for (modules.items) |*module| {
        if (moduleContainsMain(module)) {
            try root_modules.put(module.name, {});
        }
    }
    if (root_modules.count() == 0) {
        for (ctx.input_files.items) |input_file| {
            for (modules.items) |*module| {
                if (std.mem.eql(u8, module.path, input_file)) {
                    try root_modules.put(module.name, {});
                }
            }
        }
    }

    var reachable_modules = std.StringHashMap(void).init(alloc);
    defer reachable_modules.deinit();
    var queue: std.ArrayList([]const u8) = .empty;
    defer queue.deinit(alloc);

    var root_it = root_modules.iterator();
    while (root_it.next()) |entry| {
        try reachable_modules.put(entry.key_ptr.*, {});
        try queue.append(alloc, entry.key_ptr.*);
    }

    var q_idx: usize = 0;
    while (q_idx < queue.items.len) : (q_idx += 1) {
        const current_module_name = queue.items[q_idx];

        for (modules.items, 0..) |module, module_idx| {
            if (!std.mem.eql(u8, module.name, current_module_name)) continue;
            const task = &resolve_tasks[module_idx];
            if (task.err) |err| return err;
            if (task.unmatched_dep_index) |dep_idx| {
                const dep = module.dependencies.items[dep_idx];
                std.debug.print("\x1b[31mError:\x1b[0m Cannot resolve module '\x1b[33m{s}\x1b[0m' imported from module {s}\n", .{ dep, current_module_name });
                arena_ast.deinit();
                return error.ModuleNotFound;
            }

            for (task.matched_dep_indices.items) |matched_idx| {
                const candidate = modules.items[matched_idx].name;
                if (!reachable_modules.contains(candidate)) {
                    try reachable_modules.put(candidate, {});
                    try queue.append(alloc, candidate);
                }
            }
        }
    }
    if (timing) |t| t.resolve_time_ns = @intCast(nanoTimestamp() - resolve_start);

    const rewrite_start = nanoTimestamp();
    try rewriteReachableModulesParallel(modules.items, &reachable_modules, ctx.max_threads, alloc);
    if (timing) |t| t.rewrite_time_ns = @intCast(nanoTimestamp() - rewrite_start);

    const merge_start = nanoTimestamp();
    var merge_scan_tasks = try alloc.alloc(MergeScanTask, modules.items.len);
    defer alloc.free(merge_scan_tasks);
    for (modules.items, 0..) |*module, i| {
        merge_scan_tasks[i] = .{ .module = module };
    }
    try runTasksParallel(MergeScanTask, merge_scan_tasks, ctx.max_threads, alloc, runMergeScanTask);

    var expected_functions: usize = 0;
    var expected_globals: usize = 0;
    for (merge_scan_tasks, 0..) |task, i| {
        _ = i;
        if (!reachable_modules.contains(task.module.name)) continue;
        expected_functions += task.function_count;
        expected_globals += task.global_count;
    }

    for (modules.items) |*module| {
        if (reachable_modules.contains(module.name)) {
            try appendModuleFlagsToContext(ctx, module, alloc);
        }
    }

    const merged_program_data = ast.NodeData{
        .program = ast.Program{
            .functions = .empty,
            .globals = .empty,
        },
    };

    const merged_program = ast.Node.create(arena, merged_program_data);
    try merged_program.data.program.functions.ensureTotalCapacity(arena, expected_functions);
    try merged_program.data.program.globals.ensureTotalCapacity(arena, expected_globals);

    for (modules.items) |module| {
        if (!reachable_modules.contains(module.name)) continue;

        var module_registration = ModuleRegistration{
            .module_name = utils.dupe(u8, alloc, module.name),
            .module_path = utils.dupe(u8, alloc, module.path),
            .line_count = module.line_count,
            .dependencies = .empty,
        };
        for (module.dependencies.items) |dep| {
            try module_registration.dependencies.append(alloc, utils.dupe(u8, alloc, dep));
        }
        try ctx.module_registrations.append(alloc, module_registration);

        switch (module.ast.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    if (func.data != .use_stmt) {
                        if (func.data == .function) {
                            try ctx.function_module_bindings.append(alloc, .{
                                .symbol_name = utils.dupe(u8, alloc, func.data.function.name),
                                .module_name = utils.dupe(u8, alloc, module.name),
                            });
                        }
                        try merged_program.data.program.functions.append(arena, func);
                    }
                }
                for (prog.globals.items) |glob| {
                    if (glob.data == .var_decl) {
                        try ctx.global_module_bindings.append(alloc, .{
                            .symbol_name = utils.dupe(u8, alloc, glob.data.var_decl.name),
                            .module_name = utils.dupe(u8, alloc, module.name),
                        });
                    }
                    try merged_program.data.program.globals.append(arena, glob);
                }
            },
            else => {
                try merged_program.data.program.functions.append(arena, module.ast);
            },
        }
    }
    if (timing) |t| t.merge_time_ns = @intCast(nanoTimestamp() - merge_start);

    arena_ast.setRoot(merged_program);
    return arena_ast;
}

pub fn read_file(file_name: []const u8) anyerror![]const u8 {
    const cwd = std.Io.Dir.cwd();
    cwd.access(process_io, file_name, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return errors.ReadFileError.FileNotFound;
        }
        return errors.ReadFileError.AccessDenied;
    };

    const file_stat = try cwd.statFile(process_io, file_name, .{});
    if (file_stat.kind == .file) {
        if (file_stat.size == 0) {
            return utils.dupe(u8, allocator, "");
        }
        return try cwd.readFileAlloc(process_io, file_name, allocator, .limited(file_stat.size + 1));
    }

    return errors.ReadFileError.InvalidPath;
}

fn parseArgs(args: [][:0]u8) anyerror!Context {
    var context = Context.init();
    errdefer context.deinit(allocator);

    var i: usize = 1;

    if (args.len > 1 and std.mem.eql(u8, args[1], "run")) {
        context.run_mode = true;
        i = 2;
    }

    var parsing_program_args = false;

    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--")) {
            parsing_program_args = true;
            i += 1;

            while (i < args.len) : (i += 1) {
                try context.program_args.append(allocator, args[i]);
            }
            break;
        }

        if (parsing_program_args) {
            try context.program_args.append(allocator, args[i]);
            continue;
        }

        switch (args[i][0]) {
            '-' => {
                const flag = args[i];
                if (std.mem.eql(u8, flag, "-keepll")) {
                    context.keepll = true;
                } else if (std.mem.eql(u8, flag, "-dast")) {
                    context.show_ast = true;
                } else if (std.mem.eql(u8, flag, "-verbose")) {
                    context.show_ast = true;
                    context.verbose = true;
                } else if (std.mem.eql(u8, flag, "-q") or std.mem.eql(u8, flag, "-quiet")) {
                    context.quiet = true;
                } else if (std.mem.eql(u8, flag, "-stats")) {
                    context.stats = true;
                } else if (std.mem.eql(u8, flag, "-optimize")) {
                    context.optimize = true;
                } else if (std.mem.eql(u8, flag, "-verify-ir")) {
                    context.verify_ir = true;
                } else if (std.mem.eql(u8, flag, "-o")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoOutputPath;
                    context.output = args[i];
                } else if (std.mem.eql(u8, flag, "-arch")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoArch;
                    context.arch = args[i];
                } else if (std.mem.eql(u8, flag, "-j")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.InvalidArgument;
                    const parsed = std.fmt.parseInt(usize, args[i], 10) catch return errors.CLIError.InvalidArgument;
                    if (parsed == 0) return errors.CLIError.InvalidArgument;
                    context.max_threads = parsed;
                } else if (std.mem.eql(u8, flag, "-link")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.InvalidArgument;
                    try context.link_objects.append(allocator, args[i]);
                } else if (std.mem.eql(u8, flag, "-c")) {
                    try context.extra_args.append(allocator, flag);
                    context.output = "output.o";
                } else if (std.mem.eql(u8, flag, "-D") or std.mem.startsWith(u8, flag, "-D")) {
                    var define_spec: []const u8 = undefined;
                    if (std.mem.eql(u8, flag, "-D")) {
                        i += 1;
                        if (i >= args.len) return errors.CLIError.InvalidArgument;
                        define_spec = args[i];
                    } else {
                        define_spec = flag[2..];
                    }

                    const eq_index = std.mem.indexOfScalar(u8, define_spec, '=') orelse return errors.CLIError.InvalidArgument;
                    const name = std.mem.trim(u8, define_spec[0..eq_index], " \t");
                    const value = std.mem.trim(u8, define_spec[eq_index + 1 ..], " \t");
                    if (name.len == 0) return errors.CLIError.InvalidArgument;

                    var replaced = false;
                    for (context.define_overrides.items) |*entry| {
                        if (std.mem.eql(u8, entry.name, name)) {
                            allocator.free(entry.value);
                            entry.value = utils.dupe(u8, allocator, value);
                            replaced = true;
                            break;
                        }
                    }

                    if (!replaced) {
                        try context.define_overrides.append(allocator, .{
                            .name = utils.dupe(u8, allocator, name),
                            .value = utils.dupe(u8, allocator, value),
                        });
                    }
                } else if (std.mem.startsWith(u8, flag, "-l") and flag.len > 2) {
                    try context.extra_args.append(allocator, flag);
                } else if (std.mem.startsWith(u8, flag, "-L") and flag.len > 2) {
                    try context.extra_args.append(allocator, flag);
                } else if (std.mem.eql(u8, flag, "-l") or std.mem.eql(u8, flag, "-L")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.InvalidArgument;
                    const combined = try std.fmt.allocPrint(allocator, "{s}{s}", .{ flag, args[i] });
                    defer allocator.free(combined);
                    try context.extra_args.append(allocator, combined);
                } else if (std.mem.startsWith(u8, flag, "-Wl,")) {
                    try context.extra_args.append(allocator, flag);
                } else if (std.mem.eql(u8, flag, "-no-extensions")) {
                    context.no_extensions = true;
                } else if (std.mem.eql(u8, flag, "-isolated")) {
                    context.isolated = true;
                    context.no_extensions = true;
                } else if (std.mem.eql(u8, flag, "-help")) {
                    return errors.CLIError.NoHelp;
                } else {
                    try context.extra_args.append(allocator, flag);
                }
            },
            else => {
                const arg = args[i];
                if (std.mem.endsWith(u8, arg, ".zl")) {
                    try context.input_files.append(allocator, arg);
                } else {
                    // Files claimed by plugin-registered file extensions (e.g. an
                    // extension's own source type) are not known at parse time,
                    // since plugins load after argument parsing. Such files land
                    // in link_objects here and are reclassified into input_files
                    // once the plugin host is loaded.
                    const stat = std.Io.Dir.cwd().statFile(process_io, arg, .{}) catch |err| {
                        if (err == error.FileNotFound) {
                            try context.link_objects.append(allocator, arg);
                            continue;
                        }
                        return err;
                    };

                    if (stat.kind == .directory) {
                        try collectZlFilesFromDir(allocator, arg, &context.input_files);
                    } else {
                        try context.link_objects.append(allocator, arg);
                    }
                }
            },
        }
    }
    // link_objects may still hold files claimed by a plugin-registered file
    // extension; those are reclassified into input_files after the plugin host
    // loads, so a bare link_objects list is not necessarily an empty input.
    return if (context.input_files.items.len == 0 and context.link_objects.items.len == 0)
        errors.CLIError.NoInputPath
    else
        context;
}

const ZliLoadedModule = struct {
    path: []const u8,
    module_name: []const u8,
};

fn deinitZliLoadedModules(loaded_modules: *std.ArrayList(ZliLoadedModule), alloc: std.mem.Allocator) void {
    for (loaded_modules.items) |item| {
        alloc.free(item.path);
        alloc.free(item.module_name);
    }
    loaded_modules.deinit(alloc);
}

fn deinitOwnedStringList(strings: *std.ArrayList([]const u8), alloc: std.mem.Allocator) void {
    for (strings.items) |item| {
        alloc.free(item);
    }
    strings.deinit(alloc);
}

fn clearOwnedStringList(strings: *std.ArrayList([]const u8), alloc: std.mem.Allocator) void {
    for (strings.items) |item| {
        alloc.free(item);
    }
    strings.clearRetainingCapacity();
}

fn clearZliLoadedModules(loaded_modules: *std.ArrayList(ZliLoadedModule), alloc: std.mem.Allocator) void {
    for (loaded_modules.items) |item| {
        alloc.free(item.path);
        alloc.free(item.module_name);
    }
    loaded_modules.clearRetainingCapacity();
}

fn containsLoadedPath(list: []const ZliLoadedModule, path: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item.path, path)) return true;
    }
    return false;
}

fn stripMatchingQuotes(text: []const u8) []const u8 {
    if (text.len >= 2) {
        if ((text[0] == '"' and text[text.len - 1] == '"') or (text[0] == '\'' and text[text.len - 1] == '\'')) {
            return text[1 .. text.len - 1];
        }
    }
    return text;
}

fn inferModuleNameFromFile(file_path: []const u8, alloc: std.mem.Allocator) ![]const u8 {
    var temp_arena = ast.ArenaAST.init(alloc);
    defer temp_arena.deinit();

    var temp_ctx = Context.init();
    defer temp_ctx.deinit(alloc);

    var module = try parseModuleFile(file_path, temp_arena.allocator(), alloc, &temp_ctx);
    defer module.deinit(alloc);

    return utils.dupe(u8, alloc, module.name);
}

fn loadModuleIntoZli(loaded_modules: *std.ArrayList(ZliLoadedModule), file_path_arg: []const u8, alloc: std.mem.Allocator) !void {
    const trimmed = trimSpaces(file_path_arg);
    if (trimmed.len == 0) {
        std.debug.print("Usage: :load <file.zl>\n", .{});
        return;
    }

    const raw_path = stripMatchingQuotes(trimmed);
    const abs_path = std.Io.Dir.cwd().realPathFileAlloc(process_io, raw_path, alloc) catch |err| {
        std.debug.print("zli: cannot load '{s}': {}\n", .{ raw_path, err });
        return;
    };

    if (containsLoadedPath(loaded_modules.items, abs_path)) {
        std.debug.print("zli: already loaded {s}\n", .{abs_path});
        alloc.free(abs_path);
        return;
    }

    const module_name = inferModuleNameFromFile(abs_path, alloc) catch |err| {
        std.debug.print("zli: failed to parse module '{s}': {}\n", .{ abs_path, err });
        alloc.free(abs_path);
        return;
    };

    try loaded_modules.append(alloc, .{
        .path = abs_path,
        .module_name = module_name,
    });

    std.debug.print("zli: loaded {s} (module {s})\n", .{ abs_path, module_name });
}

fn addZliImport(imports: *std.ArrayList([]const u8), import_name_arg: []const u8, alloc: std.mem.Allocator) !void {
    const trimmed = trimSpaces(import_name_arg);
    if (trimmed.len == 0) {
        std.debug.print("Usage: :import <module.path>\n", .{});
        return;
    }

    if (containsString(imports.items, trimmed)) {
        std.debug.print("zli: import already present: {s}\n", .{trimmed});
        return;
    }

    try imports.append(alloc, utils.dupe(u8, alloc, trimmed));
    std.debug.print("zli: import added: {s}\n", .{trimmed});
}

fn printZliHelp() void {
    std.debug.print(
        \\zli commands:
        \\  :load <file.zl>     Load a source file into session
        \\  :import <module>    Add a module import for eval snippets
        \\  :files              Show loaded files and imports
        \\  :clear              Clear loaded files and imports
        \\  :reload             No-op (files are compiled fresh each eval)
        \\  :help               Show this help
        \\  :quit / :q / :exit  Exit zli
        \\
        \\Expressions print their value automatically (ghci-style).
        \\Non-expression lines are executed as statements inside temporary main().
        \\
    , .{});
}

fn makeZliStatement(line: []const u8, alloc: std.mem.Allocator) ![]const u8 {
    const trimmed = trimSpaces(line);
    if (trimmed.len == 0) return utils.dupe(u8, alloc, "");

    const last = trimmed[trimmed.len - 1];
    if (last == ';' or last == '}') {
        return utils.dupe(u8, alloc, trimmed);
    }

    return std.fmt.allocPrint(alloc, "{s};", .{trimmed});
}

fn isZliCalcExprChar(ch: u8) bool {
    if (std.ascii.isWhitespace(ch) or std.ascii.isDigit(ch)) return true;
    return ch == '+' or ch == '-' or ch == '*' or ch == '/' or ch == '%' or ch == '(' or ch == ')' or ch == '.' or ch == '\'';
}

fn isLikelyCalculatorExpression(expr: []const u8) bool {
    var has_op = false;
    var has_digit = false;
    for (expr) |ch| {
        if (!isZliCalcExprChar(ch)) return false;
        if (std.ascii.isDigit(ch)) has_digit = true;
        if (ch == '+' or ch == '-' or ch == '*' or ch == '/' or ch == '%') has_op = true;
    }
    return has_op and has_digit;
}

fn rewriteCalcIntegersToFloat(expr: []const u8, alloc: std.mem.Allocator) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(alloc);

    var i: usize = 0;
    while (i < expr.len) {
        const ch = expr[i];
        if (!std.ascii.isDigit(ch)) {
            try out.append(alloc, ch);
            i += 1;
            continue;
        }

        const start = i;
        i += 1;
        while (i < expr.len and (std.ascii.isDigit(expr[i]) or expr[i] == '\'')) : (i += 1) {}

        var has_fraction = false;
        if (i + 1 < expr.len and expr[i] == '.' and std.ascii.isDigit(expr[i + 1])) {
            has_fraction = true;
            i += 1;
            while (i < expr.len and (std.ascii.isDigit(expr[i]) or expr[i] == '\'')) : (i += 1) {}
        }

        var has_exp = false;
        if (i < expr.len and (expr[i] == 'e' or expr[i] == 'E')) {
            var j = i + 1;
            if (j < expr.len and (expr[j] == '+' or expr[j] == '-')) j += 1;
            const exp_start = j;
            while (j < expr.len and std.ascii.isDigit(expr[j])) : (j += 1) {}
            if (j > exp_start) {
                has_exp = true;
                i = j;
            }
        }

        try out.appendSlice(alloc, expr[start..i]);
        if (!has_fraction and !has_exp) {
            try out.appendSlice(alloc, ".0");
        }
    }

    return out.toOwnedSlice(alloc);
}

fn makeZliExpressionPrintStatement(line: []const u8, alloc: std.mem.Allocator) ![]const u8 {
    const trimmed = trimSpaces(line);
    if (trimmed.len == 0) return utils.dupe(u8, alloc, "");

    if (isLikelyCalculatorExpression(trimmed) and std.mem.indexOfScalar(u8, trimmed, '/') != null and std.mem.indexOfScalar(u8, trimmed, '%') == null) {
        const calc_expr = try rewriteCalcIntegersToFloat(trimmed, alloc);
        defer alloc.free(calc_expr);
        return std.fmt.allocPrint(alloc, "@printf(\"%.15g\\n\", ({s}));", .{calc_expr});
    }

    return std.fmt.allocPrint(alloc, "println(\"${{{s}}}\");", .{trimmed});
}

const ZliSnippetMode = enum {
    statement,
    expression,
};

fn buildZliSnippetSource(loaded_modules: []const ZliLoadedModule, imports: []const []const u8, statement_line: []const u8, mode: ZliSnippetMode, alloc: std.mem.Allocator) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(alloc);
    var out_writer = std.Io.Writer.Allocating.fromArrayList(alloc, &out);
    defer out = out_writer.toArrayList();
    var writer = &out_writer.writer;

    try writer.print("module zli.session;\n", .{});

    var use_set = std.StringHashMap(void).init(alloc);
    defer use_set.deinit();

    for (imports) |import_name| {
        if (!use_set.contains(import_name)) {
            try use_set.put(import_name, {});
            try writer.print("use {s}\n", .{import_name});
        }
    }

    for (loaded_modules) |loaded| {
        if (!use_set.contains(loaded.module_name)) {
            try use_set.put(loaded.module_name, {});
            try writer.print("use {s}\n", .{loaded.module_name});
        }
    }

    if (mode == .expression and !use_set.contains("std")) {
        try use_set.put("std", {});
        try writer.print("use std\n", .{});
    }

    const statement = switch (mode) {
        .statement => try makeZliStatement(statement_line, alloc),
        .expression => try makeZliExpressionPrintStatement(statement_line, alloc),
    };
    defer alloc.free(statement);

    try writer.print("\nfun main() >> i32 {{\n    {s}\n    return 0;\n}}\n", .{statement});
    return try out_writer.toOwnedSlice();
}

fn runZliSnippet(exe_path: []const u8, loaded_modules: []const ZliLoadedModule, imports: []const []const u8, line: []const u8, mode: ZliSnippetMode, alloc: std.mem.Allocator) !u8 {
    const snippet_src = try buildZliSnippetSource(loaded_modules, imports, line, mode, alloc);
    defer alloc.free(snippet_src);

    const tmp_path = try std.fmt.allocPrint(alloc, "__zli_eval_{d}.zl", .{nanoTimestamp()});
    defer alloc.free(tmp_path);

    var tmp_file = std.Io.Dir.cwd().createFile(process_io, tmp_path, .{ .truncate = true }) catch |err| {
        std.debug.print("zli: failed to create temp file: {}\n", .{err});
        return 1;
    };
    defer tmp_file.close(process_io);

    var file_buffer: [4096]u8 = undefined;
    var file_writer = tmp_file.writer(process_io, &file_buffer);
    try file_writer.interface.writeAll(snippet_src);
    try file_writer.interface.flush();
    defer std.Io.Dir.cwd().deleteFile(process_io, tmp_path) catch {};

    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(alloc);

    try argv.append(alloc, exe_path);
    try argv.append(alloc, "run");
    for (loaded_modules) |loaded| {
        try argv.append(alloc, loaded.path);
    }
    try argv.append(alloc, tmp_path);

    var child = try std.process.spawn(process_io, .{ .argv = argv.items });
    const term = try child.wait(process_io);
    return switch (term) {
        .exited => |code| code,
        else => 1,
    };
}

const ZliCapturedRun = struct {
    exit_code: u8,
    stdout: []u8,
    stderr: []u8,
};

fn runZliSnippetCaptured(exe_path: []const u8, loaded_modules: []const ZliLoadedModule, imports: []const []const u8, line: []const u8, mode: ZliSnippetMode, alloc: std.mem.Allocator) !ZliCapturedRun {
    const snippet_src = try buildZliSnippetSource(loaded_modules, imports, line, mode, alloc);
    defer alloc.free(snippet_src);

    const tmp_path = try std.fmt.allocPrint(alloc, "__zli_eval_{d}.zl", .{nanoTimestamp()});
    defer alloc.free(tmp_path);

    var tmp_file = std.Io.Dir.cwd().createFile(process_io, tmp_path, .{ .truncate = true }) catch |err| {
        std.debug.print("zli: failed to create temp file: {}\n", .{err});
        return error.TempFileFailed;
    };
    defer tmp_file.close(process_io);

    var file_buffer: [4096]u8 = undefined;
    var file_writer = tmp_file.writer(process_io, &file_buffer);
    try file_writer.interface.writeAll(snippet_src);
    try file_writer.interface.flush();
    defer std.Io.Dir.cwd().deleteFile(process_io, tmp_path) catch {};

    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(alloc);

    try argv.append(alloc, exe_path);
    try argv.append(alloc, "run");
    for (loaded_modules) |loaded| {
        try argv.append(alloc, loaded.path);
    }
    try argv.append(alloc, tmp_path);

    const run_res = try std.process.run(alloc, process_io, .{
        .argv = argv.items,
        .stdout_limit = .limited(1024 * 1024),
        .stderr_limit = .limited(1024 * 1024),
    });

    return .{
        .exit_code = switch (run_res.term) {
            .exited => |code| code,
            else => 1,
        },
        .stdout = run_res.stdout,
        .stderr = run_res.stderr,
    };
}

fn printZliFiles(loaded_modules: []const ZliLoadedModule, imports: []const []const u8) void {
    if (loaded_modules.len == 0 and imports.len == 0) {
        std.debug.print("zli: no loaded files or imports\n", .{});
        return;
    }

    if (loaded_modules.len > 0) {
        std.debug.print("Loaded files:\n", .{});
        for (loaded_modules) |loaded| {
            std.debug.print("  - {s}  (module {s})\n", .{ loaded.path, loaded.module_name });
        }
    }

    if (imports.len > 0) {
        std.debug.print("Imports:\n", .{});
        for (imports) |import_name| {
            std.debug.print("  - {s}\n", .{import_name});
        }
    }
}

fn zliPushHistory(history: *std.ArrayList([]const u8), line: []const u8, alloc: std.mem.Allocator) !void {
    const trimmed = trimSpaces(line);
    if (trimmed.len == 0) return;
    if (history.items.len > 0 and std.mem.eql(u8, history.items[history.items.len - 1], line)) return;
    try history.append(alloc, utils.dupe(u8, alloc, line));
}

fn zliReadByte(stdin_file: std.Io.File) !?u8 {
    var b: [1]u8 = undefined;
    const n = try std.posix.read(stdin_file.handle, &b);
    if (n == 0) return null;
    return b[0];
}

fn writeStdoutAll(bytes: []const u8) !void {
    var buffer: [256]u8 = undefined;
    var writer = std.Io.File.stdout().writer(process_io, &buffer);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
}

fn zliReplaceLine(buf: *std.ArrayList(u8), cursor: *usize, text: []const u8, alloc: std.mem.Allocator) !void {
    buf.clearRetainingCapacity();
    try buf.appendSlice(alloc, text);
    cursor.* = buf.items.len;
}

fn zliRedraw(prompt: []const u8, line: []const u8, cursor: usize, alloc: std.mem.Allocator) !void {
    try writeStdoutAll("\r");
    try writeStdoutAll(prompt);
    try writeStdoutAll(line);
    try writeStdoutAll("\x1b[K");

    const tail = line.len - cursor;
    if (tail > 0) {
        const move_left = try std.fmt.allocPrint(alloc, "\x1b[{d}D", .{tail});
        defer alloc.free(move_left);
        try writeStdoutAll(move_left);
    }
}

fn readZliLineInteractive(prompt: []const u8, history: *std.ArrayList([]const u8), alloc: std.mem.Allocator) !?[]const u8 {
    const stdin_file = std.Io.File.stdin();

    const orig_termios = try std.posix.tcgetattr(stdin_file.handle);
    var raw_termios = orig_termios;
    raw_termios.lflag.ICANON = false;
    raw_termios.lflag.ECHO = false;
    raw_termios.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    raw_termios.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    try std.posix.tcsetattr(stdin_file.handle, .NOW, raw_termios);
    defer std.posix.tcsetattr(stdin_file.handle, .NOW, orig_termios) catch {};

    var line_buf: std.ArrayList(u8) = .empty;
    defer line_buf.deinit(alloc);

    var draft_buf: std.ArrayList(u8) = .empty;
    defer draft_buf.deinit(alloc);

    var cursor: usize = 0;
    var history_index: ?usize = null;

    try zliRedraw(prompt, line_buf.items, cursor, alloc);

    while (true) {
        const maybe_ch = try zliReadByte(stdin_file);
        if (maybe_ch == null) {
            try writeStdoutAll("\n");
            if (line_buf.items.len == 0) return null;
            const out = try line_buf.toOwnedSlice(alloc);
            try zliPushHistory(history, out, alloc);
            return out;
        }

        const ch = maybe_ch.?;
        switch (ch) {
            '\r', '\n' => {
                try writeStdoutAll("\r\n");
                const out = try line_buf.toOwnedSlice(alloc);
                try zliPushHistory(history, out, alloc);
                return out;
            },
            3 => {
                try writeStdoutAll("^C\r\n");
                return utils.dupe(u8, alloc, "");
            },
            4 => {
                if (line_buf.items.len == 0) {
                    try writeStdoutAll("\r\n");
                    return null;
                }
            },
            127, 8 => {
                if (cursor > 0) {
                    const len = line_buf.items.len;
                    std.mem.copyForwards(u8, line_buf.items[cursor - 1 .. len - 1], line_buf.items[cursor..len]);
                    line_buf.shrinkRetainingCapacity(len - 1);
                    cursor -= 1;
                    try zliRedraw(prompt, line_buf.items, cursor, alloc);
                }
            },
            1 => {
                cursor = 0;
                try zliRedraw(prompt, line_buf.items, cursor, alloc);
            },
            5 => {
                cursor = line_buf.items.len;
                try zliRedraw(prompt, line_buf.items, cursor, alloc);
            },
            12 => {
                try writeStdoutAll("\x1b[2J\x1b[H");
                try zliRedraw(prompt, line_buf.items, cursor, alloc);
            },
            27 => {
                const esc1 = try zliReadByte(stdin_file) orelse continue;
                if (esc1 != '[') continue;
                const esc2 = try zliReadByte(stdin_file) orelse continue;

                switch (esc2) {
                    'D' => {
                        if (cursor > 0) {
                            cursor -= 1;
                            try zliRedraw(prompt, line_buf.items, cursor, alloc);
                        }
                    },
                    'C' => {
                        if (cursor < line_buf.items.len) {
                            cursor += 1;
                            try zliRedraw(prompt, line_buf.items, cursor, alloc);
                        }
                    },
                    'A' => {
                        if (history.items.len == 0) continue;
                        if (history_index == null) {
                            draft_buf.clearRetainingCapacity();
                            try draft_buf.appendSlice(alloc, line_buf.items);
                            history_index = history.items.len - 1;
                        } else if (history_index.? > 0) {
                            history_index = history_index.? - 1;
                        }
                        try zliReplaceLine(&line_buf, &cursor, history.items[history_index.?], alloc);
                        try zliRedraw(prompt, line_buf.items, cursor, alloc);
                    },
                    'B' => {
                        if (history_index == null) continue;
                        if (history_index.? + 1 < history.items.len) {
                            history_index = history_index.? + 1;
                            try zliReplaceLine(&line_buf, &cursor, history.items[history_index.?], alloc);
                        } else {
                            history_index = null;
                            try zliReplaceLine(&line_buf, &cursor, draft_buf.items, alloc);
                        }
                        try zliRedraw(prompt, line_buf.items, cursor, alloc);
                    },
                    'H' => {
                        cursor = 0;
                        try zliRedraw(prompt, line_buf.items, cursor, alloc);
                    },
                    'F' => {
                        cursor = line_buf.items.len;
                        try zliRedraw(prompt, line_buf.items, cursor, alloc);
                    },
                    '3' => {
                        const tilde = try zliReadByte(stdin_file) orelse continue;
                        if (tilde == '~' and cursor < line_buf.items.len) {
                            const len = line_buf.items.len;
                            std.mem.copyForwards(u8, line_buf.items[cursor .. len - 1], line_buf.items[cursor + 1 .. len]);
                            line_buf.shrinkRetainingCapacity(len - 1);
                            try zliRedraw(prompt, line_buf.items, cursor, alloc);
                        }
                    },
                    else => {},
                }
            },
            else => {
                if (ch < 32) continue;

                const len = line_buf.items.len;
                try line_buf.append(alloc, 0);
                if (cursor < len) {
                    std.mem.copyBackwards(u8, line_buf.items[cursor + 1 .. len + 1], line_buf.items[cursor..len]);
                }
                line_buf.items[cursor] = ch;
                cursor += 1;
                try zliRedraw(prompt, line_buf.items, cursor, alloc);
            },
        }
    }
}

fn runZli(cli_args: [][:0]u8, alloc: std.mem.Allocator) !u8 {
    if (!interpreter.checkLLIAvailable(alloc)) {
        std.debug.print("Error: lli (LLVM interpreter) is not available.\n", .{});
        std.debug.print("Please ensure LLVM is installed and lli is in your PATH.\n", .{});
        return 1;
    }

    const exe_path = try std.process.executablePathAlloc(process_io, alloc);
    defer alloc.free(exe_path);

    var loaded_modules: std.ArrayList(ZliLoadedModule) = .empty;
    defer deinitZliLoadedModules(&loaded_modules, alloc);

    var imports: std.ArrayList([]const u8) = .empty;
    defer deinitOwnedStringList(&imports, alloc);

    for (cli_args) |arg| {
        try loadModuleIntoZli(&loaded_modules, arg, alloc);
    }

    std.debug.print("zli: interactive mode (type :help for commands)\n", .{});

    const stdin_file = std.Io.File.stdin();
    const stdin_is_tty = stdin_file.isTty(process_io) catch false;

    var history: std.ArrayList([]const u8) = .empty;
    defer deinitOwnedStringList(&history, alloc);

    var stdin_buf: [4096]u8 = undefined;
    var stdin_reader = stdin_file.reader(process_io, &stdin_buf);

    while (true) {
        var owned_line: ?[]const u8 = null;
        defer if (owned_line) |line_to_free| alloc.free(line_to_free);

        const line = if (stdin_is_tty) blk: {
            const maybe_line = try readZliLineInteractive("zli> ", &history, alloc);
            if (maybe_line == null) break;
            owned_line = maybe_line.?;
            break :blk maybe_line.?;
        } else blk: {
            std.debug.print("zli> ", .{});
            const maybe_line = stdin_reader.interface.takeDelimiter('\n') catch |err| switch (err) {
                error.StreamTooLong => {
                    _ = stdin_reader.interface.discardDelimiterInclusive('\n') catch {};
                    std.debug.print("zli: input line is too long\n", .{});
                    continue;
                },
                else => {
                    std.debug.print("zli: failed to read input: {}\n", .{err});
                    break;
                },
            };
            if (maybe_line == null) {
                std.debug.print("\n", .{});
                break;
            }
            break :blk maybe_line.?;
        };

        const trimmed = trimSpaces(line);
        if (trimmed.len == 0) continue;

        if (trimmed[0] == ':') {
            const cmd = trimSpaces(trimmed[1..]);

            if (std.mem.eql(u8, cmd, "q") or std.mem.eql(u8, cmd, "quit") or std.mem.eql(u8, cmd, "exit")) {
                break;
            } else if (std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "h")) {
                printZliHelp();
            } else if (std.mem.eql(u8, cmd, "files")) {
                printZliFiles(loaded_modules.items, imports.items);
            } else if (std.mem.eql(u8, cmd, "clear")) {
                clearZliLoadedModules(&loaded_modules, alloc);
                clearOwnedStringList(&imports, alloc);
                std.debug.print("zli: cleared\n", .{});
            } else if (std.mem.eql(u8, cmd, "reload")) {
                std.debug.print("zli: reload is a no-op (files compile fresh on each eval)\n", .{});
            } else if (std.mem.startsWith(u8, cmd, "load ")) {
                const arg = trimSpaces(cmd[5..]);
                try loadModuleIntoZli(&loaded_modules, arg, alloc);
            } else if (std.mem.eql(u8, cmd, "load")) {
                std.debug.print("Usage: :load <file.zl>\n", .{});
            } else if (std.mem.startsWith(u8, cmd, "import ")) {
                const arg = trimSpaces(cmd[7..]);
                try addZliImport(&imports, arg, alloc);
            } else if (std.mem.eql(u8, cmd, "import")) {
                std.debug.print("Usage: :import <module.path>\n", .{});
            } else {
                std.debug.print("zli: unknown command ':{s}' (type :help)\n", .{cmd});
            }

            continue;
        }

        const ends_as_statement = trimmed[trimmed.len - 1] == ';' or trimmed[trimmed.len - 1] == '}';

        const expr_candidate: ?[]const u8 = if (!ends_as_statement) trimmed else null;

        if (expr_candidate) |expr_text| {
            const expr_eval = runZliSnippetCaptured(exe_path, loaded_modules.items, imports.items, expr_text, .expression, alloc) catch |err| {
                std.debug.print("zli: expression evaluation failed: {}\n", .{err});
                continue;
            };
            defer alloc.free(expr_eval.stdout);
            defer alloc.free(expr_eval.stderr);

            if (expr_eval.exit_code == 0) {
                if (expr_eval.stdout.len > 0) std.debug.print("{s}", .{expr_eval.stdout});
                if (expr_eval.stderr.len > 0) std.debug.print("{s}", .{expr_eval.stderr});
                continue;
            }
        }

        const exit_code = runZliSnippet(exe_path, loaded_modules.items, imports.items, trimmed, .statement, alloc) catch |err| {
            std.debug.print("zli: evaluation failed: {}\n", .{err});
            continue;
        };
        if (exit_code != 0) {
            std.debug.print("zli: command exited with code {d}\n", .{exit_code});
        }
    }

    return 0;
}

fn collectZlFilesFromDir(alloc: std.mem.Allocator, dir_path: []const u8, files_list: *std.ArrayList([]const u8)) !void {
    var threaded: std.Io.Threaded = .init(alloc, .{});
    defer threaded.deinit();
    const run = try std.process.run(alloc, threaded.io(), .{ .argv = &[_][]const u8{ "find", dir_path, "-type", "f", "-name", "*.zl" } });
    defer alloc.free(run.stdout);
    defer alloc.free(run.stderr);
    if (run.term != .exited or run.term.exited != 0) return error.FileNotFound;

    var it = std.mem.tokenizeScalar(u8, run.stdout, '\n');
    while (it.next()) |path| {
        try files_list.append(alloc, utils.dupe(u8, alloc, path));
    }
}

fn isSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

fn trimSpaces(s: []const u8) []const u8 {
    return std.mem.trim(u8, s, " \t\r\n");
}

fn asciiLower(a: []const u8, alloc: std.mem.Allocator) ![]u8 {
    var out = utils.alloc(u8, alloc, a.len);
    for (a, 0..) |ch, i| {
        out[i] = std.ascii.toLower(ch);
    }
    return out;
}

fn stripCommentsAndPreproc(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(alloc);
    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '*') {
            i += 2;
            while (i + 1 < input.len and !(input[i] == '*' and input[i + 1] == '/')) : (i += 1) {}
            if (i + 1 < input.len) i += 1;
            continue;
        }
        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '/') {
            while (i < input.len and input[i] != '\n') : (i += 1) {}
            continue;
        }
        if (input[i] == '#') {
            if (i == 0 or input[i - 1] == '\n') {
                while (i < input.len and input[i] != '\n') : (i += 1) {}
                continue;
            }
        }
        if (input[i] != '\r') {
            try out.append(alloc, input[i]);
        }
    }
    return out.toOwnedSlice(alloc);
}

fn collectStatements(alloc: std.mem.Allocator, content: []const u8) !std.ArrayList([]const u8) {
    var stmts: std.ArrayList([]const u8) = .empty;
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);
    var depth: usize = 0;
    for (content) |ch| {
        if (ch == '(') depth += 1;
        if (ch == ')') {
            if (depth > 0) depth -= 1;
        }
        if (ch == ';' and depth == 0) {
            const stmt = utils.dupe(u8, alloc, trimSpaces(buf.items));
            try stmts.append(alloc, stmt);
            buf.clearRetainingCapacity();
        } else {
            if (ch == '\n') {
                if (buf.items.len == 0 or buf.items[buf.items.len - 1] != ' ') {
                    try buf.append(alloc, ' ');
                }
            } else {
                try buf.append(alloc, ch);
            }
        }
    }
    return stmts;
}

fn containsAny(s: []const u8, subs: []const []const u8) bool {
    for (subs) |sub| {
        if (std.mem.indexOf(u8, s, sub) != null) return true;
    }
    return false;
}

fn stripArraySuffix(param: []const u8) []const u8 {
    if (std.mem.indexOfScalar(u8, param, '[')) |pos| {
        return trimSpaces(param[0..pos]);
    }
    return trimSpaces(param);
}

fn stripTrailingParamName(param_token: []const u8) []const u8 {
    var token = stripArraySuffix(param_token);
    token = trimSpaces(token);
    if (token.len == 0) return token;
    var idx: isize = @as(isize, @intCast(token.len)) - 1;
    while (idx >= 0 and isSpace(token[@as(usize, @intCast(idx))])) : (idx -= 1) {}
    if (idx < 0) return token;
    var end_id: isize = idx;
    while (end_id >= 0 and isIdentChar(token[@as(usize, @intCast(end_id))])) : (end_id -= 1) {}
    const id_start: isize = end_id + 1;
    if (id_start < 0 or id_start > idx) return token;
    const word = token[@as(usize, @intCast(id_start))..@as(usize, @intCast(idx + 1))];
    const lower = std.ascii.eqlIgnoreCase;
    const known_types = [_][]const u8{
        "const",    "volatile", "restrict",
        "unsigned", "signed",   "short",
        "long",     "int",      "char",
        "float",    "double",   "void",
        "size_t",   "struct",   "union",
        "enum",
    };
    for (known_types) |kw| {
        if (lower(word, kw)) return token;
    }
    var p: isize = id_start - 1;
    while (p >= 0 and isSpace(token[@as(usize, @intCast(p))])) : (p -= 1) {}
    if (p >= 0 and token[@as(usize, @intCast(p))] == '*') {
        return trimSpaces(token[0..@as(usize, @intCast(id_start))]);
    }
    return trimSpaces(token[0..@as(usize, @intCast(id_start))]);
}

fn countSubstring(hay: []const u8, needle: []const u8) usize {
    var cnt: usize = 0;
    var start: usize = 0;
    while (std.mem.indexOfPos(u8, hay, start, needle)) |pos| {
        cnt += 1;
        start = pos + needle.len;
        if (start >= hay.len) break;
    }
    return cnt;
}

fn buildPtrType(alloc: std.mem.Allocator, base: []const u8, depth: usize) ![]u8 {
    var t = utils.dupe(u8, alloc, base);
    var d: usize = 0;
    while (d < depth) : (d += 1) {
        const wrapped = try std.fmt.allocPrint(alloc, "ptr<{s}>", .{t});
        t = wrapped;
    }
    return t;
}

fn mapCTypeToZType(alloc: std.mem.Allocator, raw0: []const u8) !?[]u8 {
    var raw = trimSpaces(raw0);
    if (raw.len == 0) return null;
    var ptr_depth: usize = 0;
    {
        var tmp: std.ArrayList(u8) = .empty;
        defer tmp.deinit(alloc);
        for (raw) |ch| {
            if (ch == '*') {
                ptr_depth += 1;
            } else {
                try tmp.append(alloc, ch);
            }
        }
        raw = utils.dupe(u8, alloc, trimSpaces(tmp.items));
    }

    var lower = try asciiLower(raw, alloc);
    {
        var tmp: std.ArrayList(u8) = .empty;
        defer tmp.deinit(alloc);
        var seen_space = false;
        for (lower) |ch| {
            if (isSpace(ch)) {
                if (!seen_space) {
                    try tmp.append(alloc, ' ');
                    seen_space = true;
                }
            } else {
                seen_space = false;
                try tmp.append(alloc, ch);
            }
        }
        lower = utils.dupe(u8, alloc, trimSpaces(tmp.items));
    }

    const has_unsigned = std.mem.indexOf(u8, lower, "unsigned") != null;
    const has_signed = std.mem.indexOf(u8, lower, "signed") != null;
    const has_short = std.mem.indexOf(u8, lower, "short") != null;
    const long_count = countSubstring(lower, "long");
    const has_int = std.mem.indexOf(u8, lower, "int") != null;
    const has_char = std.mem.indexOf(u8, lower, "char") != null;
    const has_float = std.mem.indexOf(u8, lower, "float") != null;
    const has_double = std.mem.indexOf(u8, lower, "double") != null;
    const has_size_t = std.mem.indexOf(u8, lower, "size_t") != null;
    const has_void = std.mem.indexOf(u8, lower, "void") != null;
    const is_agg = containsAny(lower, &[_][]const u8{ "struct ", "union ", "enum " });

    var base: []const u8 = "";
    if (has_size_t) {
        base = "u64";
    } else if (has_float and !has_double) {
        base = "f32";
    } else if (has_double) {
        base = "f64";
    } else if (has_char) {
        base = if (ptr_depth > 0 or has_unsigned) "u8" else "i8";
    } else if (has_short) {
        base = if (has_unsigned) "u16" else "i16";
    } else if (long_count >= 2) {
        base = if (has_unsigned) "u64" else "i64";
    } else if (long_count == 1 and !has_double) {
        base = if (has_unsigned) "u64" else "i64";
    } else if (has_int or has_signed or has_unsigned) {
        base = if (has_unsigned) "u32" else "i32";
    } else if (has_void) {
        base = "void";
    } else if (is_agg) {
        base = "void";
    } else {
        if (ptr_depth == 0) return null;
        base = "void";
    }

    if (std.mem.eql(u8, base, "void") and ptr_depth == 0) {
        return utils.dupe(u8, alloc, base);
    }

    return try buildPtrType(alloc, base, ptr_depth);
}

fn parseAndWriteWrapper(alloc: std.mem.Allocator, stmt_in: []const u8) !?[]const u8 {
    var stmt = trimSpaces(stmt_in);
    if (stmt.len == 0) return null;
    if (containsAny(stmt, &[_][]const u8{ "typedef", "=" })) return null;
    if (containsAny(stmt, &[_][]const u8{ "{", "}" })) return null;

    const open = std.mem.indexOfScalar(u8, stmt, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, stmt, ')') orelse return null;
    if (close <= open) return null;

    const inner = stmt[open + 1 .. close];
    if (std.mem.indexOfScalar(u8, inner, '(') != null or std.mem.indexOfScalar(u8, inner, ')') != null) return null;
    var k: isize = @as(isize, @intCast(open)) - 1;
    while (k >= 0 and isSpace(stmt[@as(usize, @intCast(k))])) : (k -= 1) {}
    if (k < 0) return null;
    if (stmt[@as(usize, @intCast(k))] == '*' or stmt[@as(usize, @intCast(k))] == ')') return null;
    var end_ident: isize = k;
    while (end_ident >= 0 and isIdentChar(stmt[@as(usize, @intCast(end_ident))])) : (end_ident -= 1) {}
    const name_start: usize = @as(usize, @intCast(end_ident + 1));
    const name_end: usize = @as(usize, @intCast(k + 1));
    if (name_end <= name_start) return null;
    const name = trimSpaces(stmt[name_start..name_end]);
    if (name.len == 0) return null;
    const ret_raw = trimSpaces(stmt[0..name_start]);
    const ret_z = try mapCTypeToZType(alloc, ret_raw) orelse return null;
    var params_buf: std.ArrayList([]const u8) = .empty;
    defer params_buf.deinit(alloc);
    const inner_trim = trimSpaces(inner);
    if (!(inner_trim.len == 0 or std.mem.eql(u8, inner_trim, "void"))) {
        if (std.mem.indexOf(u8, inner_trim, "...") != null) return null;
        var it = std.mem.splitScalar(u8, inner_trim, ',');
        while (it.next()) |raw_param| {
            const p = stripTrailingParamName(raw_param);
            const zt_maybe = try mapCTypeToZType(alloc, p);
            if (zt_maybe == null) return null;
            try params_buf.append(alloc, zt_maybe.?);
        }
    }

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(alloc);

    const wrap_start = try std.fmt.allocPrint(alloc, "wrap @{s}(", .{name});
    defer alloc.free(wrap_start);
    try output.appendSlice(alloc, wrap_start);

    var i: usize = 0;
    while (i < params_buf.items.len) : (i += 1) {
        if (i > 0) {
            try output.appendSlice(alloc, ", ");
        }
        const param_str = try std.fmt.allocPrint(alloc, "p{d}: {s}", .{ i, params_buf.items[i] });
        defer alloc.free(param_str);
        try output.appendSlice(alloc, param_str);
    }

    const wrap_end = try std.fmt.allocPrint(alloc, ") >> {s};\n", .{ret_z});
    defer alloc.free(wrap_end);
    try output.appendSlice(alloc, wrap_end);

    return try output.toOwnedSlice(alloc);
}

fn generateWrapperFromHeader(alloc: std.mem.Allocator, header_path: []const u8, out_path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();
    const header_src = try read_file(header_path);
    defer allocator.free(header_src);
    const cleaned = try stripCommentsAndPreproc(a, header_src);
    var stmts = try collectStatements(a, cleaned);
    defer stmts.deinit(a);
    var file = try std.Io.Dir.cwd().createFile(process_io, out_path, .{ .truncate = true });
    defer file.close(process_io);
    var file_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(process_io, &file_buffer);
    const header_comment = try std.fmt.allocPrint(a, "// Auto-generated by zlang wrap from: {s}\n", .{header_path});
    try file_writer.interface.writeAll(header_comment);
    for (stmts.items) |stmt| {
        const maybe_wrapper_str = parseAndWriteWrapper(a, stmt) catch |e| {
            switch (e) {
                error.OutOfMemory => return e,
            }
        };
        if (maybe_wrapper_str) |wrapper_str| {
            try file_writer.interface.writeAll(wrapper_str);
            a.free(wrapper_str);
        }
    }
    try file_writer.interface.flush();
}
pub fn main(init: std.process.Init) !u8 {
    process_io = init.io;

    var arg_it = try std.process.Args.Iterator.initAllocator(init.minimal.args, allocator);
    defer arg_it.deinit();

    var args_list: std.ArrayList([:0]u8) = .empty;
    defer {
        for (args_list.items) |arg| allocator.free(arg);
        args_list.deinit(allocator);
    }
    while (arg_it.next()) |arg| {
        try args_list.append(allocator, try allocator.dupeZ(u8, arg));
    }
    const args = args_list.items;
    if (args.len < 2) {
        help.printHelp();
        return 1;
    }
    if (std.mem.eql(u8, args[1], "help")) {
        if (args.len > 2 and std.mem.eql(u8, args[2], "syntax")) {
            help.printHelpSyntax();
        } else {
            help.printHelp();
            var plugin_host = zlx_host.Host.init(allocator);
            defer plugin_host.deinit();
            _ = zlx_runtime.loadAllInstalled(allocator, process_io, &plugin_host) catch {};
            zlx_runtime.printPluginExtensions(&plugin_host);
        }
        return 0;
    }
    if (std.mem.eql(u8, args[1], "-version") or std.mem.eql(u8, args[1], "--version") or std.mem.eql(u8, args[1], "version")) {
        report.printVersionInfo();
        return 0;
    }
    if (std.mem.eql(u8, args[1], "zli")) {
        const zli_args = if (args.len > 2) args[2..] else args[args.len..args.len];
        return runZli(zli_args, allocator);
    }
    if (try zlx_commands.handle(args, allocator, process_io)) |exit_code| {
        return exit_code;
    }
    if (std.mem.eql(u8, args[1], "wrap") or std.mem.eql(u8, args[1], "wrap-clang")) {
        const use_clang = std.mem.eql(u8, args[1], "wrap-clang");
        if (args.len < 5) {
            if (use_clang) {
                std.debug.print("Usage: zlang wrap-clang <file.h> -o <file.zl> [-- <clang args...>]\n", .{});
            } else {
                std.debug.print("Usage: zlang wrap <file.h> -o <file.zl>\n", .{});
            }
            return 1;
        }

        const header_path = args[2];
        var out_path: ?[]const u8 = null;
        var clang_args_start: usize = args.len;

        var i: usize = 3;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "--")) {
                clang_args_start = i + 1;
                break;
            }
            if (std.mem.eql(u8, args[i], "-o")) {
                if (i + 1 >= args.len) {
                    std.debug.print("Error: missing output after -o\n", .{});
                    return 1;
                }
                out_path = args[i + 1];
                i += 1;
            }
        }

        if (out_path == null) {
            if (use_clang) {
                std.debug.print("Usage: zlang wrap-clang <file.h> -o <file.zl> [-- <clang args...>]\n", .{});
            } else {
                std.debug.print("Usage: zlang wrap <file.h> -o <file.zl>\n", .{});
            }
            return 1;
        }

        if (use_clang) {
            const clang_args = if (clang_args_start < args.len) args[clang_args_start..] else args[args.len..args.len];
            wrapgen.generateFromHeaderWithClang(allocator, header_path, out_path.?, clang_args) catch |err| {
                std.debug.print("Error generating clang wrapper for {s}: {}\n", .{ header_path, err });
                return 1;
            };
        } else {
            wrapgen.generateFromHeader(allocator, header_path, out_path.?) catch |err| {
                std.debug.print("Error generating wrapper for {s}: {}\n", .{ header_path, err });
                return 1;
            };
        }

        std.debug.print("Generated wrapper: {s}\n", .{out_path.?});
        return 0;
    }

    var ctx = parseArgs(args) catch |err| {
        const error_msg = switch (err) {
            errors.CLIError.NoInputPath => "No input path specified",
            errors.CLIError.NoOutputPath => "No output path specified after -o",
            errors.CLIError.NoArch => "No target specified after -arch",
            errors.CLIError.InvalidArgument => "Unrecognized argument",
            errors.CLIError.NoHelp => {
                help.printHelp();
                return 0;
            },
            else => "Unknown error while parsing arguments",
        };
        std.debug.print("{s}\n", .{error_msg});
        return 1;
    };
    defer ctx.deinit(allocator);

    var plugin_host = zlx_host.Host.init(allocator);
    defer plugin_host.deinit();
    plugin_host.setCliArgs(ctx.extra_args.items) catch {};
    if (!ctx.no_extensions) {
        _ = zlx_runtime.loadAllInstalled(allocator, process_io, &plugin_host) catch {};
        plugin_host_ptr = &plugin_host;
    } else if (!ctx.isolated) {
        zlx_runtime.loadManifestModulesOnly(allocator, process_io, &plugin_host) catch {};
        plugin_host_ptr = &plugin_host;
    }
    defer plugin_host_ptr = null;
    if (ctx.verbose and !ctx.quiet and ctx.no_extensions) {
        if (ctx.isolated) {
            std.debug.print("zlx: isolated mode, plugins and extension modules disabled\n", .{});
        } else {
            std.debug.print("zlx: -no-extensions, native plugins disabled\n", .{});
        }
    }

    // Reclassify link objects whose extension is claimed by a loaded plugin's
    // registered file extension: move them from link_objects to input_files so
    // the file-extension dispatch below can hand them to the owning plugin.
    if (plugin_host.file_extensions.items.len != 0) {
        var li: usize = 0;
        while (li < ctx.link_objects.items.len) {
            const obj = ctx.link_objects.items[li];
            var claimed = false;
            for (plugin_host.file_extensions.items) |reg| {
                if (std.mem.endsWith(u8, obj, reg.extension)) {
                    claimed = true;
                    break;
                }
            }
            if (claimed) {
                _ = ctx.link_objects.orderedRemove(li);
                try ctx.input_files.append(allocator, obj);
            } else li += 1;
        }
    }

    plugin_module_paths = .init(allocator);
    if (zlx_store.Store.init(allocator)) |store| {
        var st = store;
        defer st.deinit(allocator);
        for (plugin_host.modules.items) |entry| {
            const owner = entry.owner orelse continue;
            const modules_root = st.pluginModulesDir(allocator, owner) catch continue;
            defer allocator.free(modules_root);
            const abs = std.fs.path.join(allocator, &.{ modules_root, entry.path }) catch continue;
            errdefer allocator.free(abs);
            std.Io.Dir.cwd().access(process_io, abs, .{}) catch {
                allocator.free(abs);
                continue;
            };
            const name_owned = allocator.dupe(u8, entry.name) catch {
                allocator.free(abs);
                continue;
            };
            _ = plugin_module_paths.?.put(name_owned, abs) catch {
                allocator.free(name_owned);
                allocator.free(abs);
            };
        }
    } else |_| {}
    defer if (plugin_module_paths) |*map| {
        var it = map.iterator();
        while (it.next()) |e| {
            allocator.free(e.key_ptr.*);
            allocator.free(e.value_ptr.*);
        }
        map.deinit();
        plugin_module_paths = null;
    };
    {
        var ei: usize = 0;
        while (ei < ctx.extra_args.items.len) {
            const arg = ctx.extra_args.items[ei];
            var matched_flag: ?[]const u8 = null;
            for (plugin_host.cli_flags.items) |flag| {
                if (std.mem.eql(u8, flag.name, arg) or
                    (arg.len > flag.name.len and arg[flag.name.len] == '=' and std.mem.eql(u8, flag.name, arg[0..flag.name.len])))
                {
                    matched_flag = flag.name;
                    break;
                }
            }
            if (matched_flag) |flag_name| {
                try ctx.plugin_flags.append(allocator, try allocator.dupe(u8, flag_name));
                _ = ctx.extra_args.orderedRemove(ei);
            } else {
                ei += 1;
            }
        }
    }
    var used_owners: std.StringHashMap(void) = .init(allocator);
    defer used_owners.deinit();
    for (ctx.plugin_flags.items) |used_flag| {
        for (plugin_host.cli_flags.items) |flag| {
            if (!std.mem.eql(u8, flag.name, used_flag)) continue;
            if (flag.owner) |o| _ = used_owners.put(o, {}) catch {};
        }
    }
    if (zlx_runtime.scanUseImports(allocator, process_io, ctx.input_files.items)) |imports_const| {
        var imports = imports_const;
        defer zlx_runtime.freeUseImports(allocator, &imports);
        for (plugin_host.modules.items) |mod| {
            if (!imports.contains(mod.name)) continue;
            if (mod.owner) |o| _ = used_owners.put(o, {}) catch {};
        }
    } else |_| {}
    var injected_link_flags: usize = 0;
    for (plugin_host.link_flags.items) |entry| {
        const owner = entry.owner orelse continue;
        if (!used_owners.contains(owner)) continue;
        const flag_copy = utils.dupe(u8, allocator, entry.flag);
        if (!containsString(ctx.extra_args.items, flag_copy)) {
            try ctx.extra_args.append(allocator, flag_copy);
            injected_link_flags += 1;
        } else {
            allocator.free(flag_copy);
        }
    }
    if (ctx.verbose and !ctx.quiet) {
        if (ctx.plugin_flags.items.len != 0) {
            std.debug.print("zlx: consumed {d} plugin CLI flag(s)\n", .{ctx.plugin_flags.items.len});
        }
        if (injected_link_flags != 0) {
            std.debug.print("zlx: activated {d} plugin link flag(s)\n", .{injected_link_flags});
        }
    }

    var zlx_session_started = false;
    if (plugin_host_ptr) |host| {
        host.clearDiagnostics();
        host.sessionBegin();
        zlx_session_started = true;
    }
    defer if (zlx_session_started) {
        plugin_host.sessionEnd();
        printPluginDiagnostics(allocator, &plugin_host);
    };

    var prebuilt_llvm_ir: std.ArrayList([]const u8) = .empty;
    defer prebuilt_llvm_ir.deinit(allocator);
    {
        var want_continue: c_int = 0;
        var ext_handler_fired = false;
        for (ctx.extra_args.items) |a| if (std.mem.eql(u8, a, "-c")) {
            want_continue = 1;
        };

        var write_idx: usize = 0;
        var i: usize = 0;
        while (i < ctx.input_files.items.len) : (i += 1) {
            const path = ctx.input_files.items[i];
            var matched: ?zlx_abi.FileExtensionHandler = null;
            for (plugin_host.file_extensions.items) |reg| {
                if (std.mem.endsWith(u8, path, reg.extension)) {
                    matched = reg.handler;
                    break;
                }
            }
            if (matched) |handler| {
                const in_z = allocator.dupeZ(u8, path) catch {
                    ctx.input_files.items[write_idx] = path;
                    write_idx += 1;
                    continue;
                };
                defer allocator.free(in_z);
                const out_z: ?[:0]u8 = if (ctx.output.len != 0) (allocator.dupeZ(u8, ctx.output) catch null) else null;
                defer if (out_z) |o| allocator.free(o);
                const req = zlx_abi.FileExtensionRequest{
                    .input_path = in_z.ptr,
                    .output_path = if (out_z) |o| o.ptr else null,
                    .want_continue = want_continue,
                };
                var result = zlx_abi.FileExtensionResult{ .continue_path = null, .llvm_ir_path = null };
                const rc = handler(&plugin_host.api, &req, &result);
                if (rc != 0) {
                    std.debug.print("zlx: file extension handler failed for {s} (rc={d})\n", .{ path, rc });
                    return 1;
                }
                ext_handler_fired = true;
                if (result.llvm_ir_path) |lp| {
                    const owned = allocator.dupe(u8, std.mem.span(lp)) catch {
                        ctx.input_files.items[write_idx] = path;
                        write_idx += 1;
                        continue;
                    };
                    prebuilt_llvm_ir.append(allocator, owned) catch {};
                    continue;
                }
                if (result.continue_path) |cp| {
                    const owned = allocator.dupe(u8, std.mem.span(cp)) catch {
                        ctx.input_files.items[write_idx] = path;
                        write_idx += 1;
                        continue;
                    };
                    ctx.input_files.items[write_idx] = owned;
                    write_idx += 1;
                }
                continue;
            }
            ctx.input_files.items[write_idx] = path;
            write_idx += 1;
        }
        ctx.input_files.shrinkRetainingCapacity(write_idx);
        if (ext_handler_fired and want_continue == 1) {
            var j: usize = 0;
            while (j < ctx.extra_args.items.len) {
                if (std.mem.eql(u8, ctx.extra_args.items[j], "-c")) {
                    _ = ctx.extra_args.orderedRemove(j);
                } else j += 1;
            }
            if (std.mem.eql(u8, ctx.output, "output.o")) ctx.output = "a.out";
        }
        if (ctx.input_files.items.len == 0 and prebuilt_llvm_ir.items.len > 0) {
            return compilePrebuiltLLVMIR(allocator, &ctx, prebuilt_llvm_ir.items) catch |err| blk: {
                std.debug.print("Error compiling pre-built LLVM IR: {}\n", .{err});
                break :blk 1;
            };
        }
        if (ctx.input_files.items.len == 0) return 0;
    }

    if (plugin_host.syntax_blocks.items.len != 0) {
        clearZlxSourceMaps(allocator);
        var total_expansions: usize = 0;
        for (ctx.input_files.items, 0..) |path, idx| {
            if (expandPathIfNeeded(path, idx, allocator)) |maybe| {
                if (maybe) |rewritten| {
                    ctx.input_files.items[idx] = rewritten.path;
                    total_expansions += rewritten.expansions;
                }
            } else |err| {
                std.debug.print("zlx: extension block expansion failed for {s}: {s}\n", .{ path, @errorName(err) });
                return 1;
            }
        }
        if (ctx.verbose and !ctx.quiet and total_expansions != 0) {
            std.debug.print("zlx: expanded {d} extension block(s)\n", .{total_expansions});
        }
    }

    const total_start = nanoTimestamp();
    var stats = report.CompilationStats{
        .parse_time_ns = 0,
        .parse_load_time_ns = 0,
        .parse_std_time_ns = 0,
        .parse_resolve_time_ns = 0,
        .parse_rewrite_time_ns = 0,
        .parse_merge_time_ns = 0,
        .codegen_time_ns = 0,
        .backend_time_ns = 0,
        .backend_prepare_time_ns = 0,
        .backend_split_time_ns = 0,
        .backend_wall_time_ns = 0,
        .backend_cpu_sum_time_ns = 0,
        .backend_units = 0,
        .backend_unit_max_time_ns = 0,
        .backend_unit_min_time_ns = 0,
        .link_time_ns = 0,
        .total_time_ns = 0,
        .lines_of_code = 0,
        .memory_peak_kb = 0,
        .llvm_link_version_major = build_options.llvm_version_major,
        .llc_version_major = 0,
        .opt_version_major = 0,
        .clang_version_major = 0,
        .lld_version_major = 0,
        .link_backend = .unknown,
    };

    const parse_start = nanoTimestamp();
    var parse_timing = ParseTiming{};
    var ast_root = parseMultiFile(&ctx, allocator, &parse_timing) catch {
        return 1;
    };
    emitUnknownDefineOverrideWarnings(&ctx);
    const parse_end = nanoTimestamp();
    stats.parse_time_ns = @intCast(parse_end - parse_start);
    stats.parse_load_time_ns = parse_timing.load_time_ns;
    stats.parse_std_time_ns = parse_timing.std_time_ns;
    stats.parse_resolve_time_ns = parse_timing.resolve_time_ns;
    stats.parse_rewrite_time_ns = parse_timing.rewrite_time_ns;
    stats.parse_merge_time_ns = parse_timing.merge_time_ns;

    defer ast_root.deinit();
    if (ctx.show_ast) {
        ast.printASTTree(ast_root.getRoot());
    }

    if (ctx.stats) {
        if (ctx.module_registrations.items.len > 0) {
            for (ctx.module_registrations.items) |entry| {
                stats.lines_of_code += entry.line_count;
            }
        } else {
            for (ctx.input_files.items) |input_file| {
                const input = read_file(input_file) catch continue;
                defer allocator.free(input);
                stats.lines_of_code += countLinesInText(input);
            }
        }
    }

    const semantic_file_path = if (ctx.input_files.items.len > 0) ctx.input_files.items[0] else "<input>";
    var semantic_function_file_paths = std.StringHashMap([]const u8).init(allocator);
    defer semantic_function_file_paths.deinit();
    var semantic_global_file_paths = std.StringHashMap([]const u8).init(allocator);
    defer semantic_global_file_paths.deinit();

    for (ctx.function_module_bindings.items) |binding| {
        if (modulePathByName(ctx.module_registrations.items, binding.module_name)) |module_path| {
            try semantic_function_file_paths.put(binding.symbol_name, module_path);
        }
    }
    for (ctx.global_module_bindings.items) |binding| {
        if (modulePathByName(ctx.module_registrations.items, binding.module_name)) |module_path| {
            try semantic_global_file_paths.put(binding.symbol_name, module_path);
        }
    }

    semantic.analyzeProgramWithSourceMaps(allocator, ast_root.getRoot(), semantic_file_path, &semantic_function_file_paths, &semantic_global_file_paths) catch |err| {
        const error_msg = switch (err) {
            error.SemanticFailed => "Semantic analysis failed.",
            error.OutOfMemory => "Out of memory during semantic analysis.",
        };
        std.debug.print("{s}\n", .{error_msg});
        return 1;
    };
    const codegen_start = nanoTimestamp();
    var code_generator = codegen.CodeGenerator.init(allocator) catch |err| {
        const error_msg = switch (err) {
            error.ModuleCreationFailed => "Failed to create LLVM module.",
            error.BuilderCreationFailed => "Failed to create LLVM builder.",
            error.OutOfMemory => "Out of memory during codegen initialization.",
            else => "Unknown codegen initialization error.",
        };
        std.debug.print("Error initializing codegen: {s}\n", .{error_msg});
        return 1;
    };

    code_generator.optimize_enabled = ctx.optimize;
    defer code_generator.deinit();

    for (ctx.module_registrations.items) |entry| {
        try code_generator.registerModule(entry.module_name, entry.module_path, entry.dependencies.items);
    }
    for (ctx.function_module_bindings.items) |entry| {
        try code_generator.registerFunctionModule(entry.symbol_name, entry.module_name);
    }
    for (ctx.global_module_bindings.items) |entry| {
        try code_generator.registerGlobalModule(entry.symbol_name, entry.module_name);
    }

    code_generator.generateCode(ast_root.getRoot()) catch |err| {
        if (err == error.TypeMismatch and code_generator.emitted_error) {
            return 1;
        }
        var owned_error_msg: ?[]u8 = null;
        const error_msg = switch (err) {
            error.FunctionCreationFailed => "Failed to create function.",
            error.TypeMismatch => blk: {
                if (code_generator.current_line > 0) {
                    const file_path = code_generator.module_manager.getCurrentModulePath();
                    const diag_msg = if (code_generator.current_token_text) |tok|
                        std.fmt.allocPrint(allocator, "Type mismatch near '{s}'", .{tok}) catch null
                    else
                        std.fmt.allocPrint(allocator, "Type mismatch in code generation", .{}) catch null;
                    if (diag_msg) |msg| {
                        defer allocator.free(msg);
                        diagnostics.printDiagnostic(allocator, .{
                            .file_path = file_path,
                            .line = code_generator.current_line,
                            .column = if (code_generator.current_column > 0) code_generator.current_column else 1,
                            .message = msg,
                            .severity = .Error,
                            .hint = "Check operand and assignment types",
                            .token_text = code_generator.current_token_text,
                        });
                        break :blk "Type mismatch in code generation.";
                    }
                }
                break :blk "Type mismatch in code generation.";
            },
            error.NullNotAllowedInNonPointerType => "Null can only be assigned to pointer types. Cannot assign null to non-pointer type.",
            error.UndefinedFunction => "Undefined function called.",
            error.UndefinedVariable => blk: {
                if (code_generator.current_line > 0) {
                    owned_error_msg = std.fmt.allocPrint(allocator, "Undefined variable used at line {d}.", .{code_generator.current_line}) catch null;
                    if (owned_error_msg) |msg| break :blk msg;
                }
                break :blk "Undefined variable used.";
            },
            error.UnsupportedOperation => blk: {
                if (code_generator.current_line > 0) {
                    const file_path = code_generator.module_manager.getCurrentModulePath();
                    const diag_msg = if (code_generator.current_token_text) |tok|
                        std.fmt.allocPrint(allocator, "Unsupported operator near '{s}'", .{tok}) catch null
                    else
                        std.fmt.allocPrint(allocator, "Unsupported operator used", .{}) catch null;
                    if (diag_msg) |msg| {
                        defer allocator.free(msg);
                        diagnostics.printDiagnostic(allocator, .{
                            .file_path = file_path,
                            .line = code_generator.current_line,
                            .column = if (code_generator.current_column > 0) code_generator.current_column else 1,
                            .message = msg,
                            .severity = .Error,
                            .hint = "Operator is not supported for these operand types",
                            .token_text = code_generator.current_token_text,
                        });
                        break :blk "Unsupported operator used";
                    }
                }
                break :blk "Unsupported operator used";
            },
            error.OutOfMemory => "Out of memory during code generation.",
            error.RedeclaredVariable => "Variable reinitialization",
            else => "Unknown code generation error.",
        };
        defer if (owned_error_msg) |msg| allocator.free(msg);
        std.debug.print("Error generating code: {s}\n", .{error_msg});
        return 1;
    };

    // Process pending template instantiations
    while (code_generator.pending_template_instantiations.items.len > 0) {
        const pending = try code_generator.pending_template_instantiations.toOwnedSlice(allocator);
        defer {
            for (pending) |*p| p.substitutions.deinit();
            allocator.free(pending);
        }

        for (pending) |inst| {
            const old_subs = code_generator.template_substitutions;
            code_generator.template_substitutions = inst.substitutions;
            // Body generation for instantiated functions (declareFunction was already called)
            code_generator.generateFunctionBody(inst.func_node) catch |err| {
                std.debug.print("Error generating template body: {}\n", .{err});
                return 1;
            };
            code_generator.template_substitutions = old_subs;
        }
    }

    if (!ctx.optimize or ctx.verify_ir) {
        code_generator.verifyModule() catch {
            std.debug.print("Error: generated LLVM IR failed verification\n", .{});
            return 1;
        };
    }

    const codegen_end = nanoTimestamp();
    stats.codegen_time_ns = @intCast(codegen_end - codegen_start);

    if (ctx.run_mode) {
        if (!interpreter.checkLLIAvailable(allocator)) {
            std.debug.print("Error: lli (LLVM interpreter) is not available.\n", .{});
            std.debug.print("Please ensure LLVM is installed and lli is in your PATH.\n", .{});
            return 1;
        }

        const run_nonce: u64 = @intCast(nanoTimestamp());
        const run_temp_base = std.fmt.allocPrint(allocator, "__zlang_run_temp_{d}_{d}", .{ nanoTimestamp(), run_nonce }) catch {
            std.debug.print("Error preparing temporary run file name\n", .{});
            return 1;
        };
        defer allocator.free(run_temp_base);

        const ir_file = code_generator.emitLLVMIR(run_temp_base, ctx.optimize) catch |err| {
            std.debug.print("Error emitting LLVM IR: {}\n", .{err});
            return 1;
        };
        defer {
            if (!ctx.keepll) {
                std.Io.Dir.cwd().deleteFile(process_io, ir_file) catch {};
            }
            allocator.free(ir_file);
        }

        if (ctx.verbose and !ctx.quiet) {
            std.debug.print("Running {s} with lli...\n", .{ir_file});
        }

        const exit_code = interpreter.runWithLLI(allocator, ir_file, ctx.program_args.items) catch |err| {
            std.debug.print("Error running with lli: {}\n", .{err});
            return 1;
        };

        if (ctx.stats) {
            stats.total_time_ns = @intCast(nanoTimestamp() - total_start);
            std.debug.print("\n", .{});
            report.printStats(stats);
        }
        return exit_code;
    } else {
        var backend_timing = codegen.CodeGenerator.BackendTiming{};
        const backend_prepare_start = nanoTimestamp();
        var backend_units: std.ArrayList(BackendUnit) = .empty;
        defer backend_units.deinit(allocator);

        const backend_nonce: u64 = @intCast(nanoTimestamp());
        const backend_temp_base = std.fmt.allocPrint(allocator, "{s}.tmp.{d}.{d}", .{ ctx.output, nanoTimestamp(), backend_nonce }) catch {
            std.debug.print("Error preparing backend temporary prefix\n", .{});
            return 1;
        };
        defer allocator.free(backend_temp_base);

        const primary_bc_file = std.fmt.allocPrint(allocator, "{s}.bc", .{backend_temp_base}) catch {
            std.debug.print("Error preparing backend bitcode path\n", .{});
            return 1;
        };
        errdefer allocator.free(primary_bc_file);

        code_generator.writeBitcodeToFile(primary_bc_file) catch |err| {
            std.debug.print("Error writing LLVM bitcode: {}\n", .{err});
            return 1;
        };

        if (ctx.keepll) {
            const kept_ll_file = std.fmt.allocPrint(allocator, "{s}.ll", .{ctx.output}) catch {
                std.debug.print("Error preparing kept LLVM IR path\n", .{});
                return 1;
            };
            defer allocator.free(kept_ll_file);
            code_generator.writeToFile(kept_ll_file) catch |err| {
                std.debug.print("Error writing LLVM IR: {}\n", .{err});
                return 1;
            };
        }

        var split_unit_paths: std.ArrayList([]const u8) = .empty;
        defer split_unit_paths.deinit(allocator);

        const split_prefix = std.fmt.allocPrint(allocator, "{s}.unit.bc", .{backend_temp_base}) catch {
            std.debug.print("Error preparing split prefix\n", .{});
            return 1;
        };
        defer allocator.free(split_prefix);

        const split_start = nanoTimestamp();
        const split_success = code_generator.splitBitcodeIntoUnits(primary_bc_file, split_prefix, backendSplitUnitCount(ctx.max_threads), ctx.arch, &split_unit_paths) catch false;
        const split_end = nanoTimestamp();
        stats.backend_split_time_ns = @intCast(split_end - split_start);

        if (split_success) {
            for (split_unit_paths.items) |unit_bc| {
                const unit_obj = std.fmt.allocPrint(allocator, "{s}.o", .{unit_bc}) catch {
                    std.debug.print("Error preparing split object path\n", .{});
                    return 1;
                };
                try backend_units.append(allocator, .{
                    .ir_file = unit_bc,
                    .obj_file = unit_obj,
                    .keep_ir = false,
                });
            }
            std.Io.Dir.cwd().deleteFile(process_io, primary_bc_file) catch {};
            allocator.free(primary_bc_file);
        } else {
            const primary_obj_file = std.fmt.allocPrint(allocator, "{s}.o", .{backend_temp_base}) catch {
                std.debug.print("Error preparing backend object path\n", .{});
                return 1;
            };
            try backend_units.append(allocator, .{
                .ir_file = primary_bc_file,
                .obj_file = primary_obj_file,
                .keep_ir = false,
            });
        }

        defer {
            cleanupBackendUnits(backend_units.items);
            for (backend_units.items) |unit| {
                allocator.free(unit.ir_file);
                allocator.free(unit.obj_file);
            }
        }

        var generated_objects: std.ArrayList([]const u8) = .empty;
        defer generated_objects.deinit(allocator);
        for (backend_units.items) |unit| {
            try generated_objects.append(allocator, unit.obj_file);
        }

        var object_tasks = try allocator.alloc(BackendObjectTask, generated_objects.items.len);
        defer allocator.free(object_tasks);
        const llc_task_threads: usize = if (generated_objects.items.len > 1) 1 else ctx.max_threads;
        for (generated_objects.items, 0..) |obj_file, i| {
            object_tasks[i] = .{
                .cg = &code_generator,
                .ir_file = backend_units.items[i].ir_file,
                .obj_file = obj_file,
                .arch = ctx.arch,
                .optimize = ctx.optimize,
                .max_threads = llc_task_threads,
            };
        }
        stats.backend_prepare_time_ns = @intCast(nanoTimestamp() - backend_prepare_start);

        const backend_compile_start = nanoTimestamp();
        backend_timing.llc_time_ns = compileBackendObjects(object_tasks, ctx.max_threads, allocator) catch |err| {
            std.debug.print("Error compiling IR to object: {}\n", .{err});
            return 1;
        };
        const backend_compile_end = nanoTimestamp();

        var unit_max: u64 = 0;
        var unit_min: u64 = 0;
        if (object_tasks.len > 0) {
            unit_min = object_tasks[0].llc_time_ns;
            for (object_tasks) |task| {
                if (task.llc_time_ns > unit_max) unit_max = task.llc_time_ns;
                if (task.llc_time_ns < unit_min) unit_min = task.llc_time_ns;
            }
        }

        code_generator.linkObjectFilesToExecutable(ctx.output, ctx.arch, generated_objects.items, ctx.link_objects.items, ctx.extra_args.items, ctx.max_threads, &backend_timing) catch |err| {
            std.debug.print("Error linking executable: {}\n", .{err});
            return 1;
        };
        stats.backend_time_ns = backend_timing.opt_time_ns + backend_timing.llc_time_ns;
        stats.backend_wall_time_ns = @intCast(backend_compile_end - backend_compile_start);
        stats.backend_cpu_sum_time_ns = backend_timing.llc_time_ns;
        stats.backend_units = generated_objects.items.len;
        stats.backend_unit_max_time_ns = unit_max;
        stats.backend_unit_min_time_ns = unit_min;
        stats.link_time_ns = backend_timing.link_time_ns;
        stats.llc_version_major = backend_timing.llc_version_major;
        stats.opt_version_major = backend_timing.opt_version_major;
        stats.clang_version_major = backend_timing.clang_version_major;
        stats.lld_version_major = backend_timing.lld_version_major;
        stats.link_backend = backend_timing.link_backend;
        stats.total_time_ns = @intCast(nanoTimestamp() - total_start);

        if (ctx.verbose and !ctx.quiet) {
            std.debug.print("Executable compiled to {s}\n", .{ctx.output});
        }

        if (ctx.stats) {
            stats.memory_peak_kb = report.readMemoryPeakKb();
            std.debug.print("\n", .{});
            report.printStats(stats);
        }

        return 0;
    }
}
