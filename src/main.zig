const std = @import("std");
const builtin = @import("builtin");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const parser = @import("parser/parser.zig");
const ast = @import("parser/ast.zig");
const codegen = @import("codegen/llvm.zig");
const utils = @import("codegen/utils.zig");
const diagnostics = @import("diagnostics.zig");
const help = @import("help.zig");
const interpreter = @import("interpreter.zig");
const preprocessor = @import("preprocessor/preprocessor.zig");

const allocator = std.heap.page_allocator;

const ModuleInfo = struct {
    path: []const u8,
    ast: *ast.Node,
    dependencies: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator, path: []const u8, ast_node: *ast.Node) ModuleInfo {
        return ModuleInfo{
            .path = utils.dupe(u8, alloc, path),
            .ast = ast_node,
            .dependencies = std.ArrayList([]const u8){},
        };
    }

    pub fn deinit(self: *ModuleInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.path);
        self.dependencies.deinit(alloc);
    }
};

pub const CompilationStats = struct {
    parse_time_ns: u64,
    codegen_time_ns: u64,
    link_time_ns: u64,
    total_time_ns: u64,
    lines_of_code: usize,
    memory_peak_kb: usize,
};

fn printStats(stats: CompilationStats) void {
    const parse_ms = @as(f64, @floatFromInt(stats.parse_time_ns)) / 1_000_000.0;
    const codegen_ms = @as(f64, @floatFromInt(stats.codegen_time_ns)) / 1_000_000.0;
    const link_ms = @as(f64, @floatFromInt(stats.link_time_ns)) / 1_000_000.0;
    const total_ms = @as(f64, @floatFromInt(stats.total_time_ns)) / 1_000_000.0;

    std.debug.print("Compilation Statistics:\n", .{});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    std.debug.print("  Parse time:      {d:.2} ms\n", .{parse_ms});
    std.debug.print("  Codegen time:    {d:.2} ms\n", .{codegen_ms});
    std.debug.print("  Link time:       {d:.2} ms\n", .{link_ms});
    std.debug.print("  Total time:      {d:.2} ms\n", .{total_ms});
    std.debug.print("  Lines of code:   {d}\n", .{stats.lines_of_code});
    std.debug.print("  Memory peak:     {d} KB\n", .{stats.memory_peak_kb});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
}

pub const Context = struct {
    input_files: std.ArrayList([]const u8),
    link_objects: std.ArrayList([]const u8),
    extra_args: std.ArrayList([]const u8),
    program_args: std.ArrayList([]const u8),
    output: []const u8,
    arch: []const u8,
    keepll: bool,
    show_ast: bool,
    optimize: bool,
    verbose: bool,
    quiet: bool,
    stats: bool,
    run_mode: bool,
    brainfuck_mode: bool,
    bf_cell_size: i32,

    pub fn init() Context {
        return Context{
            .input_files = std.ArrayList([]const u8){},
            .link_objects = std.ArrayList([]const u8){},
            .extra_args = std.ArrayList([]const u8){},
            .program_args = std.ArrayList([]const u8){},
            .output = consts.DEFAULT_OUTPUT_NAME,
            .arch = "",
            .keepll = false,
            .show_ast = false,
            .optimize = false,
            .verbose = false,
            .quiet = false,
            .stats = false,
            .run_mode = false,
            .brainfuck_mode = false,
            .bf_cell_size = 8,
        };
    }

    pub fn deinit(self: *Context, alloc: std.mem.Allocator) void {
        self.input_files.deinit(alloc);
        self.link_objects.deinit(alloc);
        self.extra_args.deinit(alloc);
        self.program_args.deinit(alloc);
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
                else => {},
            }
        },
    }
}

fn getStdlibPath(alloc: std.mem.Allocator) ![]const u8 {
    if (std.process.getEnvVarOwned(alloc, "ZSTDPATH")) |zstdpath| {
        return zstdpath;
    } else |_| {
        const exe_path = try std.fs.selfExePathAlloc(alloc);
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
    has_zstdpath.* = if (std.process.getEnvVarOwned(alloc, "ZSTDPATH")) |path| blk: {
        alloc.free(path);
        break :blk true;
    } else |_| false;

    const stdlib_path = getStdlibPath(alloc) catch return null;
    defer alloc.free(stdlib_path);

    const module_file = std.fmt.allocPrint(alloc, "{s}.zl", .{module_part}) catch return null;
    defer alloc.free(module_file);

    const full_path = std.fs.path.join(alloc, &[_][]const u8{ stdlib_path, module_file }) catch return null;

    const file = std.fs.cwd().openFile(full_path, .{}) catch {
        alloc.free(full_path);
        return null;
    };
    file.close();

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
    const file = std.fs.cwd().openFile(full_path, .{}) catch return null;
    file.close();
    return utils.dupe(u8, alloc, full_path);
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

fn parseModuleFile(file_path: []const u8, arena: std.mem.Allocator, backing_alloc: std.mem.Allocator) !ModuleInfo {
    const input = read_file(file_path) catch |err| {
        std.debug.print("Error reading file {s}: {}\n", .{ file_path, err });
        return err;
    };

    const preprocessed = preprocessor.preprocess(arena, input) catch |err| {
        const msg = switch (err) {
            errors.PreprocessError.InvalidDirective => "Invalid preprocessor directive",
            errors.PreprocessError.InvalidDefine => "Invalid #define",
            errors.PreprocessError.ExpansionLimit => "Macro expansion limit reached",
            errors.PreprocessError.OutOfMemory => "Out of memory during preprocessing",
        };
        diagnostics.printDiagnostic(backing_alloc, .{
            .file_path = file_path,
            .line = 1,
            .column = 1,
            .message = msg,
            .severity = .Error,
        });
        return err;
    };

    const ast_root = parser.parse(arena, preprocessed) catch |err| {
        const parse_errors = parser.getParseErrors();
        if (parse_errors.len > 0) {
            for (parse_errors) |parse_err| {
                diagnostics.printDiagnostic(backing_alloc, .{
                    .file_path = file_path,
                    .line = parse_err.line,
                    .column = parse_err.column,
                    .message = parse_err.message,
                    .severity = .Error,
                });
            }
        } else {
            const loc = parser.lastParseErrorLocation();
            if (loc) |l| {
                diagnostics.printDiagnostic(backing_alloc, .{
                    .file_path = file_path,
                    .line = l.line,
                    .column = l.column,
                    .message = "Parse error",
                    .severity = .Error,
                });
            } else {
                std.debug.print("Error parsing file {s}: {}\n", .{ file_path, err });
            }
        }

        tryFindMoreErrors(backing_alloc, file_path, input);
        return err;
    };
    if (ast_root) |root| {
        var module = ModuleInfo.init(backing_alloc, file_path, root);
        collectUseStatements(root, &module.dependencies);
        return module;
    }
    return error.ParseFailed;
}

fn parseMultiFile(ctx: *Context, alloc: std.mem.Allocator) !ast.ArenaAST {
    var arena_ast = ast.ArenaAST.init(alloc);
    const arena = arena_ast.allocator();

    var modules = std.ArrayList(ModuleInfo){};
    defer {
        for (modules.items) |*module| {
            module.deinit(alloc);
        }
        modules.deinit(alloc);
    }

    var loaded_modules = std.StringHashMap(void).init(alloc);
    defer loaded_modules.deinit();

    var to_process = std.ArrayList([]const u8){};
    defer to_process.deinit(alloc);

    for (ctx.input_files.items) |input_file| {
        try to_process.append(alloc, input_file);
    }

    var i: usize = 0;
    while (i < to_process.items.len) : (i += 1) {
        const current_file = to_process.items[i];

        if (loaded_modules.contains(current_file)) {
            continue;
        }

        const module = try parseModuleFile(current_file, arena, alloc);
        try loaded_modules.put(utils.dupe(u8, alloc, current_file), {});

        for (module.dependencies.items) |dep| {
            if (try resolveModulePath(current_file, dep, alloc)) |dep_path| {
                if (!loaded_modules.contains(dep_path)) {
                    try to_process.append(alloc, dep_path);
                }
            } else {
                std.debug.print("\x1b[31mError:\x1b[0m Cannot resolve module '\x1b[33m{s}\x1b[0m' imported from {s}\n", .{ dep, current_file });
                arena_ast.deinit();
                return error.ModuleNotFound;
            }
        }

        try modules.append(alloc, module);
    }
    const merged_program_data = ast.NodeData{
        .program = ast.Program{
            .functions = std.ArrayList(*ast.Node){},
            .globals = std.ArrayList(*ast.Node){},
        },
    };

    const merged_program = ast.Node.create(arena, merged_program_data);
    var module_name_map = std.StringHashMap([]const u8).init(alloc);
    defer module_name_map.deinit();
    var module_deps = std.StringHashMap(std.ArrayList([]const u8)).init(alloc);
    defer {
        var it = module_deps.iterator();
        while (it.next()) |entry| entry.value_ptr.deinit(alloc);
        module_deps.deinit();
    }
    for (modules.items) |module| {
        const base = std.fs.path.basename(module.path);
        const name_no_ext = if (std.mem.endsWith(u8, base, ".zl")) base[0 .. base.len - 3] else base;
        try module_name_map.put(module.path, utils.dupe(u8, alloc, name_no_ext));
        var deps = std.ArrayList([]const u8){};
        for (module.dependencies.items) |d| try deps.append(alloc, utils.dupe(u8, alloc, d));
        try module_deps.put(utils.dupe(u8, alloc, name_no_ext), deps);
    }
    var func_to_module = std.StringHashMap([]const u8).init(alloc);
    defer func_to_module.deinit();
    for (modules.items) |module| {
        switch (module.ast.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    const owner = module_name_map.get(module.path).?;
                    if (func.data == .function) {
                        try func_to_module.put(func.data.function.name, owner);
                    } else if (func.data == .c_function_decl) {
                        try func_to_module.put(func.data.c_function_decl.name, owner);
                    }
                }
            },
            else => {},
        }
    }

    for (modules.items) |module| {
        switch (module.ast.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    if (func.data != .use_stmt) {
                        try merged_program.data.program.functions.append(arena, func);
                    }
                }
                for (prog.globals.items) |glob| {
                    try merged_program.data.program.globals.append(arena, glob);
                }
            },
            else => {
                try merged_program.data.program.functions.append(arena, module.ast);
            },
        }
    }

    arena_ast.setRoot(merged_program);
    return arena_ast;
}

pub fn read_file(file_name: []const u8) anyerror![]const u8 {
    const cwd = std.fs.cwd();
    cwd.access(file_name, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return errors.ReadFileError.FileNotFound;
        }
        return errors.ReadFileError.AccessDenied;
    };

    const file_stat = try cwd.statFile(file_name);
    if (file_stat.kind == .file) {
        const file = cwd.openFile(file_name, .{ .mode = .read_only }) catch {
            return errors.ReadFileError.AccessDenied;
        };
        defer file.close();
        const buffer = utils.alloc(u8, allocator, consts.MAX_BUFF_SIZE);
        const bytes_read = try file.readAll(buffer);
        return buffer[0..bytes_read];
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
                if (std.mem.eql(u8, flag, "-b")) {
                    context.brainfuck_mode = true;
                    context.bf_cell_size = 8;
                } else if (std.mem.eql(u8, flag, "-b8")) {
                    context.brainfuck_mode = true;
                    context.bf_cell_size = 8;
                } else if (std.mem.eql(u8, flag, "-b16")) {
                    context.brainfuck_mode = true;
                    context.bf_cell_size = 16;
                } else if (std.mem.eql(u8, flag, "-b32")) {
                    context.brainfuck_mode = true;
                    context.bf_cell_size = 32;
                } else if (std.mem.eql(u8, flag, "-b64")) {
                    context.brainfuck_mode = true;
                    context.bf_cell_size = 64;
                } else if (std.mem.eql(u8, flag, "-keepll")) {
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
                } else if (std.mem.eql(u8, flag, "-o")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoOutputPath;
                    context.output = args[i];
                } else if (std.mem.eql(u8, flag, "-arch")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoArch;
                    context.arch = args[i];
                } else if (std.mem.eql(u8, flag, "-link")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.InvalidArgument;
                    try context.link_objects.append(allocator, args[i]);
                } else if (std.mem.eql(u8, flag, "-c")) {
                    try context.extra_args.append(allocator, flag);
                    context.output = "output.o";
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
                } else if (std.mem.eql(u8, flag, "-help") or std.mem.eql(u8, flag, "--help")) {
                    return errors.CLIError.NoHelp;
                } else {
                    try context.extra_args.append(allocator, flag);
                }
            },
            else => {
                const arg = args[i];
                if (std.mem.endsWith(u8, arg, ".zl")) {
                    try context.input_files.append(allocator, arg);
                } else if (std.mem.endsWith(u8, arg, ".b") or std.mem.endsWith(u8, arg, ".bf")) {
                    try context.input_files.append(allocator, arg);
                } else {
                    const stat = std.fs.cwd().statFile(arg) catch |err| {
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
    return if (context.input_files.items.len == 0)
        errors.CLIError.NoInputPath
    else
        context;
}

fn compileBrainfuck(ctx: *Context, alloc: std.mem.Allocator) !u8 {
    const bf = @import("codegen/bf.zig");
    const c_bindings = @import("codegen/c_bindings.zig");
    const c = c_bindings.c;

    const input_file = ctx.input_files.items[0];
    const bf_code = read_file(input_file) catch |err| {
        std.debug.print("Error reading file {s}: {}\n", .{ input_file, err });
        return 1;
    };

    var code_generator = codegen.CodeGenerator.init(alloc) catch |err| {
        const error_msg = switch (err) {
            error.ModuleCreationFailed => "Failed to create LLVM module.",
            error.BuilderCreationFailed => "Failed to create LLVM builder.",
            error.OutOfMemory => "Out of memory during codegen initialization.",
            else => "Unknown codegen initialization error.",
        };
        std.debug.print("Error initializing codegen: {s}\n", .{error_msg});
        return 1;
    };
    defer code_generator.deinit();

    const main_func_type = c.LLVMFunctionType(
        c.LLVMInt32TypeInContext(code_generator.context),
        null,
        0,
        0,
    );
    const main_func = c.LLVMAddFunction(code_generator.module, "main", main_func_type);
    code_generator.current_function = main_func;

    const entry_block = c.LLVMAppendBasicBlockInContext(
        code_generator.context,
        main_func,
        "entry",
    );
    c.LLVMPositionBuilderAtEnd(code_generator.builder, entry_block);

    const bf_context_str = try std.fmt.allocPrint(alloc, "?cell_size {d}?\n?len 30000?\n{s}", .{ ctx.bf_cell_size, bf_code });
    defer alloc.free(bf_context_str);

    const bf_ast = ast.Brainfuck{ .code = bf_context_str };
    _ = bf.generateBrainfuck(&code_generator, bf_ast, ctx.optimize) catch |err| {
        std.debug.print("Error generating brainfuck code: {}\n", .{err});
        return 1;
    };

    const ret_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(code_generator.context), 0, 0);
    _ = c.LLVMBuildRet(code_generator.builder, ret_val);

    code_generator.compileToExecutable(ctx.output, ctx.arch, ctx.link_objects.items, ctx.keepll, ctx.optimize, ctx.extra_args.items) catch |err| {
        std.debug.print("Error compiling to executable: {}\n", .{err});
        return 1;
    };

    if (ctx.verbose and !ctx.quiet) {
        std.debug.print("Brainfuck executable compiled to {s}\n", .{ctx.output});
    }
    return 0;
}

fn collectZlFilesFromDir(alloc: std.mem.Allocator, dir_path: []const u8, files_list: *std.ArrayList([]const u8)) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(alloc);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".zl")) {
            const full_path = try std.fs.path.join(alloc, &[_][]const u8{ dir_path, entry.path });
            defer alloc.free(full_path);
            const path_copy = utils.dupe(u8, alloc, full_path);
            try files_list.append(alloc, path_copy);
        }
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
    var out = std.ArrayList(u8){};
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
    var stmts = std.ArrayList([]const u8){};
    var buf = std.ArrayList(u8){};
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
        var tmp = std.ArrayList(u8){};
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
        var tmp = std.ArrayList(u8){};
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
    var params_buf = std.ArrayList([]const u8){};
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

    var output = std.ArrayList(u8){};
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
    const cleaned = try stripCommentsAndPreproc(a, header_src);
    var stmts = try collectStatements(a, cleaned);
    defer stmts.deinit(a);
    var file = try std.fs.cwd().createFile(out_path, .{ .truncate = true });
    defer file.close();
    const header_comment = try std.fmt.allocPrint(a, "// Auto-generated by zlang wrap from: {s}\n", .{header_path});
    try file.writeAll(header_comment);
    for (stmts.items) |stmt| {
        const maybe_wrapper_str = parseAndWriteWrapper(a, stmt) catch |e| {
            switch (e) {
                error.OutOfMemory => return e,
            }
        };
        if (maybe_wrapper_str) |wrapper_str| {
            try file.writeAll(wrapper_str);
            a.free(wrapper_str);
        }
    }
}
pub fn main() !u8 {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        help.printHelp();
        return 1;
    }
    if (std.mem.eql(u8, args[1], "help")) {
        if (args.len > 2 and std.mem.eql(u8, args[2], "syntax")) {
            help.printHelpSyntax();
        } else {
            help.printHelp();
        }
        return 0;
    }
    if (std.mem.eql(u8, args[1], "wrap")) {
        if (args.len < 5) {
            std.debug.print("Usage: zlang wrap <file.h> -o <file.zl>\n", .{});
            return 1;
        }
        const header_path = args[2];
        var out_path: ?[]const u8 = null;
        var i: usize = 3;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "-o")) {
                if (i + 1 >= args.len) {
                    std.debug.print("Error: missing output after -o\n", .{});
                    return 1;
                }
                out_path = args[i + 1];
                break;
            }
        }
        if (out_path == null) {
            std.debug.print("Usage: zlang wrap <file.h> -o <file.zl>\n", .{});
            return 1;
        }
        generateWrapperFromHeader(allocator, header_path, out_path.?) catch |err| {
            std.debug.print("Error generating wrapper for {s}: {}\n", .{ header_path, err });
            return 1;
        };
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

    const total_start = std.time.nanoTimestamp();
    var stats = CompilationStats{
        .parse_time_ns = 0,
        .codegen_time_ns = 0,
        .link_time_ns = 0,
        .total_time_ns = 0,
        .lines_of_code = 0,
        .memory_peak_kb = 0,
    };

    if (ctx.brainfuck_mode) {
        if (ctx.input_files.items.len == 0) {
            std.debug.print("Error: No input file specified for brainfuck mode\n", .{});
            return 1;
        }
        const input_file = ctx.input_files.items[0];
        const has_valid_ext = std.mem.endsWith(u8, input_file, ".b") or std.mem.endsWith(u8, input_file, ".bf");
        if (!has_valid_ext) {
            std.debug.print("Error: Brainfuck mode requires input file with .b or .bf extension\n", .{});
            return 1;
        }
        const result = compileBrainfuck(&ctx, allocator);
        if (ctx.stats) {
            stats.total_time_ns = @intCast(std.time.nanoTimestamp() - total_start);
            std.debug.print("\n", .{});
            printStats(stats);
        }
        return result;
    }

    const parse_start = std.time.nanoTimestamp();
    var ast_root = parseMultiFile(&ctx, allocator) catch {
        return 1;
    };
    const parse_end = std.time.nanoTimestamp();
    stats.parse_time_ns = @intCast(parse_end - parse_start);

    defer ast_root.deinit();
    if (ctx.show_ast) {
        ast.printASTTree(ast_root.getRoot());
    }

    if (ctx.stats) {
        for (ctx.input_files.items) |input_file| {
            const input = read_file(input_file) catch continue;
            var line_count: usize = 0;
            var it = std.mem.splitScalar(u8, input, '\n');
            while (it.next()) |_| {
                line_count += 1;
            }
            stats.lines_of_code += line_count;
        }
    }

    const codegen_start = std.time.nanoTimestamp();
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

    defer code_generator.deinit();
    {
        for (ctx.input_files.items) |input_file| {
            const input = read_file(input_file) catch continue;
            var temp_arena = ast.ArenaAST.init(allocator);
            defer temp_arena.deinit();
            const preprocessed = preprocessor.preprocess(temp_arena.allocator(), input) catch continue;
            const temp_root = parser.parse(temp_arena.allocator(), preprocessed) catch continue;
            if (temp_root) |root| {
                var deps = std.ArrayList([]const u8){};
                defer deps.deinit(allocator);
                collectUseStatements(root, &deps);
                const base = std.fs.path.basename(input_file);
                const name_no_ext = if (std.mem.endsWith(u8, base, ".zl")) base[0 .. base.len - 3] else base;
                var dep_slices = std.ArrayList([]const u8){};
                defer dep_slices.deinit(allocator);
                for (deps.items) |d| dep_slices.append(allocator, d) catch {};
                code_generator.registerModule(name_no_ext, input_file, dep_slices.items) catch {};
                switch (root.data) {
                    .program => |prog| {
                        for (prog.functions.items) |fn_node| {
                            if (fn_node.data == .function) {
                                code_generator.registerFunctionModule(fn_node.data.function.name, name_no_ext) catch {};
                            } else if (fn_node.data == .c_function_decl) {
                                code_generator.registerFunctionModule(fn_node.data.c_function_decl.name, name_no_ext) catch {};
                            }
                        }
                    },
                    else => {},
                }
            }
        }
    }
    code_generator.generateCode(ast_root.getRoot()) catch |err| {
        const error_msg = switch (err) {
            error.FunctionCreationFailed => "Failed to create function.",
            error.TypeMismatch => "Type mismatch in code generation.",
            error.NullNotAllowedInNonPointerType => "Null can only be assigned to pointer types. Cannot assign null to non-pointer type.",
            error.UndefinedFunction => "Undefined function called.",
            error.UndefinedVariable => "Undefined variable used.",
            error.UnsupportedOperation => "Unsupported operator used",
            error.OutOfMemory => "Out of memory during code generation.",
            error.RedeclaredVariable => "Variable reinitialization",
            else => "Unknown code generation error.",
        };
        std.debug.print("Error generating code: {s}\n", .{error_msg});
        return 1;
    };
    const codegen_end = std.time.nanoTimestamp();
    stats.codegen_time_ns = @intCast(codegen_end - codegen_start);

    if (ctx.run_mode) {
        if (!interpreter.checkLLIAvailable(allocator)) {
            std.debug.print("Error: lli (LLVM interpreter) is not available.\n", .{});
            std.debug.print("Please ensure LLVM is installed and lli is in your PATH.\n", .{});
            return 1;
        }

        const ir_file = code_generator.emitLLVMIR("__zlang_run_temp", ctx.optimize) catch |err| {
            std.debug.print("Error emitting LLVM IR: {}\n", .{err});
            return 1;
        };
        defer {
            if (!ctx.keepll) {
                std.fs.cwd().deleteFile(ir_file) catch {};
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
            stats.total_time_ns = @intCast(std.time.nanoTimestamp() - total_start);
            std.debug.print("\n", .{});
            printStats(stats);
        }
        return exit_code;
    } else {
        const link_start = std.time.nanoTimestamp();
        code_generator.compileToExecutable(ctx.output, ctx.arch, ctx.link_objects.items, ctx.keepll, ctx.optimize, ctx.extra_args.items) catch |err| {
            std.debug.print("Error compiling to executable: {}\n", .{err});
            return 1;
        };
        const link_end = std.time.nanoTimestamp();
        stats.link_time_ns = @intCast(link_end - link_start);
        stats.total_time_ns = @intCast(std.time.nanoTimestamp() - total_start);

        if (ctx.verbose and !ctx.quiet) {
            std.debug.print("Executable compiled to {s}\n", .{ctx.output});
        }

        if (ctx.stats) {
            if (builtin.os.tag == .linux) {
                const stat_file = "/proc/self/status";
                const file = std.fs.cwd().openFile(stat_file, .{}) catch null;
                if (file) |f| {
                    defer f.close();
                    var buffer: [4096]u8 = undefined;
                    const bytes_read = f.readAll(&buffer) catch 0;
                    const content = buffer[0..bytes_read];
                    var lines = std.mem.splitScalar(u8, content, '\n');
                    while (lines.next()) |line| {
                        if (std.mem.startsWith(u8, line, "VmPeak:")) {
                            var it = std.mem.splitScalar(u8, line, ' ');
                            _ = it.next();
                            if (it.next()) |kb_str| {
                                stats.memory_peak_kb = std.fmt.parseInt(usize, kb_str, 10) catch 0;
                            }
                            break;
                        }
                    }
                }
            }
            std.debug.print("\n", .{});
            printStats(stats);
        }

        return 0;
    }
}
