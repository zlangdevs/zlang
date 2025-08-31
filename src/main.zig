const std = @import("std");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const parser = @import("parser/parser.zig");
const ast = @import("parser/ast.zig");
const codegen = @import("codegen/llvm.zig");

const allocator = std.heap.page_allocator;

const ModuleInfo = struct {
    path: []const u8,
    ast: *ast.Node,
    dependencies: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator, path: []const u8, ast_node: *ast.Node) ModuleInfo {
        return ModuleInfo{
            .path = alloc.dupe(u8, path) catch unreachable,
            .ast = ast_node,
            .dependencies = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *ModuleInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.path);
        self.dependencies.deinit();
    }
};

pub const Context = struct {
    input_files: std.ArrayList([]const u8),
    output: []const u8,
    arch: []const u8,
    keepll: bool,
    show_ast: bool,
    optimize: bool,
    verbose: bool,
    link_objects: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator) Context {
        return Context{
            .input_files = std.ArrayList([]const u8).init(alloc),
            .output = consts.DEFAULT_OUTPUT_NAME,
            .arch = "",
            .keepll = false,
            .show_ast = false,
            .optimize = false,
            .verbose = false,
            .link_objects = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Context) void {
        self.input_files.deinit();
        self.link_objects.deinit();
    }

    pub fn print(self: *const Context) void {
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
        },
        .function => |func| {
            for (func.body.items) |stmt| {
                collectUseStatements(stmt, dependencies);
            }
        },
        .use_stmt => |use_stmt| {
            dependencies.append(use_stmt.module_path) catch {};
        },
        else => {
            switch (node.data) {
                .assignment => |as| collectUseStatements(as.value, dependencies),
                .var_decl => |decl| if (decl.initializer) |init| collectUseStatements(init, dependencies),
                .function_call => |call| for (call.args.items) |arg| collectUseStatements(arg, dependencies),
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

fn resolveModulePath(base_path: []const u8, module_name: []const u8, alloc: std.mem.Allocator) ?[]const u8 {
    if (std.mem.endsWith(u8, module_name, ".zl")) {
        return alloc.dupe(u8, module_name);
    }
    const base_dir = std.fs.path.dirname(base_path) orelse ".";
    const module_file = try std.fmt.allocPrint(alloc, "{s}.zl", .{module_name});
    defer alloc.free(module_file);
    const full_path = try std.fs.path.join(alloc, &[_][]const u8{ base_dir, module_file });
    defer alloc.free(full_path);
    const file = std.fs.cwd().openFile(full_path, .{}) catch return null;
    file.close();
    return alloc.dupe(u8, full_path);
}

fn parseMultiFile(ctx: *Context, alloc: std.mem.Allocator) !*ast.Node {
    var modules = std.ArrayList(ModuleInfo).init(alloc);
    defer {
        for (modules.items) |*module| {
            module.deinit(alloc);
        }
        modules.deinit();
    }
    for (ctx.input_files.items) |input_file| {
        const input = read_file(input_file) catch |err| {
            std.debug.print("Error reading file {s}: {}\n", .{ input_file, err });
            return err;
        };
        const ast_root = parser.parse(alloc, input) catch |err| {
            std.debug.print("Error parsing file {s}: {}\n", .{ input_file, err });
            return err;
        };
        if (ast_root) |root| {
            var module = ModuleInfo.init(alloc, input_file, root);
            collectUseStatements(root, &module.dependencies);
            try modules.append(module);
        }
    }
    const merged_program_data = ast.NodeData{
        .program = ast.Program{
            .functions = std.ArrayList(*ast.Node).init(alloc),
        },
    };

    const merged_program = try ast.Node.create(alloc, merged_program_data);
    for (modules.items) |module| {
        switch (module.ast.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    if (func.data != .use_stmt) {
                        try merged_program.data.program.functions.append(func);
                    }
                }
            },
            else => {
                try merged_program.data.program.functions.append(module.ast);
            },
        }
    }

    return merged_program;
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
        const buffer = try file.readToEndAlloc(allocator, consts.MAX_BUFF_SIZE);
        return buffer;
    }

    return errors.ReadFileError.InvalidPath;
}

fn parseArgs(args: [][:0]u8) anyerror!Context {
    var context = Context.init(allocator);
    errdefer context.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
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
                    try context.link_objects.append(args[i]);
                } else if (std.mem.eql(u8, flag, "-help")) {
                    return errors.CLIError.NoHelp;
                } else {
                    return errors.CLIError.InvalidArgument;
                }
            },
            else => {
                const arg = args[i];
                if (std.mem.endsWith(u8, arg, ".zl")) {
                    try context.input_files.append(arg);
                } else {
                    const stat = std.fs.cwd().statFile(arg) catch |err| {
                        if (err == error.FileNotFound) {
                            try context.link_objects.append(arg);
                            continue;
                        }
                        return err;
                    };

                    if (stat.kind == .directory) {
                        try collectZlFilesFromDir(allocator, arg, &context.input_files);
                    } else {
                        try context.link_objects.append(arg);
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

fn collectZlFilesFromDir(alloc: std.mem.Allocator, dir_path: []const u8, files_list: *std.ArrayList([]const u8)) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(alloc);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.path, ".zl")) {
            const full_path = try std.fs.path.join(alloc, &[_][]const u8{ dir_path, entry.path });
            defer alloc.free(full_path);
            const path_copy = try alloc.dupe(u8, full_path);
            try files_list.append(path_copy);
        }
    }
}

pub fn main() !u8 {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: zlang <path to file or directory>", .{});
        return 1;
    }

    var ctx = parseArgs(args) catch |err| {
        const error_msg = switch (err) {
            errors.CLIError.NoInputPath => "No input path specified",
            errors.CLIError.NoOutputPath => "No output path specified after -o",
            errors.CLIError.NoArch => "No target specified after -arch",
            errors.CLIError.InvalidArgument => "Unrecognized argument",
            errors.CLIError.NoHelp => "Ahahahha, help message? I wont help you",
            else => "Unknown error while parsing arguments",
        };
        std.debug.print("{s}\n", .{error_msg});
        return 1;
    };
    defer ctx.deinit();

    const ast_root = parseMultiFile(&ctx, allocator) catch |err| {
        std.debug.print("Error parsing files: {}\n", .{err});
        return 1;
    };

    defer ast_root.destroy();
    if (ctx.show_ast) {
        ast.printASTTree(ast_root);
    }

    // Generate LLVM IR and compile to executable
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
    code_generator.generateCode(ast_root) catch |err| {
        const error_msg = switch (err) {
            error.FunctionCreationFailed => "Failed to create function.",
            error.TypeMismatch => "Type mismatch in code generation.",
            error.UndefinedFunction => "Undefined function called.",
            error.UndefinedVariable => "Undefined variable used.",
            error.UnsupportedOperation => "Unsopported operator used",
            error.OutOfMemory => "Out of memory during code generation.",
            error.RedeclaredVariable => "Variable reinitialization",
            else => "Unknown code generation error.",
        };
        std.debug.print("Error generating code: {s}\n", .{error_msg});
        return 1;
    };

    code_generator.compileToExecutable(ctx.output, ctx.arch, ctx.link_objects.items, ctx.keepll, ctx.optimize) catch |err| {
        std.debug.print("Error compiling to executable: {}\n", .{err});
        return 1;
    };

    if (ctx.verbose) {
        std.debug.print("Executable compiled to {s}\n", .{ctx.output});
    }
    return 0;
}
