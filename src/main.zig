const std = @import("std");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const parser = @import("parser/parser.zig");
const ast = @import("parser/ast.zig");
const codegen = @import("codegen/llvm.zig");

const allocator = std.heap.page_allocator;

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
    } else if (file_stat.kind == .directory) {
        return errors.ReadFileError.InvalidPath;
    }
    return "";
}

const Context = struct {
    input_path: []const u8,
    output_path: []const u8,
    arch: []const u8,
    keepll: bool = false,
    
    pub fn print(self: *const Context) void {
        std.debug.print("========Compilation context=======\n", .{});
        std.debug.print("Input path: {s}\n", .{self.input_path});
        std.debug.print("Output path: {s}\n", .{self.output_path});
        std.debug.print("Architecture: {s}\n", .{self.arch});
        std.debug.print("Keep ll: {s}\n", .{ if (self.keepll) "yes" else "no" });
        std.debug.print("==================================\n", .{});
    }
};



fn parseArgs(alloc: std.mem.Allocator, args: [][:0] u8) anyerror!Context {
    _ = alloc;
    var context = Context{
        .input_path = "",
        .output_path = "",
        .arch = "",
        .keepll = false,
    };
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        switch (args[i][0]) {
            '-' => {
                const flag = args[i];
                if (std.mem.eql(u8, flag, "-keepll")) {
                    context.keepll = true;
                } else if (std.mem.eql(u8, flag, "-o")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoOutputPath;
                    context.output_path = args[i];
                } else if (std.mem.eql(u8, flag, "-arch")) {
                    i += 1;
                    if (i >= args.len) return errors.CLIError.NoArch;
                    context.arch = args[i];
                } else {
                    return errors.CLIError.InvalidArgument;
                }
            },
            else => {
                if (context.input_path.len == 0) {
                    context.input_path = args[i];
                }
            },
        }
    }
    
    return if (context.input_path.len == 0) 
        errors.CLIError.NoInputPath
    else context;
}

pub fn main() !u8 {
    const args = try std.process.argsAlloc(allocator);
    const ctx = parseArgs(allocator, args) catch |err| {
        const error_msg = switch(err) {
            errors.CLIError.NoInputPath => "No input path specified",
            errors.CLIError.NoOutputPath => "No output path specified after -o",
            errors.CLIError.NoArch => "No target specified after -arch",
            errors.CLIError.InvalidArgument => "Unrecognized argument",
            else => "Unknown error while parsing arguments"
        };
        std.debug.print("Error: {s}\n", .{error_msg});
        return 1;
    };
    ctx.print();
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: zlang <path to file>", .{});
        return 1;
    }
    const input_file = ctx.input_path;
    const input = read_file(input_file) catch |err| {
        const error_msg = switch (err) {
            error.FileNotFound => "Specified file does not exist or path is invalid.",
            error.AccessDenied => "Error accessing file.",
            error.InvalidPath => "We don't support directories yet.",
            error.OutOfMemory => "",
            error.IOError => "", // TODO: implement these errors
            else => "",
        };
        std.debug.print("Error: {s}\n", .{error_msg});
        return 1;
    };
    std.debug.print("Content of input file: {s}\n", .{input});

    const ast_root = parser.parse(allocator, input) catch |err| {
        const error_msg = switch (err) {
            error.LexerInitFailed => "Failed to initialize the lexer.",
            error.FileOpenFailed => "Failed to open file.",
            error.ParseFailed => "Failed to parse input.",
            error.OutOfMemory => "Out of memory.",
        };
        std.debug.print("Error parsing: {s}\n", .{error_msg});
        return 1;
    };

    if (ast_root) |root| {
        defer root.destroy();
        ast.printASTTree(root);

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

        code_generator.generateCode(root) catch |err| {
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

        // Write LLVM IR to file
        const exe_filename = if (ctx.output_path.len == 0) consts.DEFAULT_OUTPUT_NAME
            else ctx.output_path;
        const ir_filename = try std.fmt.allocPrint(allocator, "{s}.ll", .{exe_filename});
        defer allocator.free(ir_filename);
        code_generator.writeToFile(ir_filename) catch |err| {
            std.debug.print("Error writing LLVM IR to file: {}\n", .{err});
            return 1;
        };
        std.debug.print("LLVM IR written to {s}\n", .{ir_filename});

        // Compile to executable
        code_generator.compileToExecutable(exe_filename, ctx.arch) catch |err| {
            std.debug.print("Error compiling to executable: {}\n", .{err});
            return 1;
        };
        std.debug.print("Executable compiled to {s}\n", .{exe_filename});
        if (!ctx.keepll) {
            const cwd = std.fs.cwd();
            try cwd.deleteFile(ir_filename);
        }
    } else {
        std.debug.print("No AST generated.\n", .{});
    }

    return 0;
}
