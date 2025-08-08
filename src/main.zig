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

pub fn main() !u8 {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: zlang <path to file>", .{});
        return 1;
    }
    const input_file = args[1];
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
                error.OutOfMemory => "Out of memory during code generation.",
                else => "Unknown code generation error.",
            };
            std.debug.print("Error generating code: {s}\n", .{error_msg});
            return 1;
        };

        // Write LLVM IR to file
        const ir_filename = "output.ll";
        code_generator.writeToFile(ir_filename) catch |err| {
            std.debug.print("Error writing LLVM IR to file: {}\n", .{err});
            return 1;
        };
        std.debug.print("LLVM IR written to {s}\n", .{ir_filename});

        // Compile to executable
        const exe_filename = "output";
        code_generator.compileToExecutable(exe_filename) catch |err| {
            std.debug.print("Error compiling to executable: {}\n", .{err});
            return 1;
        };
        std.debug.print("Executable compiled to {s}\n", .{exe_filename});
    } else {
        std.debug.print("No AST generated.\n", .{});
    }

    return 0;
}
