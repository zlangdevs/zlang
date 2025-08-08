const std = @import("std");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const compiler = @import("compiler.zig");

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
        std.debug.print("Usage: zlang <path to file> [options]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  --ast-only    Show only AST without compilation\n", .{});
        std.debug.print("  --ir-only     Show only LLVM IR without object file\n", .{});
        std.debug.print("  --output <file> Specify output object file name\n", .{});
        return 1;
    }

    const input_file = args[1];
    var ast_only = false;
    var ir_only = false;
    var output_file: ?[]const u8 = null;

    // Parse command line arguments
    var i: usize = 2;
    while (i < args.len) {
        if (std.mem.eql(u8, args[i], "--ast-only")) {
            ast_only = true;
        } else if (std.mem.eql(u8, args[i], "--ir-only")) {
            ir_only = true;
        } else if (std.mem.eql(u8, args[i], "--output") and i + 1 < args.len) {
            i += 1;
            output_file = args[i];
        }
        i += 1;
    }

    // Read input file
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

    std.debug.print("🚀 Compiling Zlang file: {s}\n", .{input_file});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});

    // Parse to AST
    const ast_root = parser.parse(allocator, input) catch |err| {
        const error_msg = switch (err) {
            error.LexerInitFailed => "Failed to initialize the lexer.",
            error.FileOpenFailed => "Failed to open file.",
            error.ParseFailed => "Failed to parse input.",
            error.OutOfMemory => "Out of memory.",
        };
        std.debug.print("❌ Parse Error: {s}\n", .{error_msg});
        return 1;
    };

    if (ast_root) |root| {
        defer root.destroy();

        std.debug.print("✅ Parsing completed successfully!\n", .{});
        ast.printASTTree(root);

        if (ast_only) {
            return 0;
        }

        // Compile to LLVM IR
        std.debug.print("🔧 Compiling to LLVM IR...\n", .{});

        var llvm_compiler = compiler.compileAST(allocator, root, "zlang_module") catch |err| {
            const error_msg = switch (err) {
                error.UndefinedVariable => "Undefined variable referenced.",
                error.UndefinedFunction => "Undefined function called.",
                error.TypeMismatch => "Type mismatch in expression.",
                error.InvalidReturn => "Invalid return statement.",
                error.LLVMError => "LLVM compilation error.",
                error.OutOfMemory => "Out of memory during compilation.",
            };
            std.debug.print("❌ Compilation Error: {s}\n", .{error_msg});
            return 1;
        };
        defer llvm_compiler.deinit();

        std.debug.print("✅ LLVM IR compilation completed!\n", .{});

        // Verify LLVM module
        llvm_compiler.verify() catch |err| switch (err) {
            error.LLVMError => {
                std.debug.print("❌ LLVM verification failed!\n", .{});
                return 1;
            },
            else => return 1,
        };

        // Print LLVM IR
        llvm_compiler.printIR();

        if (ir_only) {
            return 0;
        }

        // Generate object file
        const obj_file = output_file orelse "output.o";
        std.debug.print("🎯 Generating object file: {s}\n", .{obj_file});

        llvm_compiler.writeObjectFile(obj_file) catch |err| {
            const error_msg = switch (err) {
                error.LLVMError => "Failed to generate object file.",
                error.OutOfMemory => "Out of memory.",
            };
            std.debug.print("❌ Object Generation Error: {s}\n", .{error_msg});
            return 1;
        };

        std.debug.print("🎉 Compilation successful!\n", .{});
        std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
        std.debug.print("💡 To create executable, run: gcc {s} -o executable\n", .{obj_file});
    } else {
        std.debug.print("❌ No AST generated.\n", .{});
        return 1;
    }

    return 0;
}
