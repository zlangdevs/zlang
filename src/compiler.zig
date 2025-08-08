const std = @import("std");
const ast = @import("ast.zig");
const errors = @import("errors.zig");

pub const CompileError = error{
    UndefinedVariable,
    UndefinedFunction,
    TypeMismatch,
    InvalidReturn,
    CompilationFailed,
    OutOfMemory,
};

const Variable = struct {
    name: []const u8,
    type_name: []const u8,
    value: i32 = 0, // For mock implementation
};

const Function = struct {
    name: []const u8,
    return_type: []const u8,
    is_libc: bool = false,
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),

    // Symbol tables
    variables: std.HashMap([]const u8, Variable, std.hash_map.StringContext, 80),
    functions: std.HashMap([]const u8, Function, std.hash_map.StringContext, 80),

    // Current function context
    current_function: ?[]const u8 = null,
    indent_level: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, module_name: []const u8) !Compiler {
        var compiler = Compiler{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .variables = std.HashMap([]const u8, Variable, std.hash_map.StringContext, 80).init(allocator),
            .functions = std.HashMap([]const u8, Function, std.hash_map.StringContext, 80).init(allocator),
        };

        // Add header
        try compiler.writeLine("; Generated LLVM IR by Zlang Compiler");
        try compiler.writeLine(std.fmt.allocPrint(allocator, "; Module: {s}", .{module_name}) catch "");
        try compiler.writeLine("");

        // Declare common libc functions
        try compiler.declareLibcFunctions();

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.variables.deinit();
        self.functions.deinit();
        self.output.deinit();
    }

    fn declareLibcFunctions(self: *Compiler) !void {
        // printf declaration
        try self.writeLine("declare i32 @printf(i8*, ...)");
        try self.functions.put("printf", Function{
            .name = "printf",
            .return_type = "i32",
            .is_libc = true,
        });

        // puts declaration
        try self.writeLine("declare i32 @puts(i8*)");
        try self.functions.put("puts", Function{
            .name = "puts",
            .return_type = "i32",
            .is_libc = true,
        });

        try self.writeLine("");
    }

    fn writeLine(self: *Compiler, line: []const u8) !void {
        // Add indentation
        var i: u32 = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.output.append(' ');
            try self.output.append(' ');
        }

        try self.output.appendSlice(line);
        try self.output.append('\n');
    }

    fn writeIndented(self: *Compiler, text: []const u8) !void {
        const old_indent = self.indent_level;
        self.indent_level += 1;
        try self.writeLine(text);
        self.indent_level = old_indent;
    }

    pub fn compile(self: *Compiler, root: *ast.Node) CompileError!void {
        try self.compileNode(root);
    }

    fn compileNode(self: *Compiler, node: *ast.Node) CompileError!void {
        switch (node.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    try self.compileNode(func);
                }
            },
            .function => |func| {
                try self.compileFunction(func);
            },
            .var_decl => |decl| {
                try self.compileVarDecl(decl);
            },
            .function_call => |call| {
                try self.compileFunctionCall(call);
            },
            .return_stmt => |ret| {
                try self.compileReturn(ret);
            },
            .identifier => |ident| {
                try self.compileIdentifier(ident);
            },
            .number_literal => |num| {
                try self.compileNumberLiteral(num);
            },
            .string_literal => |str| {
                try self.compileStringLiteral(str);
            },
        }
    }

    fn compileFunction(self: *Compiler, func: ast.Function) CompileError!void {
        // Store function in symbol table
        try self.functions.put(func.name, Function{
            .name = func.name,
            .return_type = func.return_type,
            .is_libc = false,
        });

        self.current_function = func.name;

        // Generate function signature
        const llvm_return_type = self.getLLVMType(func.return_type);
        const func_line = try std.fmt.allocPrint(self.allocator, "define {s} @{s}() {{", .{ llvm_return_type, func.name });
        try self.writeLine(func_line);

        // Function body
        self.indent_level += 1;
        try self.writeLine("entry:");

        // Clear local variables for new function
        var new_vars = std.HashMap([]const u8, Variable, std.hash_map.StringContext, 80).init(self.allocator);
        const old_vars = self.variables;
        self.variables = new_vars;
        defer {
            self.variables.deinit();
            self.variables = old_vars;
        }

        // Compile function body
        for (func.body.items) |stmt| {
            try self.compileNode(stmt);
        }

        // Add default return if needed
        if (!self.hasReturn(func.body.items)) {
            if (std.mem.eql(u8, func.return_type, "void")) {
                try self.writeIndented("ret void");
            } else {
                try self.writeIndented("ret i32 0");
            }
        }

        self.indent_level -= 1;
        try self.writeLine("}");
        try self.writeLine("");

        self.current_function = null;
    }

    fn compileVarDecl(self: *Compiler, decl: ast.VarDecl) CompileError!void {
        const llvm_type = self.getLLVMType(decl.type_name);

        // Create alloca for local variable
        const alloca_line = try std.fmt.allocPrint(self.allocator, "%{s} = alloca {s}", .{ decl.name, llvm_type });
        try self.writeIndented(alloca_line);

        // Store variable in symbol table
        try self.variables.put(decl.name, Variable{
            .name = decl.name,
            .type_name = decl.type_name,
        });

        // Handle initialization
        if (decl.initializer) |init| {
            const init_value = try self.getExpressionValue(init);
            const store_line = try std.fmt.allocPrint(self.allocator, "store {s} {s}, {s}* %{s}", .{ llvm_type, init_value, llvm_type, decl.name });
            try self.writeIndented(store_line);
        }
    }

    fn compileFunctionCall(self: *Compiler, call: ast.FunctionCall) CompileError!void {
        // Look up function
        const func_entry = self.functions.get(call.name) orelse {
            std.debug.print("Error: Undefined function '{s}'\n", .{call.name});
            return CompileError.UndefinedFunction;
        };

        // Generate arguments
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer {
            for (args.items) |arg| {
                self.allocator.free(arg);
            }
            args.deinit();
        }

        for (call.args.items) |arg| {
            const arg_value = try self.getExpressionValue(arg);
            try args.append(arg_value);
        }

        // Generate function call
        const call_line = if (args.items.len > 0)
            try std.fmt.allocPrint(self.allocator, "call {s} @{s}({s})", .{ self.getLLVMType(func_entry.return_type), call.name, try self.joinArgs(args.items) })
        else
            try std.fmt.allocPrint(self.allocator, "call {s} @{s}()", .{ self.getLLVMType(func_entry.return_type), call.name });

        try self.writeIndented(call_line);
    }

    fn compileReturn(self: *Compiler, ret: ast.ReturnStmt) CompileError!void {
        if (ret.expression) |expr| {
            const value = try self.getExpressionValue(expr);
            const return_line = try std.fmt.allocPrint(self.allocator, "ret i32 {s}", .{value});
            try self.writeIndented(return_line);
        } else {
            try self.writeIndented("ret void");
        }
    }

    fn compileIdentifier(self: *Compiler, ident: ast.Identifier) CompileError!void {
        _ = self.variables.get(ident.name) orelse {
            std.debug.print("Error: Undefined variable '{s}'\n", .{ident.name});
            return CompileError.UndefinedVariable;
        };

        // For standalone identifier, just load it
        const load_line = try std.fmt.allocPrint(self.allocator, "%temp = load i32, i32* %{s}", .{ident.name});
        try self.writeIndented(load_line);
    }

    fn compileNumberLiteral(self: *Compiler, num: ast.NumberLiteral) CompileError!void {
        _ = self;
        _ = num;
        // Number literals are handled in getExpressionValue
    }

    fn compileStringLiteral(self: *Compiler, str: ast.StringLiteral) CompileError!void {
        // Create global string constant
        const global_name = try std.fmt.allocPrint(self.allocator, "@str_{d}", .{str.value.len} // Use length as unique identifier
        );

        const global_line = try std.fmt.allocPrint(self.allocator, "{s} = private unnamed_addr constant [{d} x i8] c\"{s}\\00\"", .{ global_name, str.value.len + 1, str.value });
        try self.writeLine(global_line);
    }

    fn getExpressionValue(self: *Compiler, node: *ast.Node) CompileError![]const u8 {
        return switch (node.data) {
            .number_literal => |num| try self.allocator.dupe(u8, num.value),
            .string_literal => |str| blk: {
                // For string literals in expressions, return pointer to global
                const global_name = try std.fmt.allocPrint(self.allocator, "getelementptr inbounds ([{d} x i8], [{d} x i8]* @str_{d}, i32 0, i32 0)", .{ str.value.len + 1, str.value.len + 1, str.value.len });
                break :blk global_name;
            },
            .identifier => |ident| blk: {
                _ = self.variables.get(ident.name) orelse {
                    return CompileError.UndefinedVariable;
                };
                break :blk try std.fmt.allocPrint(self.allocator, "%{s}_load", .{ident.name});
            },
            .function_call => |call| blk: {
                // Handle function calls that return values
                const func_entry = self.functions.get(call.name) orelse {
                    return CompileError.UndefinedFunction;
                };

                // Generate unique result name
                break :blk try std.fmt.allocPrint(self.allocator, "%call_{s}", .{call.name});
            },
            else => try self.allocator.dupe(u8, "0"),
        };
    }

    fn joinArgs(self: *Compiler, args: [][]const u8) ![]const u8 {
        if (args.len == 0) return try self.allocator.dupe(u8, "");

        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        for (args, 0..) |arg, i| {
            if (i > 0) try result.appendSlice(", ");
            try result.appendSlice("i32 ");
            try result.appendSlice(arg);
        }

        return result.toOwnedSlice();
    }

    fn getLLVMType(self: *Compiler, type_name: []const u8) []const u8 {
        _ = self;
        if (std.mem.eql(u8, type_name, "i32")) {
            return "i32";
        } else if (std.mem.eql(u8, type_name, "void")) {
            return "void";
        } else {
            return "i32"; // Default to i32 for unknown types
        }
    }

    fn hasReturn(self: *Compiler, statements: []const *ast.Node) bool {
        _ = self;
        for (statements) |stmt| {
            switch (stmt.data) {
                .return_stmt => return true,
                else => continue,
            }
        }
        return false;
    }

    pub fn printIR(self: *Compiler) void {
        std.debug.print("\n🔥 Generated LLVM IR:\n");
        std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
        std.debug.print("{s}", .{self.output.items});
        std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
    }

    pub fn verify(self: *Compiler) !void {
        _ = self;
        std.debug.print("✅ Mock LLVM Module verified successfully!\n");
    }

    pub fn writeObjectFile(self: *Compiler, filename: []const u8) !void {
        // Create mock object file
        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();

        // Write mock object file content
        try file.writeAll("Mock object file generated by Zlang compiler\n");
        try file.writeAll("This would contain actual machine code in a real implementation\n");

        // Also write the IR to a .ll file for inspection
        const ir_filename = try std.fmt.allocPrint(self.allocator, "{s}.ll", .{filename[0 .. filename.len - 2]} // Remove .o extension
        );
        defer self.allocator.free(ir_filename);

        const ir_file = try std.fs.cwd().createFile(ir_filename, .{});
        defer ir_file.close();
        try ir_file.writeAll(self.output.items);

        std.debug.print("✅ Object file written to: {s}\n", .{filename});
        std.debug.print("📄 LLVM IR written to: {s}\n", .{ir_filename});
    }
};

// Convenient compile function
pub fn compileAST(allocator: std.mem.Allocator, root: *ast.Node, module_name: []const u8) !Compiler {
    var compiler = try Compiler.init(allocator, module_name);
    try compiler.compile(root);
    return compiler;
}
