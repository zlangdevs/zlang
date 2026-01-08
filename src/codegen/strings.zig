const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

const llvm = @import("llvm.zig");

/// Parse escape sequences in a string literal
/// Converts escape sequences like \n, \t, \r, etc. to their actual byte values
/// Returns an owned slice that must be freed by the caller
pub fn parseEscape(allocator: std.mem.Allocator, str: []const u8) errors.CodegenError![]const u8 {
    var transformed_string = std.ArrayList(u8){};
    defer transformed_string.deinit(allocator);
    var i: usize = 0;
    while (i < str.len) : (i += 1) {
        if (str[i] == '\\') {
            i += 1;
            if (i < str.len) {
                switch (str[i]) {
                    'n' => transformed_string.append(allocator, '\n') catch return errors.CodegenError.OutOfMemory,
                    't' => transformed_string.append(allocator, '\t') catch return errors.CodegenError.OutOfMemory,
                    'r' => transformed_string.append(allocator, '\r') catch return errors.CodegenError.OutOfMemory,
                    '\'' => transformed_string.append(allocator, '\'') catch return errors.CodegenError.OutOfMemory,
                    '"' => transformed_string.append(allocator, '"') catch return errors.CodegenError.OutOfMemory,
                    '0' => transformed_string.append(allocator, 0) catch return errors.CodegenError.OutOfMemory,
                    '\\' => transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory,
                    else => {
                        transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory;
                        transformed_string.append(allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
                    },
                }
            } else {
                transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory;
            }
        } else {
            transformed_string.append(allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
        }
    }
    try transformed_string.append(allocator, 0);
    return transformed_string.toOwnedSlice(allocator);
}

/// Generate LLVM code for a string literal expression
/// Creates a global string pointer with null termination
pub fn generateStringLiteral(cg: *llvm.CodeGenerator, str_lit: ast.StringLiteral) errors.CodegenError!c.LLVMValueRef {
    const parsed_str = try parseEscape(cg.allocator, str_lit.value);
    defer cg.allocator.free(parsed_str);

    const str_with_null = try std.mem.concatWithSentinel(cg.allocator, u8, &.{parsed_str}, 0);
    defer cg.allocator.free(str_with_null);

    return c.LLVMBuildGlobalStringPtr(
        cg.builder,
        str_with_null.ptr,
        "str",
    );
}
