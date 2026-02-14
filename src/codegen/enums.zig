const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

const CodeGenerator = @import("llvm.zig").CodeGenerator;

pub fn generateEnumDeclaration(self: *CodeGenerator, enum_decl: ast.EnumDecl) errors.CodegenError!void {
    var current_value: i32 = 0;
    for (enum_decl.values.items) |enum_value| {
        var value: i32 = current_value;
        if (enum_value.value) |expr| {
            const const_value = try self.generateExpression(expr);
            if (c.LLVMIsConstant(const_value) == 0) {
                std.debug.print("warning: skipping enum value {s}.{s}: non-constant initializer\n", .{ enum_decl.name, enum_value.name });
                current_value +%= 1;
                continue;
            }

            const const_type = c.LLVMTypeOf(const_value);
            if (c.LLVMGetTypeKind(const_type) != c.LLVMIntegerTypeKind) {
                std.debug.print("warning: skipping enum value {s}.{s}: unsupported initializer type\n", .{ enum_decl.name, enum_value.name });
                current_value +%= 1;
                continue;
            }

            const int_bits_64 = c.LLVMConstIntGetZExtValue(const_value);
            const int_bits_32: u32 = @truncate(int_bits_64);
            value = @bitCast(int_bits_32);
        }

        const value_bits: u32 = @bitCast(value);
        const enum_const = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, value_bits), 0);
        const enum_name_z = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ enum_decl.name, enum_value.name });
        defer self.allocator.free(enum_name_z);
        const enum_name_z_null = utils.dupeZ(self.allocator, enum_name_z);
        defer self.allocator.free(enum_name_z_null);
        const global_var = c.LLVMAddGlobal(self.module, c.LLVMInt32TypeInContext(self.context), enum_name_z_null.ptr);
        c.LLVMSetInitializer(global_var, enum_const);
        c.LLVMSetLinkage(global_var, c.LLVMExternalLinkage);
        c.LLVMSetGlobalConstant(global_var, 1);

        const var_info = structs.VariableInfo{
            .value = @ptrCast(global_var),
            .type_ref = @ptrCast(c.LLVMInt32TypeInContext(@ptrCast(self.context))),
            .type_name = "i32",
            .is_const = true,
        };
        try self.variables.put(utils.dupe(u8, self.allocator, enum_name_z), var_info);
        try self.variables.put(utils.dupe(u8, self.allocator, enum_value.name), var_info);

        current_value = value +% 1;
    }
}

pub fn processGlobalEnums(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!void {
    switch (func_node.data) {
        .function => |func| {
            for (func.body.items) |stmt| {
                try findAndProcessEnums(self, stmt);
            }
        },
        else => {},
    }
}

pub fn findAndProcessEnums(self: *CodeGenerator, node: *ast.Node) errors.CodegenError!void {
    switch (node.data) {
        .enum_decl => |enum_decl| {
            try generateEnumDeclaration(self, enum_decl);
        },
        .if_stmt => |if_stmt| {
            for (if_stmt.then_body.items) |stmt| {
                try findAndProcessEnums(self, stmt);
            }
            if (if_stmt.else_body) |else_body| {
                for (else_body.items) |stmt| {
                    try findAndProcessEnums(self, stmt);
                }
            }
        },
        .for_stmt => |for_stmt| {
            for (for_stmt.body.items) |stmt| {
                try findAndProcessEnums(self, stmt);
            }
        },
        .c_for_stmt => |c_for_stmt| {
            for (c_for_stmt.body.items) |stmt| {
                try findAndProcessEnums(self, stmt);
            }
        },
        else => {},
    }
}

pub fn tryLoadEnumValue(self: *CodeGenerator, qual_id: ast.QualifiedIdentifier) errors.CodegenError!?c.LLVMValueRef {
    if (qual_id.base.data != .identifier) {
        return null;
    }

    const base_name = qual_id.base.data.identifier.name;
    const enum_var_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ base_name, qual_id.field });
    defer self.allocator.free(enum_var_name);

    if (CodeGenerator.getVariable(self, enum_var_name)) |enum_var_info| {
        const load_instr = c.LLVMBuildLoad2(self.builder, @ptrCast(enum_var_info.type_ref), @ptrCast(enum_var_info.value), "enum_value");
        const alignment = self.getAlignmentForType(@ptrCast(enum_var_info.type_ref));
        c.LLVMSetAlignment(load_instr, alignment);
        return load_instr;
    }

    return null;
}
