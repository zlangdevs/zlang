const std = @import("std");

const c_bindings = @import("codegen/c_bindings.zig");
const c = c_bindings.c;

pub const ConversionKind = enum {
    none,
    simd_f32_2,
    simd_f32_4,
    packed_rgba_u32,
};

pub const StructField = struct {
    name: []const u8,
    z_type: []const u8,
};

pub const StructAbiMapping = struct {
    abi_type: ?[]const u8 = null,
    conversion: ConversionKind = .none,

    pub fn hasConversion(self: StructAbiMapping) bool {
        return self.conversion != .none and self.abi_type != null;
    }
};

fn eqlCaseInsensitive(a: []const u8, b: []const u8) bool {
    return std.ascii.eqlIgnoreCase(a, b);
}

fn allFieldsOfType(fields: []const StructField, expected: []const u8) bool {
    for (fields) |field| {
        if (!std.mem.eql(u8, field.z_type, expected)) return false;
    }
    return true;
}

fn hasRgbaFieldNames(fields: []const StructField) bool {
    if (fields.len != 4) return false;
    return eqlCaseInsensitive(fields[0].name, "r") and
        eqlCaseInsensitive(fields[1].name, "g") and
        eqlCaseInsensitive(fields[2].name, "b") and
        eqlCaseInsensitive(fields[3].name, "a");
}

pub fn classifyStruct(fields: []const StructField) StructAbiMapping {
    if (fields.len == 2 and allFieldsOfType(fields, "f32")) {
        return .{
            .abi_type = "simd<f32, 2>",
            .conversion = .simd_f32_2,
        };
    }

    if (fields.len == 4 and allFieldsOfType(fields, "f32")) {
        return .{
            .abi_type = "simd<f32, 4>",
            .conversion = .simd_f32_4,
        };
    }

    if (fields.len == 4 and allFieldsOfType(fields, "u8") and hasRgbaFieldNames(fields)) {
        return .{
            .abi_type = "u32",
            .conversion = .packed_rgba_u32,
        };
    }

    return .{};
}

pub fn getStructSizeBytesForModule(module: c.LLVMModuleRef, struct_type: c.LLVMTypeRef) u64 {
    const module_data_layout = c.LLVMGetModuleDataLayout(module);
    if (module_data_layout != null) {
        const abi_size = c.LLVMABISizeOfType(module_data_layout, struct_type);
        if (abi_size > 0) return abi_size;
    }

    const fallback_target_data = c.LLVMCreateTargetData("e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128");
    defer c.LLVMDisposeTargetData(fallback_target_data);
    return c.LLVMABISizeOfType(fallback_target_data, struct_type);
}
