pub const sdk_zig = @embedFile("sdk.zig");
pub const abi_zig = @embedFile("abi.zig");
pub const api_v1_h = @embedFile("zlang_plugin_api_v1.h");
pub const sdk_h = @embedFile("zlx_plugin_sdk.h");

pub const File = struct { name: []const u8, bytes: []const u8 };

pub const files = [_]File{
    .{ .name = "sdk.zig", .bytes = sdk_zig },
    .{ .name = "abi.zig", .bytes = abi_zig },
    .{ .name = "zlang_plugin_api_v1.h", .bytes = api_v1_h },
    .{ .name = "zlx_plugin_sdk.h", .bytes = sdk_h },
};
