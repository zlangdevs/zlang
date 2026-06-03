//! ZLX Plugin SDK (Zig) — convenience wrappers over the raw v1 plugin API.
//!
//! Plugin authors `const sdk = @import("zlx_plugin_sdk");` and use the
//! helpers instead of touching the extern struct directly. Mirrors the
//! C header `zlx_plugin_sdk.h`.

const std = @import("std");
const abi = @import("abi.zig");

pub const ApiVersion = abi.api_version;
pub const DiagnosticLevel = abi.DiagnosticLevel;
pub const DelimiterMode = abi.DelimiterMode;
pub const RegisterResult = abi.RegisterResult;

pub const HostApi = extern struct {
    api_version: u32,
    register_syntax_block: *const fn (*HostApi, [*:0]const u8, *const BlockSyntax, BlockHandler) callconv(.c) c_int,
    register_help_section: *const fn (*HostApi, [*:0]const u8, [*:0]const u8) callconv(.c) c_int,
    register_cli_flag: *const fn (*HostApi, [*:0]const u8, ?[*:0]const u8, c_int) callconv(.c) c_int,
    register_module: *const fn (*HostApi, [*:0]const u8, [*:0]const u8) callconv(.c) c_int,
    register_link_flag: *const fn (*HostApi, [*:0]const u8) callconv(.c) c_int,
    diagnostic: *const fn (*HostApi, c_int, ?[*:0]const u8, u32, u32, [*:0]const u8, ?[*:0]const u8) callconv(.c) void,
    resolve_type_size: *const fn (*HostApi, [*:0]const u8, [*:0]const u8) callconv(.c) i32,
    get_cli_flag: *const fn (*HostApi, [*:0]const u8) callconv(.c) ?[*:0]const u8,
    register_file_extension: *const fn (*HostApi, [*:0]const u8, FileExtensionHandler) callconv(.c) c_int,
    register_keyword_block: *const fn (*HostApi, [*:0]const u8, *const BlockSyntax, BlockHandler) callconv(.c) c_int,
};

pub const FileExtensionRequest = extern struct {
    input_path: [*:0]const u8,
    output_path: ?[*:0]const u8,
    want_continue: c_int,
};
pub const FileExtensionResult = extern struct {
    continue_path: ?[*:0]const u8,
    llvm_ir_path: ?[*:0]const u8 = null,
};
pub const FileExtensionHandler = *const fn (*HostApi, *const FileExtensionRequest, *FileExtensionResult) callconv(.c) c_int;

pub const BlockSyntax = extern struct { mode: c_int, terminator: ?[*:0]const u8 };
pub const BlockInput = extern struct {
    file: [*:0]const u8,
    line: u32,
    column: u32,
    raw_source: [*]const u8,
    raw_source_len: u32,
};
pub const BlockOutput = extern struct {
    generated_zlang_source: [*]const u8,
    generated_zlang_source_len: u32,
    source_map: ?[*]const SourceMapEntry,
    source_map_len: u32,
};
pub const SourceMapEntry = extern struct {
    generated_offset: u32,
    original_line: u32,
    original_column: u32,
};
pub const BlockHandler = *const fn (*HostApi, *const BlockInput, *BlockOutput) callconv(.c) c_int;

// ============================================================
// Syntax registration
// ============================================================

pub fn registerSyntax(host: *HostApi, name: [*:0]const u8, handler: BlockHandler) c_int {
    const syntax = BlockSyntax{ .mode = @intFromEnum(DelimiterMode.brace_counting), .terminator = null };
    return host.register_syntax_block(host, name, &syntax, handler);
}

pub fn registerKeyword(host: *HostApi, name: [*:0]const u8, handler: BlockHandler) c_int {
    const syntax = BlockSyntax{ .mode = @intFromEnum(DelimiterMode.brace_counting), .terminator = null };
    return host.register_keyword_block(host, name, &syntax, handler);
}

// ============================================================
// Diagnostics
// ============================================================

pub fn diag(
    host: *HostApi,
    level: DiagnosticLevel,
    file: ?[*:0]const u8,
    line: u32,
    column: u32,
    message: [*:0]const u8,
    hint: ?[*:0]const u8,
) void {
    host.diagnostic(host, @intFromEnum(level), file, line, column, message, hint);
}

pub fn diagError(host: *HostApi, file: ?[*:0]const u8, line: u32, col: u32, msg: [*:0]const u8) void {
    diag(host, .err, file, line, col, msg, null);
}

pub fn diagWarn(host: *HostApi, file: ?[*:0]const u8, line: u32, col: u32, msg: [*:0]const u8) void {
    diag(host, .warning, file, line, col, msg, null);
}

pub fn diagNote(host: *HostApi, file: ?[*:0]const u8, line: u32, col: u32, msg: [*:0]const u8) void {
    diag(host, .note, file, line, col, msg, null);
}

pub fn diagErrorHint(
    host: *HostApi,
    file: ?[*:0]const u8,
    line: u32,
    col: u32,
    msg: [*:0]const u8,
    hint: [*:0]const u8,
) void {
    diag(host, .err, file, line, col, msg, hint);
}

// ============================================================
// Module / link / help / CLI flag
// ============================================================

pub fn registerModule(host: *HostApi, name: [*:0]const u8, path: [*:0]const u8) c_int {
    return host.register_module(host, name, path);
}

pub fn registerLinkFlag(host: *HostApi, flag: [*:0]const u8) c_int {
    return host.register_link_flag(host, flag);
}

pub fn registerHelp(host: *HostApi, name: [*:0]const u8, text: [*:0]const u8) c_int {
    return host.register_help_section(host, name, text);
}

pub fn registerCliFlag(
    host: *HostApi,
    name: [*:0]const u8,
    description: ?[*:0]const u8,
    mandatory: bool,
) c_int {
    return host.register_cli_flag(host, name, description, if (mandatory) 1 else 0);
}

pub fn cliFlag(host: *HostApi, name: [*:0]const u8) ?[:0]const u8 {
    const ptr = host.get_cli_flag(host, name) orelse return null;
    return std.mem.sliceTo(ptr, 0);
}

pub fn resolveTypeSize(host: *HostApi, file: [*:0]const u8, type_name: [*:0]const u8) i32 {
    return host.resolve_type_size(host, file, type_name);
}

// ============================================================
// Block output writers
// ============================================================

pub fn emit(output: *BlockOutput, text: []const u8) c_int {
    output.* = .{
        .generated_zlang_source = text.ptr,
        .generated_zlang_source_len = @intCast(text.len),
        .source_map = null,
        .source_map_len = 0,
    };
    return 0;
}

pub fn emitEmpty(output: *BlockOutput) c_int {
    output.* = .{
        .generated_zlang_source = @ptrCast(""),
        .generated_zlang_source_len = 0,
        .source_map = null,
        .source_map_len = 0,
    };
    return 0;
}

// ============================================================
// Input accessors
// ============================================================

pub fn inputFile(input: *const BlockInput) [:0]const u8 {
    return std.mem.sliceTo(input.file, 0);
}

pub fn inputRaw(input: *const BlockInput) []const u8 {
    return input.raw_source[0..@intCast(input.raw_source_len)];
}

pub fn inputLine(input: *const BlockInput) u32 {
    return input.line;
}

pub fn inputColumn(input: *const BlockInput) u32 {
    return input.column;
}
