const std = @import("std");

pub const api_version: u32 = 5;
pub const api_min_supported: u32 = 1;
pub const api_max_supported: u32 = 5;

pub const DiagnosticLevel = enum(c_int) {
    err = 1,
    warning = 2,
    note = 3,
};

pub const DelimiterMode = enum(c_int) {
    brace_counting = 1,
    custom_terminator = 2,
};

pub const RegisterResult = enum(c_int) {
    ok = 0,
    duplicate = 1,
    invalid = 2,
    unsupported = 3,
};

pub const BlockSyntax = extern struct {
    mode: DelimiterMode,
    terminator: ?[*:0]const u8,
};

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

pub const BlockHandler = *const fn (
    host: *HostApi,
    input: *const BlockInput,
    output: *BlockOutput,
) callconv(.c) c_int;

pub const HostApi = extern struct {
    api_version: u32,
    register_syntax_block: *const fn (
        host: *HostApi,
        name: [*:0]const u8,
        syntax: *const BlockSyntax,
        handler: BlockHandler,
    ) callconv(.c) c_int,
    register_help_section: *const fn (
        host: *HostApi,
        section_id: [*:0]const u8,
        text: [*:0]const u8,
    ) callconv(.c) c_int,
    register_cli_flag: *const fn (
        host: *HostApi,
        flag_name: [*:0]const u8,
        help_text: ?[*:0]const u8,
        mandatory: c_int,
    ) callconv(.c) c_int,
    register_module: *const fn (
        host: *HostApi,
        module_name: [*:0]const u8,
        package_relative_path: [*:0]const u8,
    ) callconv(.c) c_int,
    register_link_flag: *const fn (
        host: *HostApi,
        flag: [*:0]const u8,
    ) callconv(.c) c_int,
    diagnostic: *const fn (
        host: *HostApi,
        level: DiagnosticLevel,
        file: ?[*:0]const u8,
        line: u32,
        column: u32,
        message: [*:0]const u8,
        hint: ?[*:0]const u8,
    ) callconv(.c) void,
    resolve_type_size: *const fn (
        host: *HostApi,
        file: [*:0]const u8,
        type_name: [*:0]const u8,
    ) callconv(.c) i32,
    get_cli_flag: *const fn (
        host: *HostApi,
        name: [*:0]const u8,
    ) callconv(.c) ?[*:0]const u8,
    register_file_extension: *const fn (
        host: *HostApi,
        extension: [*:0]const u8,
        handler: FileExtensionHandler,
    ) callconv(.c) c_int,
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

pub const FileExtensionHandler = *const fn (
    host: *HostApi,
    request: *const FileExtensionRequest,
    result: *FileExtensionResult,
) callconv(.c) c_int;

pub const ProbeResult = extern struct {
    api_min: u32,
    api_max: u32,
    name: [*:0]const u8,
    version: [*:0]const u8,
    requires_host_features: ?[*:null]const ?[*:0]const u8,
};

pub const PluginDesc = extern struct {
    api_min: u32,
    api_max: u32,
    name: [*:0]const u8,
    version: [*:0]const u8,
    register_plugin: *const fn (host: *HostApi) callconv(.c) c_int,
    session_begin: ?*const fn (host: *HostApi) callconv(.c) void,
    session_end: ?*const fn (host: *HostApi) callconv(.c) void,
};

pub const ProbeFn = *const fn (host_api_version: u32) callconv(.c) ?*ProbeResult;
pub const InitFn = *const fn (host: *HostApi) callconv(.c) ?*PluginDesc;

pub const probe_symbol = "zlang_plugin_probe";
pub const init_symbol = "zlang_plugin_init";

pub const Compatibility = enum {
    compatible,
    api_too_old,
    api_too_new,
};

pub fn checkApiRange(plugin_min: u32, plugin_max: u32) Compatibility {
    if (plugin_max < api_min_supported) return .api_too_old;
    if (plugin_min > api_max_supported) return .api_too_new;
    return .compatible;
}

test "api range compatibility" {
    try std.testing.expectEqual(Compatibility.compatible, checkApiRange(api_min_supported, api_max_supported));
    try std.testing.expectEqual(Compatibility.compatible, checkApiRange(api_max_supported, api_max_supported));
    try std.testing.expectEqual(Compatibility.api_too_new, checkApiRange(api_max_supported + 1, api_max_supported + 2));
    try std.testing.expectEqual(Compatibility.api_too_old, checkApiRange(0, api_min_supported - 1));
}
