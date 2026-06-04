const std = @import("std");

const HostApi = extern struct {
    api_version: u32,
    register_syntax_block: *const fn (
        host: *HostApi,
        name: [*:0]const u8,
        syntax: *const BlockSyntax,
        handler: BlockHandler,
    ) callconv(.c) c_int,
    register_help_section: *const fn (host: *HostApi, id: [*:0]const u8, text: [*:0]const u8) callconv(.c) c_int,
    register_cli_flag: *const fn (host: *HostApi, name: [*:0]const u8, help: ?[*:0]const u8, mandatory: c_int) callconv(.c) c_int,
    register_module: *const fn (host: *HostApi, name: [*:0]const u8, path: [*:0]const u8) callconv(.c) c_int,
    register_link_flag: *const fn (host: *HostApi, flag: [*:0]const u8) callconv(.c) c_int,
    diagnostic: *const fn (
        host: *HostApi,
        level: c_int,
        file: ?[*:0]const u8,
        line: u32,
        column: u32,
        message: [*:0]const u8,
        hint: ?[*:0]const u8,
    ) callconv(.c) void,
};

const BlockSyntax = extern struct {
    mode: c_int,
    terminator: ?[*:0]const u8,
};

const BlockInput = extern struct {
    file: [*:0]const u8,
    line: u32,
    column: u32,
    raw_source: [*]const u8,
    raw_source_len: u32,
};

const BlockOutput = extern struct {
    generated_zlang_source: [*]const u8,
    generated_zlang_source_len: u32,
};

const BlockHandler = *const fn (host: *HostApi, input: *const BlockInput, output: *BlockOutput) callconv(.c) c_int;

const ProbeResult = extern struct {
    api_min: u32,
    api_max: u32,
    name: [*:0]const u8,
    version: [*:0]const u8,
    requires_host_features: ?[*:null]const ?[*:0]const u8,
};

const PluginDesc = extern struct {
    api_min: u32,
    api_max: u32,
    name: [*:0]const u8,
    version: [*:0]const u8,
    register_plugin: *const fn (host: *HostApi) callconv(.c) c_int,
};

var probe_singleton: ProbeResult = .{
    .api_min = 1,
    .api_max = 1,
    .name = "dummy",
    .version = "0.1.0",
    .requires_host_features = null,
};

var desc_singleton: PluginDesc = .{
    .api_min = 1,
    .api_max = 1,
    .name = "dummy",
    .version = "0.1.0",
    .register_plugin = registerPlugin,
};

const dummy_block_expansion: []const u8 = "i32 __dummy_block_marker = 42;";

fn dummyHandler(host: *HostApi, input: *const BlockInput, output: *BlockOutput) callconv(.c) c_int {
    _ = host;
    _ = input;
    output.* = .{
        .generated_zlang_source = dummy_block_expansion.ptr,
        .generated_zlang_source_len = @intCast(dummy_block_expansion.len),
    };
    return 0;
}

fn registerPlugin(host: *HostApi) callconv(.c) c_int {
    const syntax = BlockSyntax{ .mode = 1, .terminator = null };
    _ = host.register_syntax_block(host, "dummy_block", &syntax, dummyHandler);
    _ = host.register_cli_flag(host, "-dummy", "dummy flag", 0);
    _ = host.register_module(host, "dummy", "std/dummy.zl");
    _ = host.register_link_flag(host, "-lm");
    host.diagnostic(host, 3, null, 0, 0, "dummy plugin loaded", null);
    return 0;
}

export fn zlang_plugin_probe(host_api_version: u32) callconv(.c) ?*ProbeResult {
    _ = host_api_version;
    return &probe_singleton;
}

export fn zlang_plugin_init(host: *HostApi) callconv(.c) ?*PluginDesc {
    _ = host;
    return &desc_singleton;
}
