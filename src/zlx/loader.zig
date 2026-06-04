const std = @import("std");
const abi = @import("abi.zig");
const host_mod = @import("host.zig");

pub const LoadError = error{
    OpenFailed,
    MissingProbeSymbol,
    MissingInitSymbol,
    ProbeReturnedNull,
    InitReturnedNull,
    ApiTooOld,
    ApiTooNew,
    ProbeInitMismatch,
    RegisterFailed,
};

pub const LoadInfo = struct {
    name: []u8,
    version: []u8,
    api_min: u32,
    api_max: u32,

    pub fn deinit(self: LoadInfo, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        alloc.free(self.version);
    }
};

pub fn loadAndRegister(alloc: std.mem.Allocator, host: *host_mod.Host, so_path: []const u8) LoadError!LoadInfo {
    var lib = std.DynLib.open(so_path) catch return error.OpenFailed;
    errdefer lib.close();

    const probe_fn = lib.lookup(abi.ProbeFn, abi.probe_symbol) orelse return error.MissingProbeSymbol;
    const init_fn = lib.lookup(abi.InitFn, abi.init_symbol) orelse return error.MissingInitSymbol;

    const probe = probe_fn(abi.api_version) orelse return error.ProbeReturnedNull;

    switch (abi.checkApiRange(probe.api_min, probe.api_max)) {
        .compatible => {},
        .api_too_old => return error.ApiTooOld,
        .api_too_new => return error.ApiTooNew,
    }

    const desc = init_fn(&host.api) orelse return error.InitReturnedNull;

    if (desc.api_min != probe.api_min or desc.api_max != probe.api_max) return error.ProbeInitMismatch;
    const probe_name = std.mem.span(probe.name);
    const desc_name = std.mem.span(desc.name);
    if (!std.mem.eql(u8, probe_name, desc_name)) return error.ProbeInitMismatch;
    const probe_version = std.mem.span(probe.version);
    const desc_version = std.mem.span(desc.version);
    if (!std.mem.eql(u8, probe_version, desc_version)) return error.ProbeInitMismatch;

    const desc_name_for_owner = std.mem.span(desc.name);
    host.setCurrentOwner(desc_name_for_owner) catch return error.RegisterFailed;
    const rc = desc.register_plugin(&host.api);
    host.clearCurrentOwner();
    if (rc != 0) return error.RegisterFailed;

    const session_begin = if (desc.api_max >= 3) desc.session_begin else null;
    const session_end = if (desc.api_max >= 3) desc.session_end else null;
    host.addLoadedPlugin(desc_name, session_begin, session_end) catch return error.RegisterFailed;

    return .{
        .name = alloc.dupe(u8, desc_name) catch return error.RegisterFailed,
        .version = alloc.dupe(u8, desc_version) catch return error.RegisterFailed,
        .api_min = desc.api_min,
        .api_max = desc.api_max,
    };
}
