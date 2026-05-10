# ZLang Extensions Roadmap (`.zlx`)

## 1. Vision

Goal: make `zlang` modular so language/runtime features are installable extensions.

Examples:
- `brainfuck.zlx`: adds `brainfuck { ... }` blocks and related CLI flags.
- `threading.zlx`: adds thread/sync stdlib modules and native runtime linking.
- `zlisp.zlx`: adds lisp-like embedded blocks.

Core principles:
- One extension = one feature.
- Installed once, available globally.
- Stable plugin ABI with explicit compatibility handshake.
- No hard dependency on internal compiler structs.

## 1.1 Implementation status

Done on branch `zlx`:
- `EXTENSIONS_ROADMAP.md` is tracked in git and pushed to GitHub on branch `zlx`.
- Added `src/zlx/manifest.zig` with v1 ZON manifest parsing and validation.
- Added `src/zlx/store.zig` for the user module store at `~/.zlang/modules`.
- Added `src/zlx/index.zig` for `~/.zlang/modules/index.zon`.
- Added CLI commands: `install`, `list-modules`, `del-module`, `validate-module`, `module-info`.
- Added current-target compatibility detection; incompatible packages are indexed as `incompatible`.
- Added dependency recording in `index.zon`; missing dependencies mark packages as `incompatible` with a reason.
- Added dependency cycle detection and `module-load-order` for future plugin initialization order.
- Added `src/zlx/package.zig` package layout abstraction; current `single_manifest` layout reads the whole `.zlx` as a ZON manifest, with archive layouts reserved for v2.
- `module-info` now reports the detected `layout`.
- Added `include/zlang_plugin_api_v1.h` with the v1 plugin/host C ABI types (no native loading yet).
- Added `src/zlx/abi.zig`, the Zig mirror of the v1 ABI, exposing `api_version`, supported range, entry-symbol names, and an `checkApiRange` helper.
- Added `zlang module-abi` to print the host's supported ABI range and entry symbols for plugin authors.
- Added `src/zlx/host.zig`: a concrete `HostApi` instance with C-callable stubs that record syntax-block, module, CLI-flag, link-flag, help-section registrations and diagnostics, with duplicate detection.
- Added `zlang module-dryrun <file.zlx>`: simulates plugin-side registrations from manifest data through the host stubs and reports counts/duplicates. Validates `api_min/api_max` against the host's supported range before simulating.
- Added `src/zlx/loader.zig`: `std.DynLib`-backed loader that opens a `.so`, resolves `zlang_plugin_probe` and `zlang_plugin_init`, validates the probe API range, calls init, checks the desc/probe metadata agree, and invokes `register_plugin` against the host stubs.
- Added `zlang module-load <file.so>`: loads a plugin and prints registration counts/diagnostics.
- Added `tests/plugins/dummy_plugin.zig`: reference plugin exercising every host stub (compile with `zig build-lib -dynamic -fPIC tests/plugins/dummy_plugin.zig`).
- `zlang install` now also installs a sidecar `.so` next to the input `.zlx` (same stem) into `~/.zlang/modules/<name>.so`; `del-module` removes it.
- Added `zlang module-loadall`: walks the index in topological load order, loads every plugin with a sidecar through the host stubs, and reports per-plugin status plus aggregated counts.
- Added `src/zlx/runtime.zig`: `loadAllInstalled` populates a host from every installed plugin sidecar; `printPluginExtensions` formats the host's CLI flags, syntax blocks, modules and help sections.
- `zlang help` now appends a `Plugin extensions:` section listing what installed plugins register, so plugin authors can see the host saw their registrations without writing tests.
- Plugin-registered link flags are now merged into `ctx.extra_args` after `parseArgs`, so the linker invocation includes flags contributed by installed plugins (verified with the dummy plugin registering `-lm`).
- Current MVP treats `.zlx` as a single ZON manifest file copied to `~/.zlang/modules/<name>.zlx`.

Not done yet:
- Real `.zlx` container archive unpacking.
- Native plugin `.so` probing/loading.
- Versioned dependency constraints.
- Parser extension-block dispatch.
- Extension-provided stdlib module search paths.
- Link flag activation by feature usage.

Next planned increments:
- Pipe plugin-registered CLI flags into the main argument parser so `zlang -dummy` is consumed by the plugin instead of erroring as an unknown flag.
- Activate plugin link flags by feature use rather than unconditionally (RFC §5.4): only inject flags when the build actually uses the plugin's syntax block, module, or CLI flag.
- Extend the package layout abstraction with a real container layout (archive extraction) so `entry`/`std/*.zl` paths are extracted from the `.zlx` instead of relying on a manifest-side sidecar `.so`.

---

## 2. `.zlx` format

`zlx` is a package/container, not just a raw binary.

Recommended package layout:

```text
brainfuck.zlx
├── manifest.zon
├── lib/plugin.so
├── std/brainfuck.zl
├── docs/README.md
└── examples/hello_bf.zl
```

### `manifest.zon` (v1 draft)

```zig
.{
    .name = "brainfuck",
    .version = "1.0.0",

    // Package format version. This is independent from plugin ABI version.
    // Compiler uses it to parse/validate manifest and package layout.
    .format_version = 1,

    // Plugin ABI compatibility with compiler host API
    .api_min = 1,
    .api_max = 1,

    // Native platform compatibility for bundled plugin binary.
    .targets = .{
        "linux-x86_64",
    },

    // Optional dependencies on other installed extensions.
    .dependencies = .{},

    // Native plugin entry
    .entry = "lib/plugin.so",

    // Capabilities provided by this package.
    .provides = .{
        "syntax.block",
        "cli.flags",
        "help",
        "link.flags",
        "stdlib.modules",
    },

    // Optional stdlib modules exposed by this extension
    .modules = .{
        .{ .name = "brainfuck", .path = "std/brainfuck.zl" },
    },

    .syntax_blocks = .{
        .{ .name = "brainfuck", .delimiter_mode = .brace_counting, .mandatory = true },
    },

    .cli_flags = .{
        .{ .name = "-b", .value = "cell_size=8", .mandatory = true },
        .{ .name = "-b8", .value = "cell_size=8", .mandatory = true },
        .{ .name = "-b16", .value = "cell_size=16", .mandatory = true },
        .{ .name = "-b32", .value = "cell_size=32", .mandatory = true },
        .{ .name = "-b64", .value = "cell_size=64", .mandatory = true },
    },

    // Optional native libraries shipped in package
    .native_libs = .{},
    .link_flags = .{},

    // Exposure policy
    .expose = .{
        .global_syntax = true,
        .global_modules = false,
    },
}
```

Rules:
- `format_version` controls package layout and manifest parsing.
- `api_min/api_max` controls runtime plugin ABI compatibility.
- Manifest API fields are used for pre-load filtering only. The loaded native binary descriptor is the source of truth. If manifest and binary disagree, the plugin is marked `broken`.
- Native plugin packages must declare supported `.targets`.
- Dependencies in v1 are a flat list of extension names without version constraints. Cycles are invalid. Load order is topological. Versioned dependencies are deferred to v2.
- Current implementation records flat dependencies in `index.zon`; if any declared dependency is not installed and compatible, the module is indexed as `incompatible: missing dependency`.
- Current implementation marks dependency cycles as `incompatible: dependency cycle` and exposes a topological order for loadable modules through `zlang module-load-order`.
- `native_libs` and `link_flags` are not applied globally just because a package is installed. They are applied only when the extension is used by the current compilation unit or explicitly marks itself as always-required.

---

## 3. CLI lifecycle

Planned commands:

```bash
zlang install ./brainfuck.zlx
zlang list-modules
zlang module-load-order
zlang module-info ./brainfuck.zlx
zlang module-abi
zlang module-dryrun ./brainfuck.zlx
zlang module-load ./libbrainfuck.so
zlang module-loadall
zlang validate-module ./brainfuck.zlx
zlang del-module brainfuck
```

Install locations:
- User: `~/.zlang/modules/<name>/...`
- Optional system-wide: `/usr/share/zlang/modules/<name>/...`

Installed index file (example):

```text
~/.zlang/modules/index.zon
```

---

## 4. Plugin ABI and handshake

### 4.1 Entry point

Each plugin exports two C ABI symbols:

```c
ZlangProbeResult* zlang_plugin_probe(uint32_t host_api_version);
```

`probe` is side-effect-light and returns metadata needed for compatibility checks before full initialization.

After probe succeeds, compiler calls:

```c
ZlangPluginDesc* zlang_plugin_init(ZlangHostApi* host);
```

Lifecycle:
- `probe` returns ABI range, plugin name/version, and required host features.
- Host validates manifest vs probe metadata.
- Host calls `init` only after probe succeeds.
- `init` returns plugin descriptor and registration callback.
- Host validates global conflicts/dependencies.
- Host calls `desc.register_plugin(host)` to perform registrations.
- `ZlangPluginDesc.api_min/api_max` must match probe result; mismatch marks plugin `broken`.

### 4.2 Handshake checks

Compiler validates before enabling plugin:
- ABI version range (`api_min/api_max` vs host API version).
- Required feature set.
- Target/platform constraints (optional manifest fields).
- Declared dependencies are installed and compatible.
- Probe and init entry point availability.

If mismatch: plugin is listed as `incompatible` with reason.

### 4.3 Safety rule

Plugin does not access internal `zlang` compiler structs directly.
All interactions go through `ZlangHostApi` function table.

---

## 5. Extension points in compiler

## 5.1 CLI API
- Register new flags.
- Register custom help section.
- Optional custom subcommands in later versions.

## 5.2 Syntax API
- Register block language handlers, e.g. `brainfuck { ... }`.
- Register expression-style block handlers later.
- Register raw block delimiter mode.

Important parser requirement:
- Core grammar must support a generic statement-position extension-block path (`IDENTIFIER { ... }`).
- Parser delegates content to registered plugin handler.

Rules:
- Extension blocks are recognized only in statement position in v1.
- A block name is valid only if an installed plugin registered it as global syntax.
- If multiple extensions register the same block name, compiler disables the later block registration as conflicting.
- If an extension block name conflicts with a core language keyword, core syntax wins and that plugin block registration is disabled.

Raw block contract:
- Compiler passes plugin the raw bytes inside delimiters, without the delimiters.
- Line/column of block start is provided separately.
- In `brace_counting` mode, nested braces are counted naively and strings/comments are not interpreted by zlang parser. This is suitable for simple syntaxes such as brainfuck.
- In `custom_terminator` mode, plugin declares an opening form and terminator. Compiler captures raw bytes until terminator. This is intended for languages where `}` can appear in strings/comments.
- Plugins that need language-aware parsing must parse their raw text themselves.

Example delimiter modes:

```text
brainfuck { ... }                  // brace_counting
lua [[ ... ]]                      // custom_terminator
zlisp #zlx( ... )zlx#              // custom_terminator
```

## 5.3 Stdlib/module API
- Register module names and source paths from extension package.
- Optional auto-exposure policy.

## 5.4 Linking API
- Register persistent link flags and native libs for compilation units using extension features.
- Link data is activated by use, not by installation.
- Example activation sources: extension block used, extension module imported, extension CLI flag used.

## 5.5 Diagnostics API
- Emit plugin errors/warnings with file/line/column and hints.

## 5.6 Conflict handling
- Duplicate module name: disable only that module registration unless exact same package/version already owns it.
- Duplicate CLI flag: disable only that flag registration.
- Duplicate syntax block: disable only that block registration.
- Duplicate help section id: merge only if owned by same plugin; otherwise disable only that help section.
- Duplicate native library filename inside one package: invalid package.
- If a plugin requires one of the failed registrations for correctness, it can mark that registration as mandatory; then the whole plugin becomes `incompatible`.

Status model:
- `loaded`: plugin loaded and all mandatory registrations succeeded.
- `partial`: plugin loaded but some optional registrations were disabled.
- `incompatible`: API/features/target/dependencies do not match.
- `broken`: manifest and binary disagree, entry points are missing, or package is malformed.

---

## 6. Usage model

### 6.1 Brainfuck extension

After install:

```zl
fun main() >> i32 {
    brainfuck {
        ++++++++[>++++++++<-]>+.
    }
    return 0;
}
```

No extra `use`/`include` required when `global_syntax = true`.

### 6.2 Threading extension (`threading.zlx`)

Package can include:
- `std/thread.zl`, `std/sync.zl`
- native runtime libs
- `-lpthread` flags

Usage:

```zl
use thread

fun worker(arg: ptr<void>) >> ptr<void> {
    return null;
}

fun main() >> i32 {
    Thread t;
    thread.spawn(&t, worker, null);
    thread.join(t);
    return 0;
}
```

Optional syntax additions (later):

```zl
task {
    heavyWork();
}
await_all();
```

---

## 7. Realistic phased roadmap

## Phase 0: RFC and scope freeze (1-2 weeks)
- Finalize this design as `v1` RFC.
- Freeze `.zlx` manifest fields.
- Freeze ABI policy and compatibility rules.

Deliverable: approved extension RFC.

## Phase 1: Package management (1-2 weeks)
- Implement `install/list/del`.
- Validate manifests.
- Store installed module index.

Deliverable: extensions install/uninstall, no runtime loading yet.

## Phase 2: Loader + handshake (2 weeks)
- Load plugin `.so`.
- Call `zlang_plugin_init`.
- Validate API/features and track status.

Deliverable: modules can be discovered and activated safely.

## Phase 3: Host API v1 (2-3 weeks)
- CLI/help registration.
- Module registration.
- Link flag registration.
- Diagnostics callbacks.

Deliverable: hello-world plugin changes `zlang help` and adds a flag.

## Phase 4: Parser generic extension blocks (2-4 weeks)
- Add `IDENTIFIER { ... }` extension path.
- Delegate block content to plugin handler.

Deliverable: external plugin can add `brainfuck { ... }` syntax.

## Phase 5: Codegen bridge MVP (2-3 weeks)
- Plugin block handler returns generated zlang source fragment.
- Generated fragment is parsed and type-checked by normal zlang pipeline.
- IR fragments, direct AST builders, and expression blocks are deferred to later API versions.
- Integrate with current compile flow and diagnostics.

Deliverable: plugin-generated code compiles and runs.

## Phase 6: Real extension validation (2-4 weeks)
- Validate architecture against two different plugins before removing core behavior.
- Brainfuck validates global syntax blocks and generated source fragments.
- Threading validates stdlib modules, link flags, native runtime integration, and platform handling.

Deliverable: both `brainfuck.zlx` and `threading.zlx` work as external extensions in parallel with existing core features.

## Phase 7: Brainfuck extraction (1-2 weeks)
- Move current built-in brainfuck integration to `brainfuck.zlx`.
- Keep temporary backward compatibility shim for one release if needed.

No-loss acceptance criteria before removing built-in Brainfuck from clean zlang:
- `brainfuck { ... }` inside `.zl` works through `brainfuck.zlx` with the same directives currently supported by core codegen.
- Standalone `.b/.bf` compilation works through extension CLI flags `-b`, `-b8`, `-b16`, `-b32`, `-b64`.
- Cell size behavior is unchanged for 8/16/32/64-bit modes.
- Existing BF optimization behavior behind `-optimize` has an equivalent extension path or is explicitly preserved through a host API hook.
- Existing examples/tests that cover embedded and standalone BF pass against `brainfuck.zlx`.
- Built-in AST/parser/codegen BF branches are removed only after `brainfuck.zlx` and at least one non-BF extension validate the extension architecture.

Deliverable: brainfuck works as external extension.

## Phase 8: Library-style extension hardening (2-4 weeks)
- Harden `threading.zlx` with modules + native link flags.
- Validate install/load/build/run path on real examples.

Deliverable: first serious feature extension beyond syntax.

## Phase 9: Security and trust (1-2 weeks)
- Module integrity checks (hash/signature optional).
- Clear trust and loading policy.
- `zlang doctor-modules` diagnostics.

Deliverable: production-safe extension story.

Important security note:
- Native `.so` extensions are trusted code.
- They run in the compiler process and can do anything the current user can do.
- v1 does not sandbox native plugins.
- Safety is based on install-time trust, manifest validation, hashes/signatures, and clear diagnostics.
- `--no-extensions` disables native `.so` plugin loading but keeps pure `.zl` extension module paths available.
- `--isolated` disables native plugins and extension module paths for fully reproducible builds.

## Phase 10: Stabilization and compatibility policy (ongoing)
- ABI semver policy.
- CI matrix for core + official extensions.
- Regression suites for parser/codegen/linking via plugins.

Deliverable: long-term maintainable extension ecosystem.

---

## 8. Risks and mitigations

Risk: plugin ABI churn breaks ecosystem.
- Mitigation: stable C ABI and versioned host API.

Risk: parser complexity explosion.
- Mitigation: single generic extension-block syntax path.

Risk: plugin can destabilize compiler.
- Mitigation: constrained host API + strict handshake + diagnostics isolation.

Risk: native plugin is malicious or buggy.
- Mitigation: treat installed `.zlx` native code as trusted; add hashes/signatures and `--no-extensions` for reproducible/debug builds.

Risk: linking differences across distros.
- Mitigation: extension-declared native requirements + host resolver utilities.

---

## 9. MVP acceptance criteria

`v1` is considered successful when:
- `zlang install brainfuck.zlx` works.
- `brainfuck { ... }` works without core hardcode.
- `brainfuck { ... }` is implemented by returning a generated zlang source fragment.
- `threading.zlx` validates modules + link flags + native runtime integration.
- `zlang list-modules` shows compatibility status and reasons.
- `zlang del-module brainfuck` cleanly removes feature.
- `zlang --no-extensions ...` disables native plugins but keeps pure extension modules available.
- `zlang --isolated ...` disables native plugins and extension module paths.
- Existing non-extension builds continue to work.

Explicitly not in v1:
- Direct AST builder API.
- Direct LLVM IR plugin API.
- Expression-position extension blocks.
- Sandboxed plugin execution.
- Custom plugin subcommands beyond module management.

---

## 10. Minimal API sketch

This is illustrative, not final ABI.

```c
#define ZLANG_PLUGIN_API_VERSION 1

typedef struct ZlangHostApi ZlangHostApi;
typedef struct ZlangPluginDesc ZlangPluginDesc;
typedef struct ZlangProbeResult ZlangProbeResult;

typedef enum {
    ZLANG_DIAGNOSTIC_ERROR = 1,
    ZLANG_DIAGNOSTIC_WARNING = 2,
    ZLANG_DIAGNOSTIC_NOTE = 3,
} ZlangDiagnosticLevel;

typedef enum {
    ZLANG_DELIMITER_BRACE_COUNTING = 1,
    ZLANG_DELIMITER_CUSTOM_TERMINATOR = 2,
} ZlangDelimiterMode;

typedef struct {
    ZlangDelimiterMode mode;
    const char* terminator;
} ZlangBlockSyntax;

typedef struct {
    const char* file;
    unsigned line;
    unsigned column;
    const char* raw_source;
    unsigned raw_source_len;
} ZlangBlockInput;

typedef struct {
    const char* generated_zlang_source;
    unsigned generated_zlang_source_len;
} ZlangBlockOutput;

typedef int (*ZlangBlockHandler)(
    ZlangHostApi* host,
    const ZlangBlockInput* input,
    ZlangBlockOutput* output
);

struct ZlangHostApi {
    unsigned api_version;

    int (*register_syntax_block)(
        ZlangHostApi* host,
        const char* name,
        const ZlangBlockSyntax* syntax,
        ZlangBlockHandler handler
    );

    int (*register_help_section)(
        ZlangHostApi* host,
        const char* section_id,
        const char* text
    );

    int (*register_cli_flag)(
        ZlangHostApi* host,
        const char* flag_name,
        const char* help_text,
        int mandatory
    );

    int (*register_module)(
        ZlangHostApi* host,
        const char* module_name,
        const char* package_relative_path
    );

    int (*register_link_flag)(
        ZlangHostApi* host,
        const char* flag
    );

    void (*diagnostic)(
        ZlangHostApi* host,
        ZlangDiagnosticLevel level,
        const char* file,
        unsigned line,
        unsigned column,
        const char* message,
        const char* hint
    );
};

struct ZlangProbeResult {
    unsigned api_min;
    unsigned api_max;
    const char* name;
    const char* version;
    // NULL-terminated list of host API features required by this plugin.
    const char* const* requires_host_features;
};

struct ZlangPluginDesc {
    unsigned api_min;
    unsigned api_max;
    const char* name;
    const char* version;
    // Called only after probe/init compatibility checks and conflict planning.
    int (*register_plugin)(ZlangHostApi* host);
};

ZlangProbeResult* zlang_plugin_probe(unsigned host_api_version);
ZlangPluginDesc* zlang_plugin_init(ZlangHostApi* host);
```

Lifecycle rules:
- Host copies all strings passed to registration and diagnostics callbacks immediately.
- Plugin-owned strings only need to stay valid until the callback returns.
- `host` pointer passed to callbacks is the same host instance received by `zlang_plugin_init` in v1.
- Compiler loads plugins once per process and does not unload `.so` files during process lifetime.
- `ZlangPluginDesc.api_min/api_max/name/version` must match `ZlangProbeResult`; mismatch marks plugin `broken`.

---

## 11. Next implementation artifact

After this roadmap, next concrete artifact should be:

`include/zlang_plugin_api_v1.h`

with minimal stable ABI definitions for:
- plugin descriptor,
- host API table,
- registration callbacks,
- diagnostics interface.
