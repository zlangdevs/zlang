#include "../../include/zlang_plugin_api_v1.h"

static const char generated[] = "fun zlx_source_map_broken() >> i32 {\n    i32 = ;\n    return 0;\n}\n";
static const ZlangSourceMapEntry map_entries[] = {
    {0, 7, 3},
};

static int brokenmap_handler(ZlangHostApi* host, const ZlangBlockInput* input, ZlangBlockOutput* output) {
    (void)host;
    (void)input;
    output->generated_zlang_source = generated;
    output->generated_zlang_source_len = (uint32_t)(sizeof(generated) - 1);
    output->source_map = map_entries;
    output->source_map_len = 1;
    return 0;
}

static int register_plugin(ZlangHostApi* host) {
    ZlangBlockSyntax syntax = {
        .mode = ZLANG_DELIMITER_BRACE_COUNTING,
        .terminator = 0,
    };
    return host->register_syntax_block(host, "brokenmap", &syntax, brokenmap_handler);
}

static ZlangProbeResult probe = {
    .api_min = 1,
    .api_max = 3,
    .name = "source_map_test",
    .version = "0.1.0",
    .requires_host_features = 0,
};

static ZlangPluginDesc desc = {
    .api_min = 1,
    .api_max = 3,
    .name = "source_map_test",
    .version = "0.1.0",
    .register_plugin = register_plugin,
};

ZlangProbeResult* zlang_plugin_probe(uint32_t host_api_version) {
    (void)host_api_version;
    return &probe;
}

ZlangPluginDesc* zlang_plugin_init(ZlangHostApi* host) {
    (void)host;
    return &desc;
}
