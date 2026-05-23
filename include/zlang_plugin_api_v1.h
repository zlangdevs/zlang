#ifndef ZLANG_PLUGIN_API_V1_H
#define ZLANG_PLUGIN_API_V1_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ZLANG_PLUGIN_API_VERSION 4u

typedef struct ZlangHostApi ZlangHostApi;
typedef struct ZlangPluginDesc ZlangPluginDesc;
typedef struct ZlangProbeResult ZlangProbeResult;
typedef struct ZlangBlockSyntax ZlangBlockSyntax;
typedef struct ZlangBlockInput ZlangBlockInput;
typedef struct ZlangBlockOutput ZlangBlockOutput;
typedef struct ZlangSourceMapEntry ZlangSourceMapEntry;
typedef struct ZlangFileExtensionRequest ZlangFileExtensionRequest;
typedef struct ZlangFileExtensionResult ZlangFileExtensionResult;

typedef enum {
    ZLANG_DIAGNOSTIC_ERROR = 1,
    ZLANG_DIAGNOSTIC_WARNING = 2,
    ZLANG_DIAGNOSTIC_NOTE = 3
} ZlangDiagnosticLevel;

typedef enum {
    ZLANG_DELIMITER_BRACE_COUNTING = 1,
    ZLANG_DELIMITER_CUSTOM_TERMINATOR = 2
} ZlangDelimiterMode;

typedef enum {
    ZLANG_REGISTER_OK = 0,
    ZLANG_REGISTER_DUPLICATE = 1,
    ZLANG_REGISTER_INVALID = 2,
    ZLANG_REGISTER_UNSUPPORTED = 3
} ZlangRegisterResult;

struct ZlangBlockSyntax {
    ZlangDelimiterMode mode;
    const char* terminator;
};

struct ZlangBlockInput {
    const char* file;
    uint32_t line;
    uint32_t column;
    const char* raw_source;
    uint32_t raw_source_len;
};

struct ZlangBlockOutput {
    const char* generated_zlang_source;
    uint32_t generated_zlang_source_len;
    /* api v3: optional mapping from generated source byte offsets to original source locations. */
    const ZlangSourceMapEntry* source_map;
    uint32_t source_map_len;
};

struct ZlangSourceMapEntry {
    uint32_t generated_offset;
    uint32_t original_line;
    uint32_t original_column;
};

typedef int (*ZlangBlockHandler)(
    ZlangHostApi* host,
    const ZlangBlockInput* input,
    ZlangBlockOutput* output
);

struct ZlangHostApi {
    uint32_t api_version;

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
        uint32_t line,
        uint32_t column,
        const char* message,
        const char* hint
    );
    /* api v2: resolve type size for given file context. Returns -1 if unknown. */
    int32_t (*resolve_type_size)(
        ZlangHostApi* host,
        const char* file,
        const char* type_name
    );
    /* api v3: returns NULL if absent, "" for bare flags, or the --flag=value value. */
    const char* (*get_cli_flag)(
        ZlangHostApi* host,
        const char* name
    );
    /* api v4: register handler for a custom file extension (e.g. ".zlb"). */
    int (*register_file_extension)(
        ZlangHostApi* host,
        const char* extension,
        int (*handler)(ZlangHostApi* host,
                       const struct ZlangFileExtensionRequest* req,
                       struct ZlangFileExtensionResult* res)
    );
};

struct ZlangFileExtensionRequest {
    const char* input_path;
    const char* output_path;     /* user's -o, or NULL */
    int32_t want_continue;       /* 1 if -c was passed */
};

struct ZlangFileExtensionResult {
    const char* continue_path;   /* if set, replaces input in pipeline */
};

struct ZlangProbeResult {
    uint32_t api_min;
    uint32_t api_max;
    const char* name;
    const char* version;
    const char* const* requires_host_features;
};

struct ZlangPluginDesc {
    uint32_t api_min;
    uint32_t api_max;
    const char* name;
    const char* version;
    int (*register_plugin)(ZlangHostApi* host);
    /* api v3: optional per-compilation lifecycle callbacks. */
    void (*session_begin)(ZlangHostApi* host);
    void (*session_end)(ZlangHostApi* host);
};

ZlangProbeResult* zlang_plugin_probe(uint32_t host_api_version);
ZlangPluginDesc* zlang_plugin_init(ZlangHostApi* host);

#ifdef __cplusplus
}
#endif

#endif
