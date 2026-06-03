#ifndef ZLX_PLUGIN_SDK_H
#define ZLX_PLUGIN_SDK_H

#include <stdint.h>
#include <string.h>

#include "zlang_plugin_api_v1.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================
 * ZLX Plugin SDK — convenience wrappers over the raw ZlangHostApi
 * function-pointer struct. Plugin authors include this header and
 * link with -lpthread (zlang itself is loaded as the host, no .a needed).
 * ============================================================ */

/* --- Syntax block registration (brace-counting is the common case) --- */

static inline int zlx_register_syntax(
    ZlangHostApi* host,
    const char* name,
    ZlangBlockHandler handler
) {
    ZlangBlockSyntax syntax = { ZLANG_DELIMITER_BRACE_COUNTING, NULL };
    return host->register_syntax_block(host, name, &syntax, handler);
}

static inline int zlx_register_keyword(
    ZlangHostApi* host,
    const char* name,
    ZlangBlockHandler handler
) {
    ZlangBlockSyntax syntax = { ZLANG_DELIMITER_BRACE_COUNTING, NULL };
    return host->register_keyword_block(host, name, &syntax, handler);
}

/* --- Diagnostics --- */

static inline void zlx_diag(
    ZlangHostApi* host,
    ZlangDiagnosticLevel level,
    const char* file,
    uint32_t line,
    uint32_t column,
    const char* message,
    const char* hint
) {
    host->diagnostic(host, level, file, line, column, message, hint);
}

#define ZLX_DIAG_ERROR(host, file, line, col, msg) \
    zlx_diag((host), ZLANG_DIAGNOSTIC_ERROR, (file), (line), (col), (msg), NULL)

#define ZLX_DIAG_WARN(host, file, line, col, msg) \
    zlx_diag((host), ZLANG_DIAGNOSTIC_WARNING, (file), (line), (col), (msg), NULL)

#define ZLX_DIAG_NOTE(host, file, line, col, msg) \
    zlx_diag((host), ZLANG_DIAGNOSTIC_NOTE, (file), (line), (col), (msg), NULL)

#define ZLX_DIAG_ERROR_HINT(host, file, line, col, msg, hint) \
    zlx_diag((host), ZLANG_DIAGNOSTIC_ERROR, (file), (line), (col), (msg), (hint))

/* --- Module / link / help / CLI flag registration --- */

static inline int zlx_register_module(
    ZlangHostApi* host,
    const char* name,
    const char* path
) {
    return host->register_module(host, name, path);
}

static inline int zlx_register_link_flag(
    ZlangHostApi* host,
    const char* flag
) {
    return host->register_link_flag(host, flag);
}

static inline int zlx_register_help(
    ZlangHostApi* host,
    const char* name,
    const char* text
) {
    return host->register_help_section(host, name, text);
}

static inline int zlx_register_cli_flag(
    ZlangHostApi* host,
    const char* name,
    const char* description,
    int mandatory
) {
    return host->register_cli_flag(host, name, description, mandatory);
}

static inline const char* zlx_cli_flag(
    ZlangHostApi* host,
    const char* name
) {
    return host->get_cli_flag(host, name);
}

static inline int32_t zlx_resolve_type_size(
    ZlangHostApi* host,
    const char* file,
    const char* type_name
) {
    if (!host->resolve_type_size) return 0;
    return host->resolve_type_size(host, file, type_name);
}

/* --- Block output writers --- */

static inline int zlx_emit(
    ZlangBlockOutput* output,
    const char* text,
    uint32_t len
) {
    output->generated_zlang_source = text;
    output->generated_zlang_source_len = len;
    return 0;
}

static inline int zlx_emit_cstr(
    ZlangBlockOutput* output,
    const char* text
) {
    return zlx_emit(output, text, (uint32_t)strlen(text));
}

static inline int zlx_emit_empty(ZlangBlockOutput* output) {
    output->generated_zlang_source = "";
    output->generated_zlang_source_len = 0;
    output->source_map = NULL;
    output->source_map_len = 0;
    return 0;
}

/* --- Input accessors --- */

static inline const char* zlx_input_file(const ZlangBlockInput* input) {
    return input->file;
}

static inline uint32_t zlx_input_line(const ZlangBlockInput* input) {
    return input->line;
}

static inline uint32_t zlx_input_column(const ZlangBlockInput* input) {
    return input->column;
}

static inline const char* zlx_input_raw(const ZlangBlockInput* input) {
    return input->raw_source;
}

static inline uint32_t zlx_input_raw_len(const ZlangBlockInput* input) {
    return input->raw_source_len;
}

/* Convenience: returns a NUL-terminated copy of the raw input.
 * Caller owns the returned buffer and must free() it. */
static inline char* zlx_input_dup(const ZlangBlockInput* input) {
    char* copy = (char*)malloc((size_t)input->raw_source_len + 1);
    if (!copy) return NULL;
    memcpy(copy, input->raw_source, input->raw_source_len);
    copy[input->raw_source_len] = '\0';
    return copy;
}

/* In-place trim: returns a pointer into the original buffer, advances start,
 * writes '\0' at the new end. The returned slice aliases the input. */
static inline char* zlx_trim(char* s) {
    if (!s) return s;
    while (*s && (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n')) s++;
    char* end = s + strlen(s);
    while (end > s && (end[-1] == ' ' || end[-1] == '\t' || end[-1] == '\r' || end[-1] == '\n')) end--;
    *end = '\0';
    return s;
}

#ifdef __cplusplus
}
#endif

#endif
