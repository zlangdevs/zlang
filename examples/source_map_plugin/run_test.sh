#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

clang -shared -fPIC -Iinclude -o examples/source_map_plugin/lib/plugin.so examples/source_map_plugin/plugin.c
./zig-out/bin/zlang module pack examples/source_map_plugin -o /tmp/source_map_test.zlx >/dev/null
./zig-out/bin/zlang module install /tmp/source_map_test.zlx >/dev/null

set +e
OUTPUT=$(./zig-out/bin/zlang examples/source_map_plugin/source_map_error.zl -o /tmp/source_map_error_bin 2>&1)
STATUS=$?
set -e

if [ "$STATUS" -eq 0 ]; then
    printf '%s\n' "expected compilation to fail"
    exit 1
fi

if ! printf '%s\n' "$OUTPUT" | grep -q "examples/source_map_plugin/source_map_error.zl:7:3"; then
    printf '%s\n' "$OUTPUT"
    printf '%s\n' "expected diagnostic at source_map_error.zl:7:3"
    exit 1
fi

if printf '%s\n' "$OUTPUT" | grep -q "/tmp/zlang-zlx-"; then
    printf '%s\n' "$OUTPUT"
    printf '%s\n' "diagnostic leaked generated temp path"
    exit 1
fi

printf '%s\n' "source-map diagnostic test passed"
