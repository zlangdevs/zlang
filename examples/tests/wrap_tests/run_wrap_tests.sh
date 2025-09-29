#!/usr/bin/env bash
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

APP="./zig-out/bin/zlang"
HEADER="examples/tests/wrap_tests/simple.h"
EXPECTED="examples/tests/wrap_tests/simple_expected.zl"
GENERATED="examples/tests/wrap_tests/simple_generated.zl"

echo -e "${YELLOW}Building compiler...${NC}"
zig build

echo -e "${YELLOW}Running wrap generator...${NC}"
$APP wrap "$HEADER" -o "$GENERATED"

echo -e "${YELLOW}Comparing generated wrapper with expected...${NC}"
if diff -u "$EXPECTED" "$GENERATED"; then
  echo -e "${GREEN}PASS: wrapper output matches expected${NC}"
  rm -f "$GENERATED"
  exit 0
else
  echo -e "${RED}FAIL: wrapper output differs from expected${NC}"
  echo -e "${YELLOW}Generated at: $GENERATED${NC}"
  exit 1
fi