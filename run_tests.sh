#!/bin/bash

TEST_DIR="./examples/tests"
APP="./zig-out/bin/lang"
FAILED=0

zig build

echo "Running tests"
echo "-------------------------------"

for test_file in "$TEST_DIR"/*; do
  $APP "$test_file"
  if $APP "$test_file"; then
    echo "File $test_file passed!"
  else
    echo "File $test_file failed!"
    ((FAILED++))
  fi
done

echo "-------------------------------"

echo "Total failed: $FAILED"
