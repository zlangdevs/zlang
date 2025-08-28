#!/bin/bash

# Use this script to format your code before every commit, and ensure that all tests are passed, and nothing broke
zig fmt .
./run_tests.sh
cd examples/multi_file_c
./build.sh