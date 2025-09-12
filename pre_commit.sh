#!/bin/bash

# Use this script to format your code before every commit, and ensure that all tests are passed, and nothing broke
clear
zig fmt .
clear
./run_tests.sh
./zig-out/bin/zlang examples/brainfuck.zl -optimize
cat thirdparties/mandelbrot.bf | ./output
echo "++++++[>++++++++<-]>.[-]++++++++++.." | ./output 
cd examples/multi_file_c
./build.sh