#!/bin/bash

# Build script for multi-file C integration example
# This script builds the ZLang multi-file project with C integration

# Configuration - Change these paths as needed for your system
CCOMPILER_PATH="gcc"  # Path to C compiler (gcc, clang, etc.)

# Auto-detect zlang compiler path
if [ -f "../../../zig-out/bin/zlang" ]; then
    ZLANG_PATH="$(cd ../../../zig-out/bin && pwd)/zlang"
elif [ -f "/home/hedgegod/zig/zlang/zig-out/bin/zlang" ]; then
    ZLANG_PATH="/home/hedgegod/zig/zlang/zig-out/bin/zlang"  # Fallback for current setup
else
    echo -e "${RED}Error: Could not find zlang compiler${NC}"
    echo -e "${YELLOW}Please set ZLANG_PATH manually in this script${NC}"
    echo -e "${YELLOW}Example: ZLANG_PATH=\"/path/to/your/zlang/compiler\"${NC}"
    exit 1
fi

# Alternative: Uncomment and modify these lines if auto-detection doesn't work
# ZLANG_PATH="/absolute/path/to/zlang/compiler"
# CCOMPILER_PATH="clang"  # or "gcc"

# Project configuration
OUTPUT_NAME="multi_file_c_example"       # Output executable name
SOURCE_FILES="main.zl wrapper.zl"        # ZLang source files
LINK_OBJECTS="cfunc"                     # C object files to link
KEEP_LL=false                           # Set to true to keep LLVM IR files

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building multi-file C integration example...${NC}"

# Check if zlang compiler exists
if [ ! -f "$ZLANG_PATH" ]; then
    echo -e "${RED}Error: zlang compiler not found at $ZLANG_PATH${NC}"
    echo -e "${YELLOW}Please update ZLANG_PATH in this script or build zlang first${NC}"
    exit 1
fi

# Check if C compiler exists
if ! command -v "$CCOMPILER_PATH" &> /dev/null; then
    echo -e "${RED}Error: C compiler '$CCOMPILER_PATH' not found${NC}"
    echo -e "${YELLOW}Please update CCOMPILER_PATH in this script${NC}"
    exit 1
fi

echo -e "${YELLOW}Using zlang compiler: $ZLANG_PATH${NC}"
echo -e "${YELLOW}Using C compiler: $CCOMPILER_PATH${NC}"

# Step 1: Build C object file
echo -e "${GREEN}Step 1: Building C object file...${NC}"
if [ ! -f "cfunc.c" ]; then
    echo -e "${RED}Error: cfunc.c not found${NC}"
    exit 1
fi

$CCOMPILER_PATH -c cfunc.c -o cfunc
if [ $? -ne 0 ]; then
    echo -e "${RED}Error: Failed to compile cfunc.c${NC}"
    exit 1
fi
echo -e "${GREEN}✓ C object file 'cfunc' built successfully${NC}"

# Step 2: Build ZLang project
echo -e "${GREEN}Step 2: Building ZLang project...${NC}"

# Prepare zlang arguments
ZLANG_ARGS="$SOURCE_FILES -o $OUTPUT_NAME"

# Add link objects
for obj in $LINK_OBJECTS; do
    ZLANG_ARGS="$ZLANG_ARGS -link $obj"
done

# Add keep ll flag if requested
if [ "$KEEP_LL" = true ]; then
    ZLANG_ARGS="$ZLANG_ARGS -keepll"
fi

echo -e "${YELLOW}Running: $ZLANG_PATH $ZLANG_ARGS${NC}"
$ZLANG_PATH $ZLANG_ARGS

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: ZLang compilation failed${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Multi-file C integration project built successfully!${NC}"
echo -e "${GREEN}Executable: $OUTPUT_NAME${NC}"

# Test the executable if it exists
if [ -f "$OUTPUT_NAME" ]; then
    echo -e "${YELLOW}Testing executable...${NC}"
    echo -e "${YELLOW}Output:${NC}"
    ./$OUTPUT_NAME
    echo -e "${GREEN}✓ Executable ran successfully${NC}"
fi

echo -e "${GREEN}Build complete!${NC}"