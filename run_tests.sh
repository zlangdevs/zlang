#!/bin/bash

TEST_DIR="./examples/tests"
APP="./zig-out/bin/zlang"
OUTPUT_BIN="./output"
FAILED_COMPILE=0
FAILED_EXPECTED=0
TOTAL_TESTS=0
PASSED_TESTS=0
COMPILE_ONLY_FILES="void_test.zl,test_simple.zl,complex_test.zl,multi_test.zl"
declare -a FAILED_COMPILE_FILES
declare -a FAILED_EXPECTED_FILES
declare -a PASSED_FILES

find_file_by_pattern() {
    local pattern="$1"
    local found_files=()
    
    for file in "$TEST_DIR"/"$pattern"*.zl; do
        if [ -f "$file" ]; then
            found_files+=("$file")
        fi
    done
    
    if [ ${#found_files[@]} -eq 0 ]; then
        for file in "$TEST_DIR"/*"$pattern"*.zl; do
            if [ -f "$file" ]; then
                found_files+=("$file")
            fi
        done
    fi
    
    if [ ${#found_files[@]} -eq 1 ]; then
        echo "${found_files[0]}"
        return 0
    elif [ ${#found_files[@]} -gt 1 ]; then
        echo "Multiple files found:" >&2
        for file in "${found_files[@]}"; do
            echo "  - $(basename "$file")" >&2
        done
        return 2
    else
        return 1
    fi
}

test_single_file() {
    local test_file="$1"
    local filename=$(basename "$test_file")
    
    echo "Testing $filename..."
    echo "====================================================="
    
    echo "Compiling with $APP:"
    if $APP "$test_file"; then
        if [ -f "a.out" ]; then
            BINARY="a.out"
        elif [ -f "output" ]; then
            BINARY="output"
        else
            echo "❌ $filename - FAILED (Binary not found)"
            return 1
        fi
        
        if [[ $COMPILE_ONLY_FILES == *"$filename"* ]]; then
            echo "✅ $filename - PASSED (compile only)"
            rm -f "$BINARY"
            return 0
        else
            echo ""
            echo "Running ./$BINARY:"
            chmod +x "$BINARY"
            OUTPUT=$("./$BINARY" 2>&1)
            EXIT_CODE=$?
            
            echo "$OUTPUT"
            echo "------------------------------------------------"
            EXPECTED_ERRORS=0
            EXPECTED_FOUND=0
            
            while IFS= read -r line; do
                if [[ "$line" =~ expected\ (.+)\):([-+]?[0-9]*\.?[0-9]+) ]]; then
                    EXPECTED_FOUND=1
                    expected_value=${BASH_REMATCH[1]}
                    actual_value=${BASH_REMATCH[2]}
                    
                    if [ "$expected_value" = "$actual_value" ]; then
                        echo "✅ Expected $expected_value, got $actual_value"
                    else
                        echo "❌ Expected $expected_value, but got $actual_value"
                        EXPECTED_ERRORS=$((EXPECTED_ERRORS + 1))
                    fi
                fi
            done <<< "$OUTPUT"
            
            rm -f "$BINARY"
            
            if [ $EXIT_CODE -eq 0 ]; then
                if [ $EXPECTED_FOUND -eq 1 ]; then
                    if [ $EXPECTED_ERRORS -eq 0 ]; then
                        echo "🎉 All expected values matched!"
                        return 0
                    else
                        echo "❌ Some expected values didn't match!"
                        return 1
                    fi
                else
                    echo "✅ Program executed successfully (no expected values to check)"
                    return 0
                fi
            else
                echo "❌ Program exited with code $EXIT_CODE"
                return 1
            fi
        fi
    else
        echo "❌ $filename - FAILED (Compilation error)"
        return 1
    fi
}

if [ $# -gt 0 ]; then
    if [[ "$1" == *.zl ]]; then
        if [ -f "$1" ]; then
            test_file="$1"
        elif [ -f "$TEST_DIR/$1" ]; then
            test_file="$TEST_DIR/$1"
        else
            echo "Error: File $1 not found"
            exit 1
        fi
    else
        test_file=$(find_file_by_pattern "$1")
        if [ $? -eq 0 ]; then
            echo "Found file: $(basename "$test_file")"
        elif [ $? -eq 2 ]; then
            exit 1
        else
            echo "Error: No files found matching pattern '$1'"
            exit 1
        fi
    fi
    
    test_single_file "$test_file"
    exit $?
fi

echo "Building compiler..."
zig build
if [ $? -ne 0 ]; then
    echo "Error: zig build failed"
    exit 1
fi

echo ""
echo "Running tests"
echo "====================================================="

for test_file in "$TEST_DIR"/*.zl; do
    if [ ! -f "$test_file" ]; then
        continue
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    filename=$(basename "$test_file")
    
    clear
    echo "Testing $filename..."
    echo "====================================================="
    
    if $APP "$test_file" 2>/dev/null; then
        if [ -f "a.out" ]; then
            BINARY="a.out"
        elif [ -f "output" ]; then
            BINARY="output"
        else
            echo "❌ $filename - FAILED (Binary not found)"
            FAILED_COMPILE_FILES+=("$filename")
            FAILED_COMPILE=$((FAILED_COMPILE + 1))
            echo ""
            echo "Press any key to continue to next test..."
            read -n 1 -s
            continue
        fi
        
        if [[ $COMPILE_ONLY_FILES == *"$filename"* ]]; then
            echo "✅ $filename - PASSED (compile only)"
            PASSED_FILES+=("$filename")
            PASSED_TESTS=$((PASSED_TESTS + 1))
            rm -f "$BINARY"
        else
            chmod +x "$BINARY"
            OUTPUT=$("./$BINARY" 2>&1)
            EXIT_CODE=$?
            EXPECTED_ERRORS=0
            
            while IFS= read -r line; do
                if [[ "$line" =~ expected\ (.+)\):([-+]?[0-9]*\.?[0-9]+) ]]; then
                    expected_value=${BASH_REMATCH[1]}
                    actual_value=${BASH_REMATCH[2]}
                    
                    if [ "$expected_value" = "$actual_value" ]; then
                        echo "✓ Expected $expected_value, got $actual_value"
                    else
                        echo "✗ Expected $expected_value, but got $actual_value"
                        EXPECTED_ERRORS=$((EXPECTED_ERRORS + 1))
                    fi
                fi
            done <<< "$OUTPUT"
            
            echo "------------------------------------------------"
            echo "Program output:"
            echo "$OUTPUT"
            echo "------------------------------------------------"
            
            if [ $EXIT_CODE -eq 0 ] && [ $EXPECTED_ERRORS -eq 0 ]; then
                echo "✅ $filename - PASSED"
                PASSED_FILES+=("$filename")
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                echo "❌ $filename - FAILED (Exit code: $EXIT_CODE, Expected errors: $EXPECTED_ERRORS)"
                FAILED_EXPECTED_FILES+=("$filename")
                FAILED_EXPECTED=$((FAILED_EXPECTED + 1))
            fi
            
            rm -f "$BINARY"
        fi
        
    else
        echo "❌ $filename - FAILED (Compilation error)"
        FAILED_COMPILE_FILES+=("$filename")
        FAILED_COMPILE=$((FAILED_COMPILE + 1))
    fi
    
    if [ $? -ne 0 ] || [[ " ${FAILED_COMPILE_FILES[@]} " =~ " ${filename} " ]] || [[ " ${FAILED_EXPECTED_FILES[@]} " =~ " ${filename} " ]]; then
        echo ""
        echo "Press any key to continue to next test..."
        read -n 1 -s
    fi
done

clear
echo "====================================================="
echo "TEST SUMMARY"
echo "====================================================="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed compilation: $FAILED_COMPILE"
echo "Failed expected values: $FAILED_EXPECTED"
echo ""
echo "┌──────────────────────────────┬──────────────┐"
echo "│           FILENAME           │   STATUS     │"
echo "├──────────────────────────────┼──────────────┤"

for file in "${PASSED_FILES[@]}"; do
    printf "│ %-28s │ \033[32mPASSED\033[0m       │\n" "$file"
done

for file in "${FAILED_COMPILE_FILES[@]}"; do
    printf "│ %-28s │ \033[31mCOMPILE\033[0m      │\n" "$file"
done

for file in "${FAILED_EXPECTED_FILES[@]}"; do
    printf "│ %-28s │ \033[31mEXPECTED\033[0m     │\n" "$file"
done

echo "├──────────────────────────────┼──────────────┤"
printf "│ TOTAL: %-21d │ \033[32m%3d\033[0m \033[31m%3d\033[0m      │\n" \
       $TOTAL_TESTS $PASSED_TESTS $((FAILED_COMPILE + FAILED_EXPECTED))
echo "└──────────────────────────────┴──────────────┘"
echo ""

if [ ${#FAILED_COMPILE_FILES[@]} -gt 0 ]; then
    echo "Failed compilation:"
    for file in "${FAILED_COMPILE_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [ ${#FAILED_EXPECTED_FILES[@]} -gt 0 ]; then
    echo "Failed expected values:"
    for file in "${FAILED_EXPECTED_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [ $FAILED_COMPILE -eq 0 ] && [ $FAILED_EXPECTED -eq 0 ]; then
    echo "🎉 All tests passed!"
else
    echo "❌ Some tests failed."
fi