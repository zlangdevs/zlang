#!/bin/bash

TEST_DIR="./examples/tests"
COMPILE_FAIL_DIR="./examples/tests/compile_fail"
WARNING_DIR="./examples/tests/warning"
APP="./zig-out/bin/zlang"
OUTPUT_BIN="./output"
FAILED_COMPILE=0
FAILED_EXPECTED=0
FAILED_COMPILE_FAIL=0
FAILED_WARNING=0
TOTAL_TESTS=0
PASSED_TESTS=0
COMPILE_ONLY_FILES="void_test.zl,test_simple.zl,complex_test.zl,multi_test.zl"
declare -a FAILED_COMPILE_FILES
declare -a FAILED_EXPECTED_FILES
declare -a FAILED_COMPILE_FAIL_FILES
declare -a FAILED_WARNING_FILES
declare -a PASSED_FILES
TABLE_NAME_WIDTH=28
CLEAR_EACH_TEST="${CLEAR_EACH_TEST:-1}"

# Set ZSTDPATH to the stdlib directory relative to the project root
export ZSTDPATH="$(pwd)/stdlib"

maybe_clear_screen() {
    if [ "$CLEAR_EACH_TEST" = "1" ] && [ -t 1 ]; then
        clear
    fi
}

format_table_name() {
    local name="$1"
    if [ ${#name} -le $TABLE_NAME_WIDTH ]; then
        printf "%s" "$name"
    else
        printf "%s..." "${name:0:$((TABLE_NAME_WIDTH - 3))}"
    fi
}

find_file_by_pattern() {
    local pattern="$1"
    local found_files=()
    local search_dirs=("$TEST_DIR" "$COMPILE_FAIL_DIR" "$WARNING_DIR")
    
    for dir in "${search_dirs[@]}"; do
        for file in "$dir"/"$pattern"*.zl; do
            if [ -f "$file" ]; then
                found_files+=("$file")
            fi
        done
    done
    
    if [ ${#found_files[@]} -eq 0 ]; then
        for dir in "${search_dirs[@]}"; do
            for file in "$dir"/*"$pattern"*.zl; do
                if [ -f "$file" ]; then
                    found_files+=("$file")
                fi
            done
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
    
    # Check if this test needs math library linking
    local math_link=""
    if [[ "$filename" == *"math"* ]] || grep -q "use std\.math" "$test_file" 2>/dev/null; then
        math_link="-lm"
    fi
    
    echo "Compiling with $APP:"
    if $APP "$test_file" $math_link; then
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

test_compile_fail_file() {
    local test_file="$1"
    local filename=$(basename "$test_file")
    local expected_file="${test_file%.zl}.expected"
    local strict_compile_fail="${STRICT_COMPILE_FAIL:-0}"

    echo "Testing compile-fail $filename..."
    echo "====================================================="

    local output
    output=$($APP "$test_file" 2>&1)
    local compile_exit=$?

    rm -f "output" "a.out"

    if [ $compile_exit -eq 0 ]; then
        echo "❌ $filename - FAILED (Compilation unexpectedly succeeded)"
        return 1
    fi

    if [ ! -f "$expected_file" ]; then
        echo "✅ $filename - PASSED (compile failed as expected, no expectation file)"
        return 0
    fi

    local missing=0
    local matched=0
    local expected_count=0
    while IFS= read -r expected_line; do
        if [ -z "$expected_line" ] || [[ "$expected_line" =~ ^[[:space:]]*# ]]; then
            continue
        fi
        expected_count=$((expected_count + 1))
        if ! printf "%s" "$output" | grep -Fq "$expected_line"; then
            missing=$((missing + 1))
        else
            matched=$((matched + 1))
        fi
    done < "$expected_file"

    if [ $missing -eq 0 ]; then
        echo "✅ $filename - PASSED"
        return 0
    fi

    if [ "$strict_compile_fail" = "0" ] && [ $matched -gt 0 ]; then
        echo "✅ $filename - PASSED (matched $matched/$expected_count expected fragments)"
        return 0
    fi

    while IFS= read -r expected_line; do
        if [ -z "$expected_line" ] || [[ "$expected_line" =~ ^[[:space:]]*# ]]; then
            continue
        fi
        if ! printf "%s" "$output" | grep -Fq "$expected_line"; then
            echo "❌ Missing expected diagnostic text: $expected_line"
        fi
    done < "$expected_file"

    echo "---- compiler output ----"
    echo "$output"
    echo "-------------------------"
    return 1
}

test_warning_file() {
    local test_file="$1"
    local filename=$(basename "$test_file")
    local expected_file="${test_file%.zl}.expected"
    local strict_warning="${STRICT_WARNING:-0}"

    echo "Testing warning $filename..."
    echo "====================================================="

    local math_link=""
    if [[ "$filename" == *"math"* ]] || grep -q "use std\.math" "$test_file" 2>/dev/null; then
        math_link="-lm"
    fi

    local output
    output=$($APP "$test_file" $math_link 2>&1)
    local compile_exit=$?

    rm -f "output" "a.out"

    if [ $compile_exit -ne 0 ]; then
        echo "❌ $filename - FAILED (Compilation failed, warning test expects success)"
        echo "---- compiler output ----"
        echo "$output"
        echo "-------------------------"
        return 1
    fi

    if [ ! -f "$expected_file" ]; then
        echo "✅ $filename - PASSED (compiled, no expectation file)"
        return 0
    fi

    local missing=0
    local matched=0
    local expected_count=0
    while IFS= read -r expected_line; do
        if [ -z "$expected_line" ] || [[ "$expected_line" =~ ^[[:space:]]*# ]]; then
            continue
        fi
        expected_count=$((expected_count + 1))
        if ! printf "%s" "$output" | grep -Fq "$expected_line"; then
            missing=$((missing + 1))
        else
            matched=$((matched + 1))
        fi
    done < "$expected_file"

    if [ $missing -eq 0 ]; then
        echo "✅ $filename - PASSED"
        return 0
    fi

    if [ "$strict_warning" = "0" ] && [ $matched -gt 0 ]; then
        echo "✅ $filename - PASSED (matched $matched/$expected_count expected fragments)"
        return 0
    fi

    while IFS= read -r expected_line; do
        if [ -z "$expected_line" ] || [[ "$expected_line" =~ ^[[:space:]]*# ]]; then
            continue
        fi
        if ! printf "%s" "$output" | grep -Fq "$expected_line"; then
            echo "❌ Missing expected warning text: $expected_line"
        fi
    done < "$expected_file"

    echo "---- compiler output ----"
    echo "$output"
    echo "-------------------------"
    return 1
}

if [ $# -gt 0 ]; then
    maybe_clear_screen
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
    
    case "$test_file" in
        "$COMPILE_FAIL_DIR"/*)
            test_compile_fail_file "$test_file"
            exit $?
            ;;
        "$WARNING_DIR"/*)
            test_warning_file "$test_file"
            exit $?
            ;;
        *)
            test_single_file "$test_file"
            exit $?
            ;;
    esac
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
    
    # Check if this test needs math library linking
    math_link=""
    if [[ "$filename" == *"math"* ]] || grep -q "use std\.math" "$test_file" 2>/dev/null; then
        math_link="-lm"
    fi
    
    maybe_clear_screen
    echo "Testing $filename..."
    echo "====================================================="
    
    if $APP "$test_file" $math_link 2>/dev/null; then
        if [ -f "a.out" ]; then
            BINARY="a.out"
        elif [ -f "output" ]; then
            BINARY="output"
        else
            echo "❌ $filename - FAILED (Binary not found)"
            FAILED_COMPILE_FILES+=("$filename")
            FAILED_COMPILE=$((FAILED_COMPILE + 1))
            if [ "${INTERACTIVE:-0}" = "1" ]; then
                echo ""
                echo "Press any key to continue to next test..."
                read -n 1 -s
            fi
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
    
    if [ "${INTERACTIVE:-0}" = "1" ] && ([ $? -ne 0 ] || [[ " ${FAILED_COMPILE_FILES[@]} " =~ " ${filename} " ]] || [[ " ${FAILED_EXPECTED_FILES[@]} " =~ " ${filename} " ]]); then
        echo ""
        echo "Press any key to continue to next test..."
        read -n 1 -s
    fi
done

if [ -d "$COMPILE_FAIL_DIR" ]; then
    for test_file in "$COMPILE_FAIL_DIR"/*.zl; do
        if [ ! -f "$test_file" ]; then
            continue
        fi

        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        filename=$(basename "$test_file")

        maybe_clear_screen

        if test_compile_fail_file "$test_file"; then
            PASSED_FILES+=("$filename")
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_COMPILE_FAIL_FILES+=("$filename")
            FAILED_COMPILE_FAIL=$((FAILED_COMPILE_FAIL + 1))
        fi
    done
fi

if [ -d "$WARNING_DIR" ]; then
    for test_file in "$WARNING_DIR"/*.zl; do
        if [ ! -f "$test_file" ]; then
            continue
        fi

        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        filename=$(basename "$test_file")

        maybe_clear_screen

        if test_warning_file "$test_file"; then
            PASSED_FILES+=("$filename")
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_WARNING_FILES+=("$filename")
            FAILED_WARNING=$((FAILED_WARNING + 1))
        fi
    done
fi

maybe_clear_screen
echo "====================================================="
echo "TEST SUMMARY"
echo "====================================================="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed compilation: $FAILED_COMPILE"
echo "Failed expected values: $FAILED_EXPECTED"
echo "Failed compile-fail checks: $FAILED_COMPILE_FAIL"
echo "Failed warning checks: $FAILED_WARNING"
echo ""
echo "┌──────────────────────────────┬──────────────┐"
echo "│           FILENAME           │   STATUS     │"
echo "├──────────────────────────────┼──────────────┤"

for file in "${PASSED_FILES[@]}"; do
    printf "│ %-28s │ \033[32mPASSED\033[0m       │\n" "$(format_table_name "$file")"
done

for file in "${FAILED_COMPILE_FILES[@]}"; do
    printf "│ %-28s │ \033[31mCOMPILE\033[0m      │\n" "$(format_table_name "$file")"
done

for file in "${FAILED_EXPECTED_FILES[@]}"; do
    printf "│ %-28s │ \033[31mEXPECTED\033[0m     │\n" "$(format_table_name "$file")"
done

for file in "${FAILED_COMPILE_FAIL_FILES[@]}"; do
    printf "│ %-28s │ \033[31mCFAIL\033[0m        │\n" "$(format_table_name "$file")"
done

for file in "${FAILED_WARNING_FILES[@]}"; do
    printf "│ %-28s │ \033[31mWARNING\033[0m      │\n" "$(format_table_name "$file")"
done

echo "├──────────────────────────────┼──────────────┤"
printf "│ TOTAL: %-21d │ \033[32m%3d\033[0m \033[31m%3d\033[0m      │\n" \
       $TOTAL_TESTS $PASSED_TESTS $((FAILED_COMPILE + FAILED_EXPECTED + FAILED_COMPILE_FAIL + FAILED_WARNING))
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

if [ ${#FAILED_COMPILE_FAIL_FILES[@]} -gt 0 ]; then
    echo "Failed compile-fail checks:"
    for file in "${FAILED_COMPILE_FAIL_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [ ${#FAILED_WARNING_FILES[@]} -gt 0 ]; then
    echo "Failed warning checks:"
    for file in "${FAILED_WARNING_FILES[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [ $FAILED_COMPILE -eq 0 ] && [ $FAILED_EXPECTED -eq 0 ] && [ $FAILED_COMPILE_FAIL -eq 0 ] && [ $FAILED_WARNING -eq 0 ]; then
    echo "🎉 All tests passed!"
else
    echo "❌ Some tests failed."
fi
