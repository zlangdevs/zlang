#!/bin/bash

TEST_DIR="./examples/tests"
APP="./zig-out/bin/lang"
FAILED=0
PASSED=0

echo "🚀 Building Zlang Compiler..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if ! zig build; then
    echo "❌ Build failed!"
    exit 1
fi

echo "✅ Build successful!"
echo ""
echo "🧪 Running Zlang Tests..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Test 1: AST Generation Tests
echo "📋 Testing AST Generation..."
for test_file in "$TEST_DIR"/*.zl; do
    if [ -f "$test_file" ]; then
        echo -n "  $(basename "$test_file"): "
        if $APP "$test_file" --ast-only >/dev/null 2>&1; then
            echo "✅ AST OK"
            ((PASSED++))
        else
            echo "❌ AST FAILED"
            ((FAILED++))
        fi
    fi
done

echo ""
echo "🔧 Testing LLVM IR Generation..."
for test_file in "$TEST_DIR"/*.zl; do
    if [ -f "$test_file" ]; then
        echo -n "  $(basename "$test_file"): "
        if $APP "$test_file" --ir-only >/dev/null 2>&1; then
            echo "✅ IR OK"
            ((PASSED++))
        else
            echo "❌ IR FAILED"
            ((FAILED++))
        fi
    fi
done

echo ""
echo "🎯 Testing Object File Generation..."
for test_file in "$TEST_DIR"/*.zl; do
    if [ -f "$test_file" ]; then
        base_name=$(basename "$test_file" .zl)
        obj_file="test_${base_name}.o"
        echo -n "  $(basename "$test_file"): "

        if $APP "$test_file" --output "$obj_file" >/dev/null 2>&1; then
            if [ -f "$obj_file" ]; then
                echo "✅ OBJ OK"
                rm -f "$obj_file"  # Clean up
                ((PASSED++))
            else
                echo "❌ OBJ NOT CREATED"
                ((FAILED++))
            fi
        else
            echo "❌ OBJ FAILED"
            ((FAILED++))
        fi
    fi
done

echo ""
echo "🔗 Testing Full Compilation Pipeline..."
test_file="$TEST_DIR/simple.zl"
if [ -f "$test_file" ]; then
    echo -n "  Full pipeline test: "
    obj_file="test_full.o"
    exe_file="test_full"

    if $APP "$test_file" --output "$obj_file" >/dev/null 2>&1; then
        if gcc "$obj_file" -o "$exe_file" 2>/dev/null; then
            echo "✅ FULL PIPELINE OK"
            rm -f "$obj_file" "$exe_file"  # Clean up
            ((PASSED++))
        else
            echo "❌ LINKING FAILED"
            rm -f "$obj_file"
            ((FAILED++))
        fi
    else
        echo "❌ COMPILATION FAILED"
        ((FAILED++))
    fi
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Test Results:"
echo "   ✅ Passed: $PASSED"
echo "   ❌ Failed: $FAILED"
echo "   📈 Total:  $((PASSED + FAILED))"

if [ $FAILED -eq 0 ]; then
    echo "🎉 All tests passed! Zlang compiler is working perfectly!"
    exit 0
else
    echo "💥 Some tests failed. Please check the issues above."
    exit 1
fi
