# Struct field pointer codegen crash

## What breaks

Returning a pointer to a user-defined struct field can crash the compiler during LLVM codegen.

Minimal repro:

```zl
struct Foo {
    value i32,
}

struct Wrap {
    foo Foo,
}

Wrap g_wrap = {0};

fun getFoo() >> ptr<Foo> {
    return &g_wrap.foo;
}

fun main() >> i32 {
    ptr<Foo> foo = getFoo();
    return foo[0].value;
}
```

## Actual result

- `zlang tmp_ptr_repro.zl` crashes with a segmentation fault
- stack trace points into `src/codegen/llvm.zig:3981`
- for larger programs this can also surface as the opaque message `Error generating code: Type mismatch in code generation.` with no useful source location

## Expected result

- either compile successfully, or
- emit a proper diagnostic that says pointers to struct fields are not supported yet

## What should be fixed

1. Fix codegen for `&some_struct.field` when the result type is `ptr<StructType>`
2. Make all `TypeMismatch` codegen failures include source location and node/token context
3. Avoid crashing; return a structured diagnostic instead
