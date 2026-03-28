Title: Codegen failure on runtime struct zero assignment (`s = {0};`)

Date: 2026-03-28
Project: `zltris`
Component: zlang compiler/codegen
Severity: high (build/runtime blocker)

Summary
- Assigning a whole struct at runtime using zero-literal form can fail code generation.
- Pattern that triggers issue: `some_struct_var = {0};`

Observed in project
- During VS BOT work in `zltris`, resetting bot runtime with whole-struct assignment caused compiler/codegen failure.
- Workaround was required: explicit per-field reset instead of `= {0};`.

Minimal reproduction
```zl
module repro;

struct S {
    a i32,
    b bool,
}

S g_s = {0};

fun main() >> i32 {
    g_s = {0}; // runtime assignment form triggers codegen issue
    return 0;
}
```

Expected
- Compiler accepts runtime struct assignment with `{0}` and generates valid code.

Actual
- Code generation fails (build blocked) when runtime assignment `g_s = {0};` is present.

Workaround
- Replace whole-struct runtime assignment with explicit field writes:
```zl
g_s.a = 0;
g_s.b = false;
```

Notes
- Global/static initialization with `{0}` is fine; the issue is in runtime assignment path.
- If needed, I can provide the exact failing compiler output from a dedicated isolated repro run.
