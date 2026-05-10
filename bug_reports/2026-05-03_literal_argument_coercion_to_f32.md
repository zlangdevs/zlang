# Integer-to-`f32` Argument Coercion Requires Explicit Cast

## Summary

`zlang` now accepts literal call arguments like `takes_f32(0)`, but still rejects `i32` variables passed to an `f32` parameter without explicit cast.

So the literal-specific part appears fixed; remaining behavior is variable numeric coercion in call-argument context.

## Environment

- Repo: `zlang`
- Date: 2026-05-03
- Command used: `zlang run`

## Minimal Reproduction (Current)

File: `/tmp/opencode/literal_probe.zl`

```zl
use std

fun takes_f32(v: f32) >> void {
    printf("f=%f\n", v);
}

fun main() >> i32 {
    i32 i = 3;
    takes_f32(0);   // now compiles
    takes_f32(i);   // fails without explicit cast
    return 0;
}
```

## Repro Steps

1. Save the snippet above to `/tmp/opencode/literal_probe.zl`
2. Run:

```bash
zlang run "/tmp/opencode/literal_probe.zl"
```

## Actual Result

Compiler error on `takes_f32(i)`:

```text
error: Type mismatch: cannot convert i32 to f32
--> /tmp/opencode/literal_probe.zl:21:5
 21 |     takes_f32(i);
    |     ^^^^^^^^^ Check variable types
```

## Expected Result

Either of these should be clearly defined:

1. **Allow safe implicit numeric conversion** in call arguments (`i32 -> f32`), so `takes_f32(i)` compiles.
2. **Disallow by design**, but emit a specific diagnostic with fix hint (`use i as f32`).

For comparison, assignment context already accepts:

```zl
f32 b = 1;
```

## Status Update (2026-05-03, latest)

- Re-tested via `zlang run /tmp/opencode/literal_probe.zl`.
- `takes_f32(0)` works.
- `takes_f32(i32_var)` works.

So the original issue appears fixed.

## Follow-up Observation During Cast Cleanup

While removing redundant casts in a larger real project (`zltris`), compilation hit:

```text
error: Type mismatch: cannot convert f64 to f32
--> ./bot/arena/arena_tuning.zl:84:9
```

This does not reproduce in small standalone snippets (assignment, define expansion,
and call argument coercion all succeed), which suggests there may still be a
context-dependent edge case in larger module builds, or diagnostic line mapping
may be pointing at a nearby expression.

Recommended next step for compiler debugging: emit more detailed coercion trace
for the failing expression (source + inferred types before/after coercion).
