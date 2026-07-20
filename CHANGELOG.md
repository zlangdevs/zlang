# Changelog

## 0.1.2 — 2026-07-20

### Stdlib additions
- `std.bits` — `popcount_u32` / `popcount_u64`, `clz_u32`, `ctz_u32`,
  `bswap_u16` / `bswap_u32` / `bswap_u64`, `rotl_u32` / `rotr_u32`,
  `align_up_u64` / `align_down_u64`, `is_power_of_two_u32` (pure ZLang).
- `std.sort` — insertion sort over `i32` / `u32` / `i64` / `f64` / `u8`
  (`sort_i32`, `sort_u32`, `sort_i64`, `sort_f64`, `sort_u8`,
  `sort_bytes` for raw `qsort`-compatible element).
- `std.stringbuf` — dynamic string builder with `append_str`,
  `append_byte`, `append_i64`, `append_u64`, `append_f64`, `clear`,
  `finalize`, `free_buf`.
- `std.conv` — `itoa_alloc` (i64, returns owned buffer), `utoa_alloc`
  (u64), `str_to_long` (any base), `str_to_ulong`, `str_to_double`
  (f64).

### Stdlib — snake_case migration
All public identifiers in `std.math`, `std.string`, `std.random`,
`std.io`, `std.fs`, `std.time`, `std.assert`, `std.env` are now
snake_case. Notable renames:
- `randFloat` → `rand_float`, `randRange` → `rand_range`,
  `randBool` → `rand_bool`, `XorShift` → `xor_shift` (struct)
- `readLine` → `read_line`, `printInt` → `print_int`,
  `printFloat` → `print_float`
- `readLineFd` → `read_line_fd`, `writeLineFd` → `write_line_fd`,
  `readAll` → `read_all`, `writeString` → `write_string`
- `openFile` → `open_file`, `fileExists` → `file_exists`,
  `deleteFile` → `delete_file`, `moveFile` → `move_file`,
  `makeDir` → `make_dir`, `DirEntry` → `dir_entry`
- `nowMillis` → `now_millis`, `nowNano` → `now_nanos` (real
  `clock_gettime` nanoseconds), `timeDiff` → `time_diff`,
  `Timer` → `timer`, `timerStart` → `timer_start`
- `toRadians` → `to_radians`, `toDegrees` → `to_degrees`,
  `isPowerOfTwo` → `is_power_of_two`, `approxEqual` → `approx_equal`,
  `signNum` → `sign_num`
- `assertEqual` → `assert_equal`, `assertNotEqual` → `assert_not_equal`,
  `assertNull` → `assert_null`, `assertInRange` → `assert_in_range`,
  `testStart` → `test_start`, `testPass` → `test_pass`,
  `testSummary` → `test_summary`
- `setIfMissing` → `set_if_missing`, `getOr` → `get_or`
- `strEq` → `str_eq`, plus `starts_with`, `ends_with`,
  `index_of_char`, `last_index_of_char`, `duplicate`

### Compiler
- TTY-aware ANSI colors in diagnostics (no escapes when stdout is not a tty).
- Diagnostics accept source text directly to avoid re-reading files
  per error.
- `printf` / `vararg` exempt from arity check (no more false
  positives on variadic calls).
- Type checking of binary arithmetic operators (rejects
  arithmetic on string literals at semantic time).
- `var x: arr<T, N>;` / `var x: struct T;` / `var x: ptr<T>;` are
  treated as initialized declarations (storage is valid; the
  user is responsible for fields).
- Local variables start as "uninitialized"; flag a read before
  the first assignment (for primitive types). Assignment to
  identifier marks the variable initialized.
- Did-you-mean suggestions on unknown identifier and unknown
  function, using Levenshtein distance over the known-name set.
- Unused-local-variable warning.
- Arity check on non-variadic function calls.
- New diagnostic field `Diagnostic.related` for notes and
  secondary locations.
- Codegen reports "Not all code paths return a value" instead of
  a misleading "Type mismatch in code generation" when a non-void
  function has a missing return.
- `parseInt` on overflowing or malformed integer/float literals
  now reports a TypeMismatch instead of silently emitting 0.
- `evalConstIntExpr` (parse-time constant evaluation) is guarded
  against i64 overflow and min-int / -1 division.
- Empty CLI argument is rejected instead of panicking.
- UAF in `-l` / `-L` spaced flags (extra_args now retains the
  combined string).
- Fixed `u32 x /= y` and `u32 x >>= n` (were emitting `SDiv` /
  `AShr`).
- Fixed `nowMillis` — was emitting `now() * 1000` (seconds
  resolution). Now uses `clock_gettime(CLOCK_REALTIME)`.
- Fixed `randFloat` — was dividing by `2^31 - 1`, could return
  exactly 1.0. Now divides by `2^31`, returns `[0, 1)`.
- OOM in module use-statement collection is now propagated
  instead of swallowed.
- Duplicate function definitions with identical parameter
  signatures are now reported.
- GitHub link in `zlang help` corrected to
  `https://github.com/zlangdevs/zlang`.

## 0.1.1 — 2026-06-13

- Fix AppImage build script (entrypoint, icon, appimagetool path)
- Refactor LLVM backend: extract codegen pipeline into `backend.zig`, cast/expression logic into separate modules
- Fix opaque pointer deref crash in untyped context
- Fix `int + ptr` arithmetic silently coercing pointer to int
- Replace OOM panics and `catch unreachable` with clean error handling
- AUR packages: [`zlang-git`](https://aur.archlinux.org/packages/zlang-git), [`zlang-bin`](https://aur.archlinux.org/packages/zlang-bin)

## 0.1.0 — 2026-06-11

First release.

- zlang compiler (LLVM backend), `build` / `run` / `wrapgen` commands
- Standard library: io, fs, math, mem, string, path, env, random, thread, time, assert
- zlx plugin SDK (C + Zig headers)
- AppImage packaging for Linux (x86_64, aarch64)

**Platform support:** Linux only.
