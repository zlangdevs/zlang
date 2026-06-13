# Changelog

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
