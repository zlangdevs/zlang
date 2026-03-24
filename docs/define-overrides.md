# Define Overrides (`-D`)

You can override source-level `#define` values from the command line.

## Syntax

- `-DNAME=VALUE`
- `-D NAME=VALUE`

Example:

```bash
zlang main.zl -DLOGMODE=true -o app
```

## Behavior

- `#define` directives in `.zl` files are read during preprocessing.
- If a matching `-DNAME=VALUE` is provided, that value replaces the source `#define NAME ...` value.
- If the same `-DNAME=...` is passed multiple times, the last value wins.
- If `-DNAME=...` is provided but `#define NAME ...` is not found in parsed `.zl` files, compilation continues and a warning is printed.

## Minimal Example

Source:

```zl
#define LOGMODE false

fun main() >> i32 {
    if (LOGMODE) {
        @printf("debug on\n");
    }
    return 0;
}
```

Build with override:

```bash
zlang main.zl -DLOGMODE=true -o app
```
