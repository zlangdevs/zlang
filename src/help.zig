const std = @import("std");

const red = "\x1b[31m";
const green = "\x1b[32m";
const yellow = "\x1b[33m";
const blue = "\x1b[34m";
const magenta = "\x1b[35m";
const cyan = "\x1b[36m";
const white = "\x1b[37m";
const bold = "\x1b[1m";
const reset = "\x1b[0m";

pub fn printLogo() void {
    std.debug.print(
        \\{s}
        \\  ______   __
        \\ |___  /  |  |
        \\    / /   |  |     {s}__ _   _ __    _ __{s}
        \\   / /    |  |    {s}/ _` | | '_ \  /  _` |{s}
        \\  / /__   |  |___{s}| (_| | | | | | | (_| |{s}
        \\ /_____|  |______|{s}\__,_| |_| |_|  \__, |{s}
        \\                                   {s}__/ |{s}
        \\                                  {s}|___/{s}
        \\{s}
        \\
    , .{
        cyan,
        green, cyan,
        green, cyan,
        green, cyan,
        green, cyan,
        green, cyan,
        green, cyan,
        reset,
    });
}

pub fn printHelp() void {
    printLogo();
    std.debug.print(
        \\{s}Usage:{s} zlang {s}<command>{s} {s}[options]{s}
        \\
        \\{s}Commands:{s}
        \\  {s}build{s}       Compile a ZLang program (default)
        \\  {s}wrap{s}        Generate ZLang wrappers for C headers
        \\  {s}help{s}        Show this help message
        \\
    , .{
        bold, reset, green, reset, yellow, reset,
        bold, reset,
        cyan, reset,
        cyan, reset,
        cyan, reset,
    });

    std.debug.print(
        \\{s}Options:{s}
        \\  {s}-o <file>{s}     Specify output file
        \\  {s}-arch <arch>{s}  Specify target architecture (e.g., x86_64)
        \\  {s}-keepll{s}      Keep intermediate LLVM IR files
        \\  {s}-dast{s}        Dump Abstract Syntax Tree
        \\  {s}-verbose{s}     Enable verbose output
        \\  {s}-optimize{s}    Enable optimizations
        \\  {s}-c{s}           Compile to object file only
        \\  {s}-l<lib>{s}      Link with library
        \\  {s}-L<path>{s}     Add library search path
        \\
    , .{
        bold, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
        yellow, reset,
    });

    std.debug.print(
        \\{s}Examples:{s}
        \\  zlang main.zl
        \\  zlang wrap mylib.h -o mylib.zl
        \\  zlang -o myapp main.zl -lSDL2
        \\
        \\{s}For more information, visit:{s} {s}https://github.com/zlang-org/zlang{s}
        \\
    , .{
        bold, reset,
        bold, reset, blue, reset,
    });
}

pub fn printHelpSyntax() void {
    std.debug.print(
        \\{s}ZLang Syntax Guide{s}
        \\
        \\{s}Functions:{s}
        \\  fun name(arg: type) >> return_type {{ ... }}
        \\
        \\{s}Variables:{s}
        \\  i32 x = 10;
        \\  const f32 pi = 3.14;
        \\
        \\{s}Control Flow:{s}
        \\  if x > 0 {{ ... }} else {{ ... }}
        \\  for i32 i = 0; i < 10; i++ {{ ... }}
        \\  while x > 0 {{ ... }}
        \\
        \\{s}Pointers:{s}
        \\  ptr<i32> p = &x;
        \\  p.* = 20;
        \\
        \\{s}Arrays:{s}
        \\  arr<i32, 10> a;
        \\  a[0] = 1;
        \\
    , .{
        bold, reset,
        cyan, reset,
        cyan, reset,
        cyan, reset,
        cyan, reset,
        cyan, reset,
    });
}
