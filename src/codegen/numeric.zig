const std = @import("std");

pub fn parseNumericLiteral(literal: []const u8) !i64 {
    if (literal.len == 0) return error.InvalidLiteral;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const cleaned = try removeDelimiters(allocator, literal);
    if (std.mem.startsWith(u8, cleaned, "0x") or std.mem.startsWith(u8, cleaned, "0X")) {
        const hex_part = cleaned[2..];
        return std.fmt.parseInt(i64, hex_part, 16);
    } else if (std.mem.startsWith(u8, cleaned, "0b") or std.mem.startsWith(u8, cleaned, "0B")) {
        const bin_part = cleaned[2..];
        return std.fmt.parseInt(i64, bin_part, 2);
    } else if (cleaned.len > 1 and cleaned[0] == '0' and std.ascii.isDigit(cleaned[1])) {
        return std.fmt.parseInt(i64, cleaned, 8);
    } else {
        return std.fmt.parseInt(i64, cleaned, 10);
    }
}

pub fn parseNumericLiteralUnsigned(literal: []const u8) !u64 {
    if (literal.len == 0) return error.InvalidLiteral;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const cleaned = try removeDelimiters(allocator, literal);
    if (std.mem.startsWith(u8, cleaned, "0x") or std.mem.startsWith(u8, cleaned, "0X")) {
        const hex_part = cleaned[2..];
        return std.fmt.parseInt(u64, hex_part, 16);
    } else if (std.mem.startsWith(u8, cleaned, "0b") or std.mem.startsWith(u8, cleaned, "0B")) {
        const bin_part = cleaned[2..];
        return std.fmt.parseInt(u64, bin_part, 2);
    } else if (cleaned.len > 1 and cleaned[0] == '0' and std.ascii.isDigit(cleaned[1])) {
        return std.fmt.parseInt(u64, cleaned, 8);
    } else {
        return std.fmt.parseInt(u64, cleaned, 10);
    }
}

pub fn parseFloatLiteral(literal: []const u8) !f64 {
    if (literal.len == 0) return error.InvalidLiteral;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const cleaned = try removeDelimiters(allocator, literal);
    defer allocator.free(cleaned);
    return std.fmt.parseFloat(f64, cleaned);
}

fn removeDelimiters(allocator: std.mem.Allocator, literal: []const u8) ![]u8 {
    var result = try allocator.alloc(u8, literal.len);
    defer allocator.free(result);
    var i: usize = 0;
    for (literal) |c| {
        if (c != '\'') {
            result[i] = c;
            i += 1;
        }
    }
    return try allocator.dupe(u8, result[0..i]);
}

pub fn isNumericLiteral(literal: []const u8) bool {
    if (literal.len == 0) return false;
    if (std.mem.startsWith(u8, literal, "0x") or std.mem.startsWith(u8, literal, "0X")) {
        return isValidHexLiteral(literal[2..]);
    }
    if (std.mem.startsWith(u8, literal, "0b") or std.mem.startsWith(u8, literal, "0B")) {
        return isValidBinaryLiteral(literal[2..]);
    }
    if (literal.len > 1 and literal[0] == '0' and std.ascii.isDigit(literal[1])) {
        return isValidOctalLiteral(literal);
    }
    return isValidDecimalLiteral(literal);
}

fn isValidHexLiteral(hex_part: []const u8) bool {
    if (hex_part.len == 0) return false;
    for (hex_part) |c| {
        if (c != '\'' and !std.ascii.isHex(c)) return false;
    }
    return true;
}

fn isValidBinaryLiteral(bin_part: []const u8) bool {
    if (bin_part.len == 0) return false;

    for (bin_part) |c| {
        if (c != '\'' and c != '0' and c != '1') return false;
    }
    return true;
}

fn isValidOctalLiteral(literal: []const u8) bool {
    for (literal) |c| {
        if (c != '\'' and (c < '0' or c > '7')) return false;
    }
    return true;
}

fn isValidDecimalLiteral(literal: []const u8) bool {
    for (literal) |c| {
        if (c != '\'' and !std.ascii.isDigit(c) and c != '-' and c != '+') return false;
    }
    return true;
}
