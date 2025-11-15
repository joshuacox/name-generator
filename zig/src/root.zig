const std = @import("std");

pub const error = error{
    MissingNounFile,
    MissingAdjFile,
};

/// Read a file, split it into non‑empty trimmed lines and return an owned slice
/// of those lines.  Each line is duplicated so the caller can free the memory
/// independently.
fn readNonEmptyLines(allocator: std.mem.Allocator, path: []const u8) ![][]const u8 {
    const cwd = try std.fs.cwd();
    const file = try cwd.openFile(path, .{});
    defer file.close();

    // Read the whole file into memory (reasonable for the expected small word lists)
    const content = try file.readToEndAlloc(allocator, 10_000_000);
    defer allocator.free(content);

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, content, "\n");
    while (it.next()) |raw_line| {
        const trimmed = std.mem.trim(u8, raw_line, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;
        const dup = try allocator.dupe(u8, trimmed);
        try lines.append(dup);
    }
    return try lines.toOwnedSlice();
}

/// Generate names according to the behaviour of `name-generator.sh`.
/// It reads the environment variables `SEPARATOR`, `NOUN_FILE`, `ADJ_FILE` and
/// optionally `counto`.  It prints `<adjective><separator><noun>` lines to stdout.
pub fn generateNames() !void {
    const env = std.os.getenv;
    const allocator = std.heap.page_allocator;

    // ----------------------------------------------------------------------
    // Resolve configuration from the environment (with the same defaults as the
    // shell script).
    // ----------------------------------------------------------------------
    const separator = env("SEPARATOR") orelse "-";
    const noun_path = env("NOUN_FILE") orelse return error.MissingNounFile;
    const adj_path = env("ADJ_FILE") orelse return error.MissingAdjFile;

    const count_str = env("counto") orelse "24";
    const count = std.fmt.parseInt(usize, count_str, 10) catch 24;

    // ----------------------------------------------------------------------
    // Load the word lists.
    // ----------------------------------------------------------------------
    const noun_lines = try readNonEmptyLines(allocator, noun_path);
    defer {
        for (noun_lines) |line| allocator.free(line);
        allocator.free(noun_lines);
    }

    const adj_lines = try readNonEmptyLines(allocator, adj_path);
    defer {
        for (adj_lines) |line| allocator.free(line);
        allocator.free(adj_lines);
    }

    // ----------------------------------------------------------------------
    // Random generator.
    // ----------------------------------------------------------------------
    var rng = std.rand.DefaultRandom.init(std.time.milliTimestamp());
    const rand = &rng.random;

    var stdout = std.io.getStdOut().writer();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        // Pick random entries.
        const noun_raw = noun_lines[rand.uintLessThan(usize, noun_lines.len)];
        const adj_raw = adj_lines[rand.uintLessThan(usize, adj_lines.len)];

        // Lower‑case the noun (preserving the original slice for the adjective).
        var lower_buf = try allocator.alloc(u8, noun_raw.len);
        defer allocator.free(lower_buf);
        for (noun_raw) |c, idx| {
            lower_buf[idx] = std.ascii.toLower(c);
        }

        // Print the generated name.
        try stdout.print("{s}{s}{s}\n", .{ adj_raw, separator, lower_buf });
    }
}
