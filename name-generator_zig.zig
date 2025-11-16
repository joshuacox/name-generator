const std = @import("std");

// Import C's getenv for environment variable access without using std.os or std.process.
pub const c = @cImport({
    @cInclude("stdlib.h");
});

/// Retrieve an environment variable using C's `getenv`.
/// Returns `null` if the variable is not set.
fn cGetenv(key: []const u8) ?[]const u8 {
    // Allocate a null‑terminated C string on the heap.
    const allocator = std.heap.page_allocator;
    const c_key = allocator.alloc(u8, key.len + 1) catch return null;
    // Ensure we free the allocation before returning.
    defer allocator.free(c_key);

    // Copy the Zig string into the buffer and add the terminating NUL.
    std.mem.copyForwards(u8, c_key[0..key.len], key);
    c_key[key.len] = 0;

    // Call C's getenv.
    const c_val = c.getenv(c_key.ptr);
    if (c_val) |ptr| {
        // Determine length of the C string returned by getenv.
        const len = std.mem.len(ptr);
        // Return a slice referencing the C string (no need to copy; it's owned by the environment).
        return ptr[0..len];
    }
    return null;
}

// ---------------------------------------------------------------
// Helper: read an environment variable, falling back to a default.
// ---------------------------------------------------------------
fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    if (cGetenv(key)) |val| {
        // Treat empty strings as unset.
        return if (val.len == 0) fallback else val;
    }
    return fallback;
}

// ---------------------------------------------------------------
// Helper: parse an integer from a string, returning fallback on error.
// ---------------------------------------------------------------
fn parseInt(str: []const u8, fallback: usize) usize {
    const parsed = std.fmt.parseInt(usize, str, 10) catch return fallback;
    return parsed;
}

// ---------------------------------------------------------------
// Helper: pick a random regular file from a directory.
// ---------------------------------------------------------------
fn pickRandomFile(allocator: *std.mem.Allocator, dir_path: []const u8) ![]const u8 {
    var dir = try std.fs.openDirAbsolute(dir_path, .{ .iterate = true });
    defer dir.close();

    const FileList = std.ArrayList([]const u8);
    var files = FileList.init(allocator);
    defer files.deinit();

    var it = try dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .File) {
            const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            try files.append(full_path);
        }
    }

    if (files.items.len == 0) {
        return error.NoFilesInDirectory;
    }

    // Seed the PRNG with the current timestamp.
    const seed = @as(u64, std.time.timestamp());
    const prng = std.rand.DefaultPrng.init(seed);
    const idx = prng.random().intRangeLessThan(usize, files.items.len);
    return files.items[idx];
}

// ---------------------------------------------------------------
// Helper: read non‑empty lines from a file.
// ---------------------------------------------------------------
fn readNonEmptyLines(allocator: *std.mem.Allocator, file_path: []const u8) ![]const []const u8 {
    const file = try std.fs.openFileAbsolute(file_path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(content);

    const LineList = std.ArrayList([]const u8);
    var lines = LineList.init(allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, content, "\n");
    while (it.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (trimmed.len > 0) {
            try lines.append(trimmed);
        }
    }

    return try allocator.dupe([]const u8, lines.items);
}

// ---------------------------------------------------------------
// Helper: lower‑case a UTF‑8 string (ASCII safe for our use case).
// ---------------------------------------------------------------
fn toLower(allocator: *std.mem.Allocator, s: []const u8) ![]const u8 {
    var buf = try allocator.alloc(u8, s.len);
    var i: usize = 0;
    for (s) |ch| {
        buf[i] = std.ascii.toLower(ch);
        i += 1;
    }
    return buf;
}

// ---------------------------------------------------------------
// Debug printer – writes to stderr when DEBUG=true.
// ---------------------------------------------------------------
fn debugPrint(
    allocator: *std.mem.Allocator,
    adjective: []const u8,
    noun: []const u8,
    adj_file: []const u8,
    adj_folder: []const u8,
    noun_file: []const u8,
    noun_folder: []const u8,
    countzero: usize,
    counto: usize,
) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print("DEBUG:\n", .{});
    try stderr.print("  adjective : {s}\n", .{adjective});
    try stderr.print("  noun      : {s}\n", .{noun});
    try stderr.print("  ADJ_FILE  : {s}\n", .{adj_file});
    try stderr.print("  ADJ_FOLDER: {s}\n", .{adj_folder});
    try stderr.print("  NOUN_FILE : {s}\n", .{noun_file});
    try stderr.print("  NOUN_FOLDER: {s}\n", .{noun_folder});
    try stderr.print("  {d} > {d}\n", .{ countzero, counto });
    _ = allocator; // silence unused warning
}

// ---------------------------------------------------------------
// Main entry point – mimics name-generator.sh behaviour.
// ---------------------------------------------------------------
pub fn main() !void {
    // Use a mutable variable for the allocator so we can pass a mutable pointer where required.
    var allocator = std.heap.page_allocator;

    // ---------------------------------
    // Configuration – environment overrides with sensible defaults.
    // ---------------------------------
    const separator = envOrDefault("SEPARATOR", "-");
    const counto_str = envOrDefault("counto", "24");
    const counto = parseInt(counto_str, 24);

    // getCwdAlloc expects the allocator value, not a pointer.
    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);

    // Join paths using the allocator value (not a pointer) as required by the current std.fs.path.join signature.
    const noun_folder = envOrDefault("NOUN_FOLDER", try std.fs.path.join(allocator, &.{ cwd, "nouns" }));
    const adj_folder = envOrDefault("ADJ_FOLDER", try std.fs.path.join(allocator, &.{ cwd, "adjectives" }));
    defer allocator.free(noun_folder);
    defer allocator.free(adj_folder);

    // Resolve files – env var overrides, otherwise pick a random file.
    const noun_file_env = envOrDefault("NOUN_FILE", "");
    const adj_file_env = envOrDefault("ADJ_FILE", "");

    const noun_file = if (noun_file_env.len > 0) noun_file_env else try pickRandomFile(&allocator, noun_folder);
    const adj_file = if (adj_file_env.len > 0) adj_file_env else try pickRandomFile(&allocator, adj_folder);
    defer allocator.free(noun_file);
    defer allocator.free(adj_file);

    // Load lines from the selected files.
    const noun_lines = try readNonEmptyLines(&allocator, noun_file);
    const adj_lines = try readNonEmptyLines(&allocator, adj_file);
    defer {
        for (noun_lines) |l| allocator.free(l);
        allocator.free(noun_lines);
        for (adj_lines) |l| allocator.free(l);
        allocator.free(adj_lines);
    }

    // Prepare PRNG.
    var prng = std.rand.DefaultPrng.init(@as(u64, std.time.timestamp()));
    const rand = &prng.random();

    // Main generation loop.
    var countzero: usize = 0;
    while (countzero < counto) : (countzero += 1) {
        // Pick random noun and adjective.
        const noun_raw = noun_lines[rand.intRangeLessThan(usize, noun_lines.len)];
        const adj_raw = adj_lines[rand.intRangeLessThan(usize, adj_lines.len)];

        // Lower‑case noun.
        const noun_lc = try toLower(&allocator, noun_raw);
        defer allocator.free(noun_lc);

        // Debug output if requested.
        if (std.mem.eql(u8, envOrDefault("DEBUG", ""), "true")) {
            try debugPrint(
                &allocator,
                adj_raw,
                noun_lc,
                adj_file,
                adj_folder,
                noun_file,
                noun_folder,
                countzero,
                counto,
            );
        }

        // Emit result.
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}{s}{s}\n", .{ adj_raw, separator, noun_lc });
    }
}
