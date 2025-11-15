const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const env = std.process;

    // Helper to get env var or default
    fn getEnvOrDefault(key: []const u8, default: []const u8) []const u8 {
        const maybe = env.getEnvVarOwned(allocator, key) catch null;
        if (maybe) |val| {
            defer allocator.free(val);
            if (val.len > 0) return val;
        }
        return default;
    }

    // Resolve current working directory (absolute)
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    // -------------------------------------------------------------------------
    // Configuration – environment overrides with sensible defaults
    // -------------------------------------------------------------------------
    const separator = getEnvOrDefault("SEPARATOR", "-");
    const noun_folder = getEnvOrDefault("NOUN_FOLDER", try std.fmt.allocPrint(allocator, "{s}/nouns", .{cwd}));
    const adj_folder = getEnvOrDefault("ADJ_FOLDER", try std.fmt.allocPrint(allocator, "{s}/adjectives", .{cwd}));
    defer if (noun_folder != "-") allocator.free(noun_folder);
    defer if (adj_folder != "-") allocator.free(adj_folder);

    // -------------------------------------------------------------------------
    // Resolve files – env var overrides, otherwise pick a random file from the folder.
    // -------------------------------------------------------------------------
    const noun_file = blk: {
        const env_val = env.getEnvVarOwned(allocator, "NOUN_FILE") catch null;
        if (env_val) |val| {
            defer allocator.free(val);
            if (val.len > 0) break :blk val;
        }
        break :blk try pickRandomFile(allocator, noun_folder);
    };
    const adj_file = blk: {
        const env_val = env.getEnvVarOwned(allocator, "ADJ_FILE") catch null;
        if (env_val) |val| {
            defer allocator.free(val);
            if (val.len > 0) break :blk val;
        }
        break :blk try pickRandomFile(allocator, adj_folder);
    };
    defer allocator.free(noun_file);
    defer allocator.free(adj_file);

    // -------------------------------------------------------------------------
    // Determine how many lines to emit (counto)
    // -------------------------------------------------------------------------
    const counto: usize = blk: {
        // 1) env var "counto"
        const env_cnt = env.getEnvVarOwned(allocator, "counto") catch null;
        if (env_cnt) |val| {
            defer allocator.free(val);
            const parsed = std.fmt.parseInt(usize, std.mem.trim(u8, val, &std.ascii.whitespace), 10) catch 0;
            if (parsed != 0) break :blk parsed;
        }
        // 2) try tput lines
        var child = std.ChildProcess.init(&.{ "tput", "lines" }, allocator);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Ignore;
        try child.spawn();
        const stdout = try child.stdout.?.readToEndAlloc(allocator, 1024);
        defer allocator.free(stdout);
        _ = try child.wait();
        const trimmed = std.mem.trim(u8, stdout, &std.ascii.whitespace);
        const parsed = std.fmt.parseInt(usize, trimmed, 10) catch 0;
        if (parsed != 0) break :blk parsed;
        // fallback
        break :blk 24;
    };

    // -------------------------------------------------------------------------
    // Load lines from the selected files
    // -------------------------------------------------------------------------
    const noun_lines = try readNonEmptyLines(allocator, noun_file);
    defer {
        for (noun_lines) |line| allocator.free(line);
        allocator.free(noun_lines);
    }
    const adj_lines = try readNonEmptyLines(allocator, adj_file);
    defer {
        for (adj_lines) |line| allocator.free(line);
        allocator.free(adj_lines);
    }

    // -------------------------------------------------------------------------
    // Main generation loop
    // -------------------------------------------------------------------------
    var rng = std.rand.DefaultPrng.init(@intCast(u64, std.time.milliTimestamp()));
    var countzero: usize = 0;
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    while (countzero < counto) : (countzero += 1) {
        // Random noun, lower‑cased
        const noun_idx = rng.random().intRangeLessThan(usize, noun_lines.len);
        var noun = noun_lines[noun_idx];
        // lower‑case in‑place (need a mutable copy)
        const noun_lc = try std.mem.dupe(allocator, u8, noun);
        defer allocator.free(noun_lc);
        for (noun_lc) |*c| c.* = std.ascii.toLower(c.*);

        // Random adjective, preserve case
        const adj_idx = rng.random().intRangeLessThan(usize, adj_lines.len);
        const adjective = adj_lines[adj_idx];

        // Debug output if DEBUG=true
        if (std.mem.eql(u8, env.getEnvVar("DEBUG") orelse "", "true")) {
            try stderr.print("DEBUG:\n", .{});
            try stderr.print("  adjective : {s}\n", .{adjective});
            try stderr.print("  noun      : {s}\n", .{noun_lc});
            try stderr.print("  ADJ_FILE  : {s}\n", .{adj_file});
            try stderr.print("  ADJ_FOLDER: {s}\n", .{adj_folder});
            try stderr.print("  NOUN_FILE : {s}\n", .{noun_file});
            try stderr.print("  NOUN_FOLDER: {s}\n", .{noun_folder});
            try stderr.print("  {d} > {d}\n", .{ countzero, counto });
        }

        // Emit result
        try stdout.print("{s}{s}{s}\n", .{ adjective, separator, noun_lc });
    }
}

// -----------------------------------------------------------------------------
// Pick a random regular file from a directory (non‑recursive)
// -----------------------------------------------------------------------------
fn pickRandomFile(allocator: std.mem.Allocator, folder: []const u8) ![]const u8 {
    var dir = try std.fs.openDirAbsolute(folder, .{ .iterate = true });
    defer dir.close();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    var it = try dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .File) {
            const full = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ folder, entry.name });
            try files.append(full);
        }
    }

    if (files.items.len == 0) {
        return error.NoFilesInFolder;
    }

    var rng = std.rand.DefaultPrng.init(@intCast(u64, std.time.milliTimestamp()));
    const idx = rng.random().intRangeLessThan(usize, files.items.len);
    const chosen = files.items[idx];
    // Transfer ownership of the chosen string to the caller
    // (the other strings in the list are freed)
    for (files.items) |f| {
        if (f != chosen) allocator.free(f);
    }
    return chosen;
}

// -----------------------------------------------------------------------------
// Read a file and return an array of non‑empty trimmed lines (owned strings)
// -----------------------------------------------------------------------------
fn readNonEmptyLines(allocator: std.mem.Allocator, path: []const u8) ![][]const u8 {
    const content = try std.fs.readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, content, "\n");
    while (it.next()) |raw| {
        const trimmed = std.mem.trim(u8, raw, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;
        const owned = try allocator.dupe(u8, trimmed);
        try lines.append(owned);
    }

    return try lines.toOwnedSlice();
}
