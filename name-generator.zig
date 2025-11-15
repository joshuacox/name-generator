const std = @import("std");

// --------------------------------
// Helper to get an environment variable or a default value.
// Returns a slice; no allocation is performed.
// ----------------------------------------------
fn getEnvOrDefault(key: []const u8, default: []const u8) []const u8 {
    if (std.process.getEnvVar(key)) |val| {
        if (val.len > 0) return val;
    }
    return default;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Resolve current working directory (absolute)
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    // ---------- Configuration – environment overrides with sensible defaults ----------
    const separator = getEnvOrDefault("SEPARATOR", "-");

    // For folder paths we allocate the default strings (they will be freed later)
    const noun_folder_default = try std.fmt.allocPrint(allocator, "{s}/nouns", .{cwd});
    const adj_folder_default = try std.fmt.allocPrint(allocator, "{s}/adjectives", .{cwd});
    const noun_folder = getEnvOrDefault("NOUN_FOLDER", noun_folder_default);
    const adj_folder = getEnvOrDefault("ADJ_FOLDER", adj_folder_default);
    // Free the allocated defaults (if they were used). If an env var was used, freeing the
    // literal default is safe because it was never allocated.
    defer {
        if (noun_folder != noun_folder_default) {
            // The default was not used; nothing to free.
        } else {
            allocator.free(noun_folder_default);
        }
    }
    defer {
        if (adj_folder != adj_folder_default) {
            // The default was not used; nothing to free.
        } else {
            allocator.free(adj_folder_default);
        }
    }

    // ---------- Resolve files – env var overrides, otherwise pick a random file ----------
    const noun_file = blk: {
        if (std.process.getEnvVar("NOUN_FILE")) |val| {
            if (val.len > 0) break :blk val;
        }
        break :blk try pickRandomFile(allocator, noun_folder);
    };
    const adj_file = blk: {
        if (std.process.getEnvVar("ADJ_FILE")) |val| {
            if (val.len > 0) break :blk val;
        }
        break :blk try pickRandomFile(allocator, adj_folder);
    };
    // NOTE: We deliberately do NOT free noun_file or adj_file here because they may
    // point to environment strings (which we do not own). The strings returned by
    // pickRandomFile are owned, but freeing them would require tracking ownership.
    // The small amount of leaked memory is acceptable for this short‑lived program.

    // ---------- Determine how many lines to emit (counto) ----------
    const counto: usize = blk: {
        // 1) env var "counto"
        if (std.process.getEnvVar("counto")) |val| {
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

    // ---------- Load lines from the selected files ----------
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

    // ---------- Main generation loop ----------
    // Use a type alias for the PRNG to avoid the init‑argument confusion.
    const Prng = std.rand.DefaultPrng;
    // Create a seed once and reuse it for each PRNG instance.
    const seed: u64 = @intCast(std.time.milliTimestamp());
    var rng = Prng.init(seed);
    var countzero: usize = 0;
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    while (countzero < counto) : (countzero += 1) {
        // Random noun, lower‑cased
        const noun_idx = rng.random().intRangeLessThan(usize, noun_lines.len);
        const noun = noun_lines[noun_idx];
        const noun_lc = try std.mem.dupe(allocator, u8, noun);
        for (noun_lc) |*c| c.* = std.ascii.toLower(c.*);

        // Random adjective, preserve case
        const adj_idx = rng.random().intRangeLessThan(usize, adj_lines.len);
        const adjective = adj_lines[adj_idx];

        // Debug output if DEBUG=true
        if (std.mem.eql(u8, (std.process.getEnvVar("DEBUG") orelse ""), "true")) {
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

        // Free the temporary lower‑cased noun string for this iteration
        allocator.free(noun_lc);
    }
}

// --------------------------------
// Pick a random regular file from a directory (non‑recursive)
// ----------------------------------------------
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

    // Use the same PRNG type alias for consistency.
    const Prng = std.rand.DefaultPrng;
    const seed: u64 = @intCast(std.time.milliTimestamp());
    var rng = Prng.init(seed);
    const idx = rng.random().intRangeLessThan(usize, files.items.len);
    const chosen = files.items[idx];
    // Transfer ownership of the chosen string to the caller (free the others)
    for (files.items) |f| {
        if (f != chosen) allocator.free(f);
    }
    return chosen;
}

// --------------------------------
// Read a file and return an array of non‑empty trimmed lines (owned strings)
// ----------------------------------------------
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
