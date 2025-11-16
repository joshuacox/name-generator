const std = @import("std");

fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    const env = std.os.getenv(key);
    return if (env) |val| if (val.len > 0) val else fallback else fallback;
}

// Resolve a path: if the environment variable is set and points to a regular file, use it.
// Otherwise pick a random regular file from the given folder.
fn resolveFileOrRandom(
    allocator: *std.mem.Allocator,
    envVar: []const u8,
    folder: []const u8,
) ![]const u8 {
    const env = std.os.getenv(envVar);
    if (env) |val| {
        if (val.len > 0) {
            const path = try std.fs.realpathAlloc(allocator, val);
            defer allocator.free(path);
            const file = std.fs.openFileAbsolute(path, .{}) catch null;
            if (file) |f| {
                f.close();
                return path;
            } else {
                return error.InvalidEnvPath;
            }
        }
    }
    // fallback: pick random regular file from folder
    return try pickRandomFile(allocator, folder);
}

// Pick a random regular file from a directory (non‑recursive).
fn pickRandomFile(allocator: *std.mem.Allocator, dir_path: []const u8) ![]const u8 {
    var dir = try std.fs.openDirAbsolute(dir_path, .{});
    defer dir.close();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    var it = try dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .File) {
            const full = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            try files.append(full);
        }
    }

    if (files.items.len == 0) return error.EmptyDirectory;

    const idx = std.rand.DefaultPrng.init(std.time.milliTimestamp()).random().intRangeLessThan(usize, files.items.len);
    return files.items[idx];
}

// Read all lines from a file, returning a list of owned strings.
fn readAllLines(allocator: *std.mem.Allocator, path: []const u8) !std.ArrayList([]const u8) {
    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(content);

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var start: usize = 0;
    for (content) |c, i| {
        if (c == '\n') {
            const line = std.mem.trim(u8, content[start..i], &std.ascii.whitespace);
            try lines.append(line);
            start = i + 1;
        }
    }
    // last line (if file doesn't end with newline)
    if (start < content.len) {
        const line = std.mem.trim(u8, content[start..], &std.ascii.whitespace);
        try lines.append(line);
    }
    return lines;
}

// Pick a random element from an ArrayList.
fn randomChoice(comptime T: type, list: []const T) T {
    const idx = std.rand.DefaultPrng.init(std.time.milliTimestamp()).random().intRangeLessThan(usize, list.len);
    return list[idx];
}

// Convert a string to lower case (ASCII only, matching the shell script behaviour).
fn toLower(allocator: *std.mem.Allocator, s: []const u8) ![]const u8 {
    var buf = try allocator.alloc(u8, s.len);
    for (s) |c, i| {
        buf[i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    return buf;
}

// Try to obtain terminal height via `tput lines`. If that fails, return fallback.
fn getTerminalLines(allocator: *std.mem.Allocator) usize {
    var child = std.ChildProcess.init(&.{ "tput", "lines" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Ignore;
    const result = child.spawnAndWait() catch return 24;
    if (result.term != .Exited or result.code != 0) return 24;

    const out = child.stdout.?;
    const output = out.readToEndAlloc(allocator, 64) catch return 24;
    defer allocator.free(output);

    const trimmed = std.mem.trim(u8, output, &std.ascii.whitespace);
    const parsed = std.fmt.parseInt(usize, trimmed, 10) catch 24;
    return parsed;
}

// Debug printer, mirrors the shell script's `debugger` function.
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
) void {
    const stderr = std.io.getStdErr().writer();
    _ = stderr.print("{s}\n", .{adjective}) catch {};
    _ = stderr.print("{s}\n", .{noun}) catch {};
    _ = stderr.print("{s}\n", .{adj_file}) catch {};
    _ = stderr.print("{s}\n", .{adj_folder}) catch {};
    _ = stderr.print("{s}\n", .{noun_file}) catch {};
    _ = stderr.print("{s}\n", .{noun_folder}) catch {};
    _ = stderr.print("{d} > {d}\n", .{ countzero, counto }) catch {};
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Determine current working directory (HERE)
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    // Configuration from environment (with defaults)
    const separator = envOrDefault("SEPARATOR", "-");
    const counto = getTerminalLines(allocator);
    const noun_folder = envOrDefault("NOUN_FOLDER", try std.fs.path.join(allocator, &.{ cwd, "nouns" }));
    const adj_folder = envOrDefault("ADJ_FOLDER", try std.fs.path.join(allocator, &.{ cwd, "adjectives" }));

    // Resolve files (env overrides or random pick)
    const noun_file = try resolveFileOrRandom(allocator, "NOUN_FILE", noun_folder);
    defer allocator.free(noun_file);
    const adj_file = try resolveFileOrRandom(allocator, "ADJ_FILE", adj_folder);
    defer allocator.free(adj_file);

    // Load lines from the selected files
    const noun_lines = try readAllLines(allocator, noun_file);
    defer noun_lines.deinit();
    const adj_lines = try readAllLines(allocator, adj_file);
    defer adj_lines.deinit();

    // Main loop
    var countzero: usize = 0;
    while (countzero < counto) : (countzero += 1) {
        const raw_noun = randomChoice([]const u8, noun_lines.items);
        const noun_lower = try toLower(allocator, raw_noun);
        defer allocator.free(noun_lower);

        const adjective = randomChoice([]const u8, adj_lines.items);

        // Debug output if requested
        if (std.os.getenv("DEBUG")) |dbg| {
            if (std.mem.eql(u8, dbg, "true")) {
                debugPrint(
                    allocator,
                    adjective,
                    noun_lower,
                    adj_file,
                    adj_folder,
                    noun_file,
                    noun_folder,
                    countzero,
                    counto,
                );
            }
        }

        // Print result
        const stdout = std.io.getStdOut().writer();
        _ = stdout.print("{s}{s}{s}\n", .{ adjective, separator, noun_lower }) catch {};
    }
}
