const std = @import("std");

fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    const env = std.process.env_get(key) orelse fallback;
    return env;
}

// Resolve a path to an absolute canonical form.
// Prefer `realpath`; fall back to `pickRandomFile`
fn resolveFileOrRandom(
    allocator: *std.mem.Allocator,
    envVar: []const u8,
    folder: []const u8,
) ![]const u8 {
    const env = envOrDefault(envVar, folder);
    return try std.fs.path.absolute(allocator, env);
}

// Pick a random regular file from a directory.
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
    for (content) |c| {
        if (c == '\n') {
            const line = std.mem.trim(u8, content[start..], &std.ascii.whitespace);
            try lines.append(line);
            start = start + 1;
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
    for (s) |c| {
        buf[0] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    return buf;
}

// Try to obtain terminal height via `tput lines`. If that fails, return fallback.
fn getTerminalLines(allocator: *std.mem.Allocator) usize {
    var cmd = std.process.Cmd.init(.{
        .cmd = &.{ "tput", "lines" },
    });

    var output = cmd.run();
    if (output.exit_code != 0) {
        return 24;
    }

    const stdout = stdout;
    const trimmed = std.mem.trim(u8, stdout, &std.ascii.whitespace);
    return std.fmt.parseInt(usize, trimmed, 10) catch 24;
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

    // Resolve files – env var overrides, otherwise pick a random file from the folder.
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
        const noun = try toLower(allocator, raw_noun);
        defer allocator.free(noun);
        const adjective = randomChoice([]const u8, adj_lines.items);

        // Debug output if requested
        if (std.debug.debug) {
            debugPrint(allocator, adjective, noun, adj_file, adj_folder, noun_file, noun_folder, countzero, counto);
        }

        // Print result
        const stdout = std.io.getStdOut().writer();
        _ = stdout.print("{s}{s}{s}\n", .{ adjective, separator, noun }) catch {};
    }
}
