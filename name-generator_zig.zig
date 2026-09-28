//! Zig implementation of name-generator.
//!
//! Behaves like `name-generator.sh`:
//!   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
//!     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
//!   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
//!     from the respective folder.
//!   - Emits `counto` lines (default: terminal height via `tput lines`,
//!     fallback 24).
//!   - Noun is lower-cased, adjective keeps original case.
//!   - Optional debug output when DEBUG=true.

const std = @import("std");

fn openFile(path: []const u8) !std.fs.File {
    if (std.fs.path.isAbsolute(path)) {
        return std.fs.openFileAbsolute(path, .{});
    } else {
        return std.fs.cwd().openFile(path, .{});
    }
}

fn openDir(path: []const u8) !std.fs.Dir {
    if (std.fs.path.isAbsolute(path)) {
        return std.fs.openDirAbsolute(path, .{ .iterate = true });
    } else {
        return std.fs.cwd().openDir(path, .{ .iterate = true });
    }
}

fn pickRandomFile(allocator: std.mem.Allocator, folder: []const u8, rand: std.rand.Random) ![]const u8 {
    var dir = try openDir(folder);
    defer dir.close();

    var files = std.ArrayList([]const u8).init(allocator);
    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .file) {
            const full_path = try std.fs.path.join(allocator, &[_][]const u8{ folder, entry.name });
            try files.append(full_path);
        }
    }

    if (files.items.len == 0) {
        return error.NoRegularFilesFound;
    }

    const idx = rand.uintLessThan(usize, files.items.len);
    return files.items[idx];
}

fn readLines(allocator: std.mem.Allocator, path: []const u8, lowercase: bool) ![][]const u8 {
    const file = try openFile(path);
    defer file.close();

    const max_size = 50 * 1024 * 1024; // 50MB
    const content = try file.readToEndAlloc(allocator, max_size);

    var lines = std.ArrayList([]const u8).init(allocator);
    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r\n");
        if (line.len > 0) {
            if (lowercase) {
                const lower = try std.ascii.allocLowerString(allocator, line);
                try lines.append(lower);
            } else {
                try lines.append(line);
            }
        }
    }

    if (lines.items.len == 0) return error.EmptyWordlist;
    return lines.toOwnedSlice();
}

fn getCountO(allocator: std.mem.Allocator) usize {
    if (std.posix.getenv("counto")) |env_val| {
        if (env_val.len > 0) {
            const trimmed = std.mem.trim(u8, env_val, " \t\r\n");
            if (std.fmt.parseInt(usize, trimmed, 10)) |val| {
                return val;
            } else |_| {}
        }
    }

    // Try `tput lines`
    const res = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "tput", "lines" },
    }) catch null;
    if (res) |r| {
        defer allocator.free(r.stdout);
        defer allocator.free(r.stderr);
        const trimmed = std.mem.trim(u8, r.stdout, " \t\r\n");
        if (std.fmt.parseInt(usize, trimmed, 10)) |val| {
            return val;
        } else |_| {}
    }

    return 24;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var seed: u64 = undefined;
    std.posix.getrandom(std.mem.asBytes(&seed)) catch {
        seed = @intCast(std.time.milliTimestamp());
    };
    var prng = std.rand.DefaultPrng.init(seed);
    const rand = prng.random();

    const separator = if (std.posix.getenv("SEPARATOR")) |s| s else "-";
    const noun_folder = if (std.posix.getenv("NOUN_FOLDER")) |nf| nf else "nouns";
    const adj_folder = if (std.posix.getenv("ADJ_FOLDER")) |af| af else "adjectives";

    const noun_file = if (std.posix.getenv("NOUN_FILE")) |f| (if (f.len > 0) f else try pickRandomFile(allocator, noun_folder, rand)) else try pickRandomFile(allocator, noun_folder, rand);
    const adj_file = if (std.posix.getenv("ADJ_FILE")) |f| (if (f.len > 0) f else try pickRandomFile(allocator, adj_folder, rand)) else try pickRandomFile(allocator, adj_folder, rand);

    const nouns = try readLines(allocator, noun_file, true);
    const adjectives = try readLines(allocator, adj_file, false);
    const counto = getCountO(allocator);

    const is_debug = if (std.posix.getenv("DEBUG")) |d| std.mem.eql(u8, d, "true") else false;

    var stdout_buf = std.io.bufferedWriter(std.io.getStdOut().writer());
    const stdout = stdout_buf.writer();

    for (0..counto) |i| {
        const noun = nouns[rand.uintLessThan(usize, nouns.len)];
        const adj = adjectives[rand.uintLessThan(usize, adjectives.len)];

        if (is_debug) {
            std.debug.print(
                "{s}\n{s}\n{s}\n{s}\n{s}\n{s}\n{d} > {d}\n",
                .{ adj, noun, adj_file, adj_folder, noun_file, noun_folder, i, counto },
            );
        }

        try stdout.print("{s}{s}{s}\n", .{ adj, separator, noun });
    }

    try stdout_buf.flush();
}
