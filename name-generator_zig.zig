const std = @import("std");

// ---------------------------------------------------------------
// Helper: read an environment variable, falling back to a default string.
// This implementation works across Zig versions by checking which
// module provides `getenv`. If neither is available we simply return
// the fallback value.
// ---------------------------------------------------------------
fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    // Zig 0.11+ provides std.process.getenv
    if (@hasDecl(std.process, "getenv")) {
        return std.process.getenv(key) orelse fallback;
    }
    // Older Zig releases expose getenv via std.os
    if (@hasDecl(std.os, "getenv")) {
        return std.os.getenv(key) orelse fallback;
    }
    // As a last resort, just return the fallback.
    return fallback;
}

// ---------------------------------------------------------------
// Resolve a path to an absolute canonical form.
// Prefer `realpath`; if it fails we propagate the error.
// ---------------------------------------------------------------
fn resolvePath(path: []const u8) anyerror![]const u8 {
    // std.os.realpath returns an error‑union: ![]const u8
    // `try` propagates the error to the caller.
    const abs_path = try std.os.realpath(path, null);
    return abs_path;
}

// ---------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------
pub fn main() !void {
    const key = "MY_ENV_VAR";
    const defaultValue = "default value";

    const envVarValue = envOrDefault(key, defaultValue);
    std.debug.print("Environment variable {s} is: {s}\n", .{ key, envVarValue });

    // Example usage of resolvePath (uncomment to test)
    // const pathToResolve = "./some_file.txt";
    // const resolvedPath = try resolvePath(pathToResolve);
    // std.debug.print("Resolved path for {s} is: {s}\n", .{ pathToResolve, resolvedPath });
}
