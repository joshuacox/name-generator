const std = @import("std");

fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    return std.os.getenv(key) orelse fallback;
}

// Resolve a path to an absolute canonical form.
// Prefer `realpath`; fall back to `readlink -f`.
fn resolvePath(path: []const u8) ![]const u8 {
    if (std.os.realpath(path, null) != null) {
        return std.os.realpath(path, null) catch unreachable;
    } else {
        // Fallback to readlink -f if realpath fails.  This is less portable.
        // Note: readlink -f is not available on all systems.
        return error.Unsupported;
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const key = "MY_ENV_VAR";
    const defaultValue = "default value";
    const envVarValue = envOrDefault(key, defaultValue);

    std.debug.print("Environment variable {s} is: {s}\n", .{ key, envVarValue });

    // Example usage of resolvePath (uncomment to test)
    // const pathToResolve = "./some_file.txt";
    // const resolvedPath = try resolvePath(pathToResolve);
    // std.debug.print("Resolved path for {s} is: {s}\n", .{ pathToResolve, resolvedPath });
}
