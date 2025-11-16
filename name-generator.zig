const std = @import("std");

fn envOrDefault(key: []const u8, fallback: []const u8) []const u8 {
    return std.os.getenv(key) orelse fallback;
}

// Resolve a path to an absolute canonical form.
// Prefer `realpath`; fall back to `readlink -f` (not implemented here).
fn resolvePath(path: []const u8) anyerror![]const u8 {
    const result = std.os.realpath(path, null);
    return switch (result) {
        .Ok => |abs_path| abs_path,
        .Err => error.Unsupported,
    };
}

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
