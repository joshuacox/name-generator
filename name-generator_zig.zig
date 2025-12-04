const std = @import("std");

pub fn main() void {
    const allocator = std.heap.page_allocator;
    const dir = try std.fs.cwd().openDir(".", .{});
    defer dir.close();

    // Build a temporary list of file paths
    var files: std.ArrayList([]const u8) = std.ArrayList([]const u8).init(allocator);

    var it = dir.iterate();
    
    while (it.next()) |entry| {
        if (entry.kind == .file) {
            const full_path = try std.fs.path.join(allocator, &.{ ".", entry.name });
            files.append(full_path) catch unreachable;
        }
    }

    // Further processing can be added here
}
