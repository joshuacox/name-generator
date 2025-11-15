const std = @import("std");
const root = @import("root.zig");

pub fn main() !void {
    // Generate names using the same environment‑variable interface as the shell script.
    // Any errors (e.g., missing files) are propagated to the runtime and will cause a non‑zero exit.
    try root.generateNames();
}
