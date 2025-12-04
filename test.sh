#!/usr/bin/env bash
set -eu
zig build-exe -I . name-generator_zig.zig -lc
