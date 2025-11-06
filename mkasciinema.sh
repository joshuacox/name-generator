#!/usr/bin/env bash
set -eux
time asciinema rec --command "./meta-benchmark.sh" benchmark.cast
