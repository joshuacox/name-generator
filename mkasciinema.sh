#!/usr/bin/env bash
set -eux
time asciinema rec --command "./benchmark.sh" benchmakr.cast
