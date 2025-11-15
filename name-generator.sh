#!/usr/bin/env sh
# robust name generator – fixes “exec failed (No such file or directory; errno=2)”
# The original script relied on commands that may not be present on all systems
# (realpath, shuf, tput).  This version adds fall‑backs so the script can run
# everywhere a POSIX‑compatible /bin/sh is available.

HERE=$(pwd)

# ----------------------------------------------------------------------
# Helper: test whether a command exists in $PATH
# ----------------------------------------------------------------------
command_exists() {
  command -v "$1" >/dev/null 2>&1
}

# ----------------------------------------------------------------------
# Resolve a path to an absolute canonical form.
# Prefer `realpath`; fall back to `readlink -f`; otherwise return the input.
# ----------------------------------------------------------------------
realpath_fallback() {
  if command_exists realpath; then
    realpath "$1"
  elif command_exists readlink; then
    readlink -f "$1"
  else
    # No canonicalisation tool – just return the argument unchanged.
    printf '%s\n' "$1"
  fi
}

# ----------------------------------------------------------------------
# Pick a random regular file from a directory.
# Prefer `shuf`; fall back to `sort -R | head -n1`.
# ----------------------------------------------------------------------
random_file_from() {
  dir=$1
  if command_exists shuf; then
    find "$dir" -type f | shuf -n 1
  else
    # `sort -R` is POSIX‑compatible and provides random ordering.
    find "$dir" -type f | sort -R | head -n 1
  fi
}

# ----------------------------------------------------------------------
# Configuration – environment overrides with sensible defaults
# ----------------------------------------------------------------------
: "${SEPARATOR:="-"}"

# Number of lines to emit – use `tput lines` when available, otherwise 24.
if command_exists tput; then
  : "${counto:=$(tput lines)}"
else
  : "${counto:=24}"
fi

: "${NOUN_FOLDER:=${HERE}/nouns}"
: "${ADJ_FOLDER:=${HERE}/adjectives}"

# Resolve files – env var overrides, otherwise pick a random file from the folder.
: "${NOUN_FILE:=$(realpath_fallback "$(random_file_from "$NOUN_FOLDER")")}"
: "${ADJ_FILE:=$(realpath_fallback "$(random_file_from "$ADJ_FOLDER")")}"

# ----------------------------------------------------------------------
# Debug helper – prints to stderr when DEBUG=true
# ----------------------------------------------------------------------
debugger() {
  if [ "${DEBUG}" = "true" ]; then
    # Enable shell tracing for extra visibility (optional)
    set -x
    printf '%s\n' "$this_adjective"
    printf '%s\n' "$this_noun"
    printf '%s\n' "$ADJ_FILE"
    printf '%s\n' "$ADJ_FOLDER"
    printf '%s\n' "$NOUN_FILE"
    printf '%s\n' "$NOUN_FOLDER"
    printf '%s > %s\n' "$countzero" "$counto"
  fi
}

# ----------------------------------------------------------------------
# Main generation loop
# ----------------------------------------------------------------------
countzero=0
while [ "$countzero" -lt "$counto" ]; do
  # Pick a random noun, lower‑casing it.
  this_noun=$(shuf -n 1 "$NOUN_FILE" 2>/dev/null || \
              sort -R "$NOUN_FILE" | head -n 1)
  this_noun=$(printf '%s' "$this_noun" | tr '[:upper:]' '[:lower:]')

  # Pick a random adjective (preserve original case).
  this_adjective=$(shuf -n 1 "$ADJ_FILE" 2>/dev/null || \
                  sort -R "$ADJ_FILE" | head -n 1)

  debugger

  printf "%s%s%s\n" "$this_adjective" "$SEPARATOR" "$this_noun"

  # Increment counter in a POSIX‑compatible way.
  countzero=$((countzero + 1))
done
