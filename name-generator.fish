#!/usr/bin/env fish
# name-generator.fish
# --------------------------------------------------------------
# Generates random “adjective‑separator‑noun” lines.
# Behaviour matches name-generator.sh:
#   * SEPARATOR defaults to "-"
#   * COUNT (or counto) defaults to `tput lines` or 24
#   * NOUN_FILE / ADJ_FILE may be overridden via env vars,
#     otherwise a random regular file is chosen from the
#     respective folder (default $PWD/nouns and $PWD/adjectives)
#   * Nouns are lower‑cased, adjectives keep original case.
# --------------------------------------------------------------

# -------------------------------------------------
# Helper: test whether a command exists in $PATH
# -------------------------------------------------
function command_exists
    command -v $argv[1] > /dev/null
end

# -------------------------------------------------
# Resolve a path to an absolute canonical form.
# Prefer `realpath`; fall back to `readlink -f`; otherwise return the input.
# -------------------------------------------------
function realpath_fallback
    if command_exists realpath
        realpath $argv[1]
    else if command_exists readlink
        readlink -f $argv[1]
    else
        printf '%s\n' $argv[1]
    end
end

# -------------------------------------------------
# Pick a random regular file from a directory.
# Prefer `shuf`; fall back to `sort -R | head -n1`.
# -------------------------------------------------
function random_file_from
    set dir $argv[1]
    if command_exists shuf
        find $dir -type f | shuf -n 1
    else
        find $dir -type f | sort -R | head -n 1
    end
end

# -------------------------------------------------
# Configuration – environment overrides with sensible defaults
# -------------------------------------------------
set -q SEPARATOR; or set SEPARATOR "-"

# Number of lines to emit – use `tput lines` when available, otherwise 24.
if command_exists tput
    set -q counto; or set counto (tput lines)
else
    set -q counto; or set counto 24
end

set -q HERE; or set HERE (pwd)

set -q NOUN_FOLDER;  or set NOUN_FOLDER "$HERE/nouns"
set -q ADJ_FOLDER;   or set ADJ_FOLDER  "$HERE/adjectives"

# -------------------------------------------------
# Resolve noun/adjective files – env var overrides, otherwise pick random.
# -------------------------------------------------
if set -q NOUN_FILE
    set NOUN_FILE (realpath_fallback $NOUN_FILE)
else
    set NOUN_FILE (realpath_fallback (random_file_from $NOUN_FOLDER))
end

if set -q ADJ_FILE
    set ADJ_FILE (realpath_fallback $ADJ_FILE)
else
    set ADJ_FILE (realpath_fallback (random_file_from $ADJ_FOLDER))
end

# -------------------------------------------------
# Main generation loop
# -------------------------------------------------
set countzero 0
while test $countzero -lt $counto
    # Pick a random noun, lower‑casing it.
    if command_exists shuf
        set this_noun (shuf -n 1 $NOUN_FILE 2>/dev/null | tr '[:upper:]' '[:lower:]')
    else
        set this_noun (sort -R $NOUN_FILE | head -n 1 | tr '[:upper:]' '[:lower:]')
    end

    # Pick a random adjective (preserve original case).
    if command_exists shuf
        set this_adj (shuf -n 1 $ADJ_FILE 2>/dev/null)
    else
        set this_adj (sort -R $ADJ_FILE | head -n 1)
    end

    printf "%s%s%s\n" "$this_adj" "$SEPARATOR" "$this_noun"

    # Increment counter (POSIX‑compatible arithmetic)
    set countzero (math "$countzero + 1")
end
