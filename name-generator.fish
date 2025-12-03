#!/usr/bin/env fish
# ---------------------------------
# Fish implementation of name-generator.sh
# ---------------------------------
# Behaviour (identical to the shell script):
#   * SEPARATOR   – defaults to "-"
#   * NOUN_FILE   – env‑var override, otherwise a random regular file
#                   from $NOUN_FOLDER (default: $PWD/nouns)
#   * ADJ_FILE    – same logic, using $ADJ_FOLDER (default: $PWD/adjectives)
#   * counto      – number of lines to emit; env‑var overrides,
#                   otherwise use `tput lines` (fallback 24)
#   * noun lines are lower‑cased, adjective lines keep original case
# ---------------------------------

# ---------- helpers ----------
function __cmd_exists
    # true if command is in $PATH
    command -v $argv[1] > /dev/null 2>&1
end

function __realpath_fallback
    # Return an absolute canonical path if possible, otherwise the argument unchanged
    if __cmd_exists realpath
        realpath $argv[1]
    else if __cmd_exists readlink
        readlink -f $argv[1]
    else
        printf '%s\n' $argv[1]
    end
end

function __random_file_from
    # Pick a random regular file from a directory
    set dir $argv[1]
    if __cmd_exists shuf
        find $dir -type f | shuf -n 1
    else
        # POSIX‑compatible fallback
        find $dir -type f | sort -R | head -n 1
    end
end

# ---------- configuration ----------
# Current working directory (same as the shell script's $HERE)
set -l HERE (pwd)

# Separator (env var overrides, default "-")
set -l SEPARATOR (set -q SEPARATOR; and echo $SEPARATOR; or echo "-")

# Count of lines to emit (env var `counto` overrides)
if set -q counto
    set -l COUNT_O $counto
else if set -q COUNT
    # Some shells export the variable in upper case; accept it as a fallback
    set -l COUNT_O $COUNT
else if __cmd_exists tput
    set -l COUNT_O (tput lines)
    # tput may fail (e.g. no tty); fall back to 24
    if test -z "$COUNT_O"
        set COUNT_O 24
    end
else
    set -l COUNT_O 24
end

# -------------------------------------------------------------------------------
# Defensive fallback: if COUNT_O is still empty (or not a positive integer)
# use the hard‑coded default of 24.  This protects the script when it is
# invoked from environments (like fish) that don’t inherit a lower‑case
# `counto` variable.
# -------------------------------------------------------------------------------
if test -z "$COUNT_O"
    set COUNT_O 24
end

# Ensure COUNT_O is a number – if it isn’t, replace with the default.
if not string match -qr '^[0-9]+$' "$COUNT_O"
    set COUNT_O 24
end

# Folder defaults (same defaults as the shell script)
set -l NOUN_FOLDER   (printf '%s' $HERE/nouns)
set -l ADJ_FOLDER    (printf '%s' $HERE/adjectives)

# Resolve noun/adjective files (env‑var overrides, otherwise pick random file)
if set -q NOUN_FILE
    set -l NOUN_FILE_PATH (__realpath_fallback $NOUN_FILE)
else
    set -l NOUN_FILE_PATH (__realpath_fallback (__random_file_from $NOUN_FOLDER))
end

if set -q ADJ_FILE
    set -l ADJ_FILE_PATH (__realpath_fallback $ADJ_FILE)
else
    set -l ADJ_FILE_PATH (__realpath_fallback (__random_file_from $ADJ_FOLDER))
end

# ---------- main loop ----------
set -l i 0
while test $i -lt $COUNT_O
    # ----- pick a random noun (lower‑cased) -----
    if __cmd_exists shuf
        set -l noun_line (shuf -n 1 $NOUN_FILE_PATH)
    else
        set -l noun_line (sort -R $NOUN_FILE_PATH | head -n 1)
    end
    set -l noun_line (string lower $noun_line)

    # ----- pick a random adjective (preserve case) -----
    if __cmd_exists shuf
        set -l adj_line (shuf -n 1 $ADJ_FILE_PATH)
    else
        set -l adj_line (sort -R $ADJ_FILE_PATH | head -n 1)
    end

    # ----- output -----
    printf '%s%s%s\n' "$adj_line" "$SEPARATOR" "$noun_line"

    # increment loop counter (POSIX‑compatible arithmetic)
    set i (math "$i + 1")
end
