#!/usr/bin/env julia
# Julia implementation of the name-generator.sh script.
# Respects the following environment variables:
#   SEPARATOR   – string placed between adjective and noun (default "-")
#   NOUN_FILE   – path to a noun list file (if not set, a random file from NOUN_FOLDER is used)
#   ADJ_FILE    – path to an adjective list file (if not set, a random file from ADJ_FOLDER is used)
#   NOUN_FOLDER – folder containing noun list files (default "./nouns")
#   ADJ_FOLDER  – folder containing adjective list files (default "./adjectives")
#   counto      – number of lines to emit (overrides terminal height)
#   DEBUG       – when set to "true", prints debug information to stderr

using Random
using Printf

# ----------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------
"""
    env_or_default(var::String, default::String) -> String

Return the value of environment variable `var` if it exists and is non‑empty,
otherwise return `default`.
"""
function env_or_default(var::String, default::String)
    val = get(ENV, var, "")
    return (isempty(val) ? default : val)
end

"""
    get_counto() -> Int

Determine how many names to generate.
First tries `tput lines`; if that fails, checks the `counto` environment variable;
finally falls back to 24.
"""
function get_counto()
    # Try tput lines
    try
        lines_str = read(`tput lines`, String)
        return parse(Int, strip(lines_str))
    catch
        # Not available or parsing failed – try env var
        env = get(ENV, "counto", "")
        if !isempty(env)
            try
                return parse(Int, strip(env))
            catch
                # ignore and fall back
            end
        end
        # Default fallback
        return 24
    end
end

"""
    pick_random_file(folder::String) -> String

Return the absolute path of a random regular file inside `folder`.
Throws an error if the folder contains no regular files.
"""
function pick_random_file(folder::String)
    entries = readdir(folder; join=true)
    files = filter(isfile, entries)
    if isempty(files)
        error("Folder $folder does not contain any regular files")
    end
    return rand(files)
end

"""
    read_nonempty_lines(path::String) -> Vector{String}

Read all non‑empty, trimmed lines from `path`.
"""
function read_nonempty_lines(path::String)
    lines = readlines(path)
    return [strip(l) for l in lines if !isempty(strip(l))]
end

"""
    debug_print(iter::Int, counto::Int,
                adjective::String, noun::String,
                noun_file::String, adj_file::String,
                noun_folder::String, adj_folder::String)

Print debugging information to `stderr` when `DEBUG=true`.
"""
function debug_print(iter::Int, counto::Int,
                     adjective::String, noun::String,
                     noun_file::String, adj_file::String,
                     noun_folder::String, adj_folder::String)
    if get(ENV, "DEBUG", "") == "true"
        @printf(stderr, "DEBUG (iteration %d/%d):\n", iter, counto)
        @printf(stderr, "  adjective : %s\n", adjective)
        @printf(stderr, "  noun      : %s\n", noun)
        @printf(stderr, "  NOUN_FILE : %s\n", noun_file)
        @printf(stderr, "  ADJ_FILE  : %s\n", adj_file)
        @printf(stderr, "  NOUN_FOLDER: %s\n", noun_folder)
        @printf(stderr, "  ADJ_FOLDER : %s\n", adj_folder)
    end
end

# ----------------------------------------------------------------------
# Configuration (environment → defaults)
# ----------------------------------------------------------------------
HERE = pwd()

SEPARATOR = env_or_default("SEPARATOR", "-")
COUNT_O   = get_counto()

NOUN_FOLDER = env_or_default("NOUN_FOLDER", joinpath(HERE, "nouns"))
ADJ_FOLDER  = env_or_default("ADJ_FOLDER",  joinpath(HERE, "adjectives"))

# Resolve files – env var overrides, otherwise pick a random file from the folder.
NOUN_FILE = env_or_default("NOUN_FILE", pick_random_file(NOUN_FOLDER))
ADJ_FILE  = env_or_default("ADJ_FILE",  pick_random_file(ADJ_FOLDER))

# Pre‑load the files' contents (filtering empty lines)
noun_lines = read_nonempty_lines(NOUN_FILE)
adj_lines  = read_nonempty_lines(ADJ_FILE)

if isempty(noun_lines)
    error("Noun list ($NOUN_FILE) is empty")
end
if isempty(adj_lines)
    error("Adjective list ($ADJ_FILE) is empty")
end

# ----------------------------------------------------------------------
# Main generation loop
# ----------------------------------------------------------------------
for i in 1:COUNT_O
    # Pick random noun and adjective
    noun_raw = rand(noun_lines)
    adj_raw  = rand(adj_lines)

    noun = lowercase(noun_raw)   # mimic shell script lower‑casing of noun
    adjective = adj_raw          # preserve original case

    debug_print(i, COUNT_O, adjective, noun,
                NOUN_FILE, ADJ_FILE, NOUN_FOLDER, ADJ_FOLDER)

    println(string(adjective, SEPARATOR, noun))
end
