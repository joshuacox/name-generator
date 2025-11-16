#!/usr/bin/env julia
# Julia implementation of the name-generator.sh script.
# Behaves like the original shell script with respect to SEPARATOR,
# NOUN_FILE and ADJ_FILE handling.

using Random
using Printf
using Dates
using Statistics
using Base.Filesystem: isfile, realpath

# ----------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------
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
First checks the `counto` environment variable; if it is set and numeric,
its value is used. Otherwise it tries `tput lines`; if that fails,
it falls back to 24.
"""
function get_counto()
    # Check environment variable first
    env = get(ENV, "counto", "")
    if !isempty(env)
        try
            return parse(Int, strip(env))
        catch
            # Invalid number – ignore and fall back
        end
    end

    # Try tput lines
    try
        lines_str = read(`tput lines`, String)
        return parse(Int, strip(lines_str))
    catch
        # Not available or parsing failed – fall back
    end

    # Default fallback
    return 24
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
    # Return an absolute (real) path for consistency with the shell script
    return realpath(rand(files))
end

"""
    resolve_file(env_var::String, folder::String) -> String

If the environment variable `env_var` is set and points to a regular file,
return its absolute path. Otherwise pick a random regular file from `folder`.
"""
function resolve_file(env_var::String, folder::String)
    val = get(ENV, env_var, "")
    if !isempty(val)
        abs_path = realpath(val)
        if isfile(abs_path)
            return abs_path
        else
            error("Environment variable $env_var points to a non‑regular file: $val")
        end
    else
        return pick_random_file(folder)
    end
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
                adjective::AbstractString, noun::AbstractString,
                noun_file::String, adj_file::String,
                noun_folder::String, adj_folder::String)

Print debugging information to `stderr` when `DEBUG=true`.
"""
function debug_print(iter::Int, counto::Int,
                     adjective::AbstractString, noun::AbstractString,
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
NOUN_FILE = resolve_file("NOUN_FILE", NOUN_FOLDER)
ADJ_FILE  = resolve_file("ADJ_FILE",  ADJ_FOLDER)

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
