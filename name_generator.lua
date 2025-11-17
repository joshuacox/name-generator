#!/usr/bin/env lua
--[[
  Lua implementation of the original `name-generator.sh`.
  It respects the same environment variables:

    SEPARATOR   – string placed between adjective and noun (default "-")
    counto      – number of lines to emit (overrides terminal height)
    NOUN_FILE   – explicit noun list file (otherwise a random file from NOUN_FOLDER)
    ADJ_FILE    – explicit adjective list file (otherwise a random file from ADJ_FOLDER)
    NOUN_FOLDER – folder that contains noun list files (default "./nouns")
    ADJ_FOLDER  – folder that contains adjective list files (default "./adjectives")
    DEBUG       – when set to "true" prints debugging information to stderr
--]]

------- ------------------------------------------ -------------------
-- Helper utilities
------- ------------------------------------------ -------------------
local function quote(str)
    -- simple quoting for shell usage
    return "'" .. (str:gsub("'", "'\\''")) .. "'"
end

local function command_exists(cmd)
    -- `command -v` is POSIX; we silence all output.
    local ok = os.execute(string.format("command -v %s > /dev/null 2>&1", cmd))
    return ok == true or ok == 0
end

local function exec_capture(cmd)
    local f = io.popen(cmd .. " 2>/dev/null")
    if not f then return nil end
    local out = f:read("*a")
    f:close()
    return out:gsub("%s+$", "")   -- trim trailing newline/space
end

local function realpath_fallback(p)
    if command_exists("realpath") then
        return exec_capture(string.format("realpath %s", quote(p))) or p
    elseif command_exists("readlink") then
        return exec_capture(string.format("readlink -f %s", quote(p))) or p
    else
        return p
    end
end

local function list_regular_files(dir)
    local files = {}
    -- Prefer LuaFileSystem if present
    local ok, lfs = pcall(require, "lfs")
    if ok and lfs.attributes then
        for entry in lfs.dir(dir) do
            if entry ~= "." and entry ~= ".." then
                local full = dir .. "/" .. entry
                local attr = lfs.attributes(full)
                if attr and attr.mode == "file" then
                    table.insert(files, full)
                end
            end
        end
    else
        -- Fallback to `find` (POSIX)
        local cmd = string.format("find %s -type f", quote(dir))
        local out = exec_capture(cmd)
        if out then
            for line in out:gmatch("[^\n]+") do
                table.insert(files, line)
            end
        end
    end
    return files
end

local function random_file_from(dir)
    local files = list_regular_files(dir)
    if #files == 0 then
        error(string.format("Folder %s does not contain any regular files", dir))
    end
    return files[math.random(#files)]
end

local function read_all_lines(path)
    local lines = {}
    for line in io.lines(path) do
        line = line:gsub("^%s+", ""):gsub("%s+$", "")
        if line ~= "" then
            table.insert(lines, line)
        end
    end
    return lines
end

------- ------------------------------------------ -------------------
-- Configuration (environment → defaults)
------- ------------------------------------------ -------------------
local HERE = exec_capture("pwd") or "."   -- current working directory

local SEPARATOR = os.getenv("SEPARATOR") or "-"

-- ----------------------------------------------------------------
-- Determine how many lines to emit (COUNT_O)
-- ------------------------------------------------------------------------------------------------
local function get_count_o()
    -- 1) env var `counto`
    local env = os.getenv("counto")
    if env then
        local n = tonumber(env)
        if n then return n end
    end
    -- 2) try `tput lines`
    if command_exists("tput") then
        local out = exec_capture("tput lines")
        local n = tonumber(out)
        if n then return n end
    end
    -- 3) fallback
    return 24
end

local COUNT_O = get_count_o()

-- ------------------------------------------------------------------------------------------------
-- Resolve folders (may be overridden by env)
-- ------------------------------------------------------------------------------------------------
local NOUN_FOLDER   = os.getenv("NOUN_FOLDER")   or (HERE .. "/nouns")
local ADJ_FOLDER    = os.getenv("ADJ_FOLDER")    or (HERE .. "/adjectives")

-- ------------------------------------------------------------------------------------------------
-- Resolve files (env var takes precedence, otherwise pick random)
-- ------------------------------------------------------------------------------------------------
local function resolve_file(env_name, folder)
    local val = os.getenv(env_name)
    if val and val:match("%S") then
        local p = realpath_fallback(val)
        local attr = io.open(p)
        if not attr then
            error(string.format("Environment variable %s points to a non‑regular file: %s", env_name, val))
        end
        attr:close()
        return p
    else
        return random_file_from(folder)
    end
end

local NOUN_FILE = resolve_file("NOUN_FILE", NOUN_FOLDER)
local ADJ_FILE  = resolve_file("ADJ_FILE",  ADJ_FOLDER)

-- ------------------------------------------------------------------------------------------------
-- Load the word lists
-- ------------------------------------------------------------------------------------------------
local noun_lines = read_all_lines(NOUN_FILE)
local adj_lines  = read_all_lines(ADJ_FILE)

if #noun_lines == 0 then error("Noun list is empty") end
if #adj_lines  == 0 then error("Adjective list is empty") end

------- ------------------------------------------ -------------------
-- Debug helper (mirrors the shell script's `debugger` function)
------- ------------------------------------------ -------------------
local function maybe_debug(adjective, noun)
    if os.getenv("DEBUG") == "true" then
        io.stderr:write("DEBUG:\n")
        io.stderr:write(string.format("  adjective : %s\n", adjective))
        io.stderr:write(string.format("  noun      : %s\n", noun))
        io.stderr:write(string.format("  ADJ_FILE  : %s\n", ADJ_FILE))
        io.stderr:write(string.format("  ADJ_FOLDER: %s\n", ADJ_FOLDER))
        io.stderr:write(string.format("  NOUN_FILE : %s\n", NOUN_FILE))
        io.stderr:write(string.format("  NOUN_FOLDER: %s\n", NOUN_FOLDER))
        io.stderr:write(string.format("  %d > %d\n", 0, COUNT_O))
    end
end

------- ------------------------------------------ -------------------
-- Main generation loop
------- ------------------------------------------ -------------------
math.randomseed(os.time())

for i = 1, COUNT_O do
    local noun_raw = noun_lines[math.random(#noun_lines)]
    local adj_raw  = adj_lines[math.random(#adj_lines)]

    local noun = string.lower(noun_raw)
    local adjective = adj_raw   -- keep original case

    maybe_debug(adjective, noun)

    print(adjective .. SEPARATOR .. noun)
end
