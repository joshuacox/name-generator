import gleam/io
import gleam/result.{Result, Ok, Error}
import gleam/list.{List, map, filter_map, length, at}
import gleam/string.{String, split, trim, to_lowercase}
import gleam/int.{Int, from_string}
import gleam/random.{int}
import gleam/option.{Option, Some, Nil}
import gleam/erlang.{lookup_env, list_dir, file_info, file_info_type, regular}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

// Get an environment variable or return a default value.
pub fn env(var_name: String, default: String) -> String {
  case lookup_env(var_name) {
    Some(value) -> value
    Nil -> default
  }
}

// Parse an integer from a string, falling back to `fallback` on error.
pub fn parse_int(str: String, fallback: Int) -> Int {
  case from_string(str) {
    Ok(i) -> i
    Error(_) -> fallback
  }
}

// Read a file and return a list of its non‑empty, trimmed lines.
pub fn read_non_empty_lines(path: String) -> Result(List(String), String) {
  // `io.read_file` returns `Result(String, IOError)`. We map the success case.
  io.read_file(path)
  |> Result.map(fn contents ->
       contents
       |> split("\n")
       |> filter_map(fn line ->
            let trimmed = trim(line)
            if trimmed == "" { Nil } else { Some(trimmed) }
          end)
     end)
}

// List regular files inside a directory (non‑recursive).
pub fn list_regular_files(dir: String) -> Result(List(String), String) {
  list_dir(dir)
  |> Result.map(fn entries ->
       entries
       |> filter_map(fn entry ->
            let full_path = dir <> "/" <> entry
            // `file_info` returns `{ok, Info}` where `Info.type` tells us if it is a regular file.
            case file_info(full_path) {
              Ok(info) ->
                case file_info_type(info) {
                  regular -> Some(full_path)
                  _ -> Nil
                }
              Error(_) -> Nil
            }
          end)
     end)
}

// Pick a random element from a non‑empty list.
pub fn random_choice<T>(list: List(T)) -> T {
  let len = length(list)
  // `int(min, max)` returns a random integer inclusive of both bounds.
  let idx = int(0, len - 1)
  // `at` returns `Option(T)`, we can safely unwrap because the list is non‑empty.
  at(list, idx)
  |> case {
    Some(v) -> v
    Nil -> panic("random_choice called with empty list")
  }
}

// Resolve a file path from an environment variable or, if not set, pick a random file
// from the supplied folder.
pub fn resolve_file(env_var: String, folder: String) -> Result(String, String) {
  case lookup_env(env_var) {
    Some(path) -> Ok(path)
    Nil ->
      // No explicit file, pick a random one from the folder.
      list_regular_files(folder)
      |> Result.map(fn files ->
           random_choice(files)
         end)
  }
}

// -----------------------------------------------------------------------------
// Main program
// -----------------------------------------------------------------------------

pub fn main() -> Nil {
  // Configuration (mirrors name-generator.sh)
  let separator = env("SEPARATOR", "-")
  let count_o = env("counto", "24") |> parse_int(_, 24)

  // Determine folders (environment overrides or defaults)
  let here = "." // current working directory
  let noun_folder = env("NOUN_FOLDER", here <> "/nouns")
  let adj_folder  = env("ADJ_FOLDER",  here <> "/adjectives")

  // Resolve the noun and adjective files
  let noun_file_result = resolve_file("NOUN_FILE", noun_folder)
  let adj_file_result  = resolve_file("ADJ_FILE",  adj_folder)

  // If either resolution fails, abort with an error message.
  let noun_file = case noun_file_result {
    Ok(p) -> p
    Error(e) -> panic("Failed to resolve noun file: " <> e)
  }

  let adj_file = case adj_file_result {
    Ok(p) -> p
    Error(e) -> panic("Failed to resolve adjective file: " <> e)
  }

  // Load the contents of the files.
  let nouns_result = read_non_empty_lines(noun_file)
  let adjs_result  = read_non_empty_lines(adj_file)

  let nouns = case nouns_result {
    Ok(l) -> l
    Error(e) -> panic("Failed to read noun file: " <> e)
  }

  let adjs = case adjs_result {
    Ok(l) -> l
    Error(e) -> panic("Failed to read adjective file: " <> e)
  }

  // Emit `count_o` lines.
  let _ = loop(0, count_o, fn(_i) -> Nil {
    // Pick random entries.
    let noun = random_choice(nouns) |> to_lowercase
    let adj  = random_choice(adjs)

    // Print the combined name.
    io.println(adj <> separator <> noun)
  })

  Nil
}

// Simple loop helper (tail‑recursive) to run an action `n` times.
pub fn loop(i: Int, limit: Int, f: fn(Int) -> Nil) -> Nil {
  if i >= limit {
    Nil
  } else {
    f(i)
    loop(i + 1, limit, f)
  }
}
