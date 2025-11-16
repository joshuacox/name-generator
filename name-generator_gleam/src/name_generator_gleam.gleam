import gleam/io
import gleam/string
import gleam/list
import gleam/random
import gleam/erlang
import gleam/result.{Ok, Error}

pub fn main() {
  // Separator (default "-")
  let separator =
    case erlang.get_env("SEPARATOR") {
      Ok(s) -> s,
      Error(_) -> "-",
    }

  // Number of lines to emit (default 24)
  let count_o =
    case erlang.get_env("counto") {
      Ok(s) ->
        case string.to_int(s) {
          Ok(i) -> i,
          Error(_) -> 24,
        },
      Error(_) -> 24,
    }

  // Resolve noun and adjective files
  let noun_file = resolve_file("NOUN_FILE", "NOUN_FOLDER", "./nouns")
  let adj_file = resolve_file("ADJ_FILE", "ADJ_FOLDER", "./adjectives")

  // Read non‑empty trimmed lines from each file
  let noun_lines = read_nonempty_lines(noun_file)
  let adj_lines = read_nonempty_lines(adj_file)

  // Generate and print the requested number of names
  generate_and_print(count_o, separator, noun_lines, adj_lines)
}

// Resolve a file path from an environment variable or, if not set,
// pick a random regular file from the given folder (default folder if its env var is unset)
fn resolve_file(
  file_env: String,
  folder_env: String,
  default_folder: String,
) -> String {
  // If the file env var is set, use it directly
  case erlang.get_env(file_env) {
    Ok(path) -> path,
    Error(_) ->
      // Determine the folder to search
      case erlang.get_env(folder_env) {
        Ok(folder) -> pick_random_file(folder),
        Error(_) -> pick_random_file(default_folder),
      },
  }
}

// Pick a random regular file from `folder`.
// Panics if the folder cannot be listed or contains no entries.
fn pick_random_file(folder: String) -> String {
  // List entries in the folder
  let entries =
    case erlang.list_dir(folder) {
      Ok(es) -> es,
      Error(_) -> io:panic("Cannot list folder " <> folder),
    }

  // Ensure there is at least one entry
  let len = List.length(entries)
  if len == 0 {
    io:panic("No files found in folder " <> folder)
  } else {
    // Pick a random entry (the original scripts assume all entries are regular files)
    // `random.int` is inclusive, so we use `len - 1` as the upper bound.
    let idx = random.int(0, len - 1)
    case List.at(entries, idx) {
      Some(name) -> folder <> "/" <> name,
      None -> io:panic("Failed to pick a file from folder " <> folder),
    }
  }
}

// Read a file, split on newlines, trim whitespace, and drop empty lines
fn read_nonempty_lines(path: String) -> List(String) {
  let content =
    case io:read_file(path) {
      Ok(c) -> c,
      Error(_) -> io:panic("Cannot read file " <> path),
    }

  content
  |> string.split("\n")
  |> List.map(fn(line) { string.trim(line) })
  |> List.filter(fn(line) { line != "" })
}

// Generate `count` names and print each on its own line
fn generate_and_print(
  count: Int,
  separator: String,
  nouns: List(String),
  adjs: List(String),
) {
  // Recursive helper to emit the required number of names
  let rec loop = fn(remaining: Int) -> 
    if remaining <= 0 {
      Nil
    } else {
      // Pick random noun and adjective
      let noun = random_choice(nouns) |> string.to_lowercase
      let adj = random_choice(adjs)

      // Build the name and output
      let name = adj <> separator <> noun
      io:println(name)

      // Continue with one fewer iteration
      loop(remaining - 1)
    }

  // Start the recursion
  loop(count)
  Nil
}

// Helper: pick a random element from a non‑empty list
fn random_choice(list: List(a)) -> a {
  let len = List.length(list)
  // `random.int` is inclusive, so we use `len - 1` as the upper bound.
  let idx = random.int(0, len - 1)
  case List.at(list, idx) {
    Some(v) -> v,
    None -> io:panic("Attempted to pick from an empty list"),
  }
}
