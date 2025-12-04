import gleam/io
import gleam/string
import gleam/list
import gleam/result.{Result, Ok, Error}
import gleam/option.{Option, Some, None}
import gleam/random
import gleam/erlang

pub fn main() {
  // Separator (default "-")
  let separator: String =
    case erlang.get_env("SEPARATOR") {
      Some(s) -> s,
      None -> "-"
    }

  // Number of lines to emit (default 24)
  let count_o: Int =
    case erlang.get_env("counto") {
      Some(s) ->
        case string.to_int(s) {
          Ok(i) -> i,
          Error(_) -> 24
        },
      None -> 24
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
  // If the environment variable points to a file, use it.
  // Otherwise pick a random regular file from the appropriate folder.
  case erlang.get_env(file_env) {
    Some(path) -> path,
    None -> resolve_from_folder(folder_env, default_folder)
  }
}

fn resolve_from_folder(
  folder_env: String,
  default_folder: String,
) -> String {
  // Determine which folder to use
  let folder =
    case erlang.get_env(folder_env) {
      Some(f) -> f,
      None -> default_folder
    }

  // Get the list of entries in that folder
  let entries =
    case erlang.list_dir(folder) {
      Ok(es) -> es,
      Error(_) -> io.panic("Cannot list folder " <> folder)
    }

  // Pick a random entry
  let idx = random.int(0, list.length(entries))
  case list.at(entries, idx) {
    Ok(name) -> folder <> "/" <> name,
    Error(_) -> io.panic("No files found in folder " <> folder)
  }
}

// Read a file, split on newlines, trim whitespace, and drop empty lines
fn read_nonempty_lines(path: String) -> List(String) {
  let content =
    case io.read_file(path) {
      Ok(c) -> c,
      Error(_) -> io.panic("Cannot read file " <> path)
    }

  content
  |> string.split("\n")
  |> list.map(fn(line) { string.trim(line) })
  |> list.filter(fn(line) { line != "" })
}

// Generate `count` names and print each on its own line
fn generate_and_print(
  count: Int,
  separator: String,
  nouns: List(String),
  adjs: List(String),
) {
  let rec loop = fn(remaining: Int) {
    if remaining <= 0 {
      Nil
    } else {
      // Pick random noun and adjective
      let noun = random_choice(nouns) |> string.to_lowercase
      let adj = random_choice(adjs)

      // Build the name and output it
      let name = adj <> separator <> noun
      io.println(name)

      // Continue
      loop(remaining - 1)
    }
  }

  loop(count)
  Nil
}

// Helper: pick a random element from a non‑empty list
fn random_choice(items: List(a)) -> a {
  let len = list.length(items)
  let idx = random.int(0, len)
  case list.at(items, idx) {
    Ok(v) -> v,
    Error(_) -> io.panic("Attempted to pick from an empty list")
  }
}
 // End of file – compiled with panic imported unqualified
