use anyhow::{Context, Result};
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Resolve an environment variable, falling back to a default value.
fn env_or_default(var: &str, default: impl AsRef<str>) -> String {
    env::var(var).unwrap_or_else(|_| default.as_ref().to_owned())
}

/// Return a random regular file that lives inside `folder`.  Errors if the folder
/// contains no files.
fn pick_random_file(folder: &Path) -> Result<PathBuf> {
    let mut files = Vec::new();
    for entry in WalkDir::new(folder)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.is_file() {
            files.push(path.to_path_buf());
        }
    }

    let mut rng = thread_rng();
    files
        .choose(&mut rng)
        .cloned()
        .with_context(|| format!("No regular files found in {}", folder.display()))
}

/// Read *all* lines from a file into a `Vec<String>`.  Empty lines are kept –
/// they are harmless for the script’s original behaviour.
fn read_lines(file: &Path) -> Result<Vec<String>> {
    let content = fs::read_to_string(file)
        .with_context(|| format!("Failed to read {}", file.display()))?;
    Ok(content.lines().map(|s| s.to_owned()).collect())
}

/// Return the number of rows of the current terminal, falling back to 24 if it
/// cannot be determined.
fn terminal_rows() -> usize {
    termsize::get()
        .map(|size| size.rows as usize) // Access the rows field of the Size struct
        .unwrap_or(24)
}

/// Print the same debug information that the original `debugger` function
/// printed, but only when `DEBUG=true` in the environment.
fn maybe_debug(
    debug: bool,
    adjective: &str,
    noun: &str,
    adj_file: &Path,
    adj_folder: &Path,
    noun_file: &Path,
    noun_folder: &Path,
    countzero: usize,
    counto: usize,
) {
    if debug {
        eprintln!("--- DEBUG ---");
        eprintln!("adjective   : {}", adjective);
        eprintln!("noun        : {}", noun);
        eprintln!("ADJ_FILE    : {}", adj_file.display());
        eprintln!("ADJ_FOLDER  : {}", adj_folder.display());
        eprintln!("NOUN_FILE   : {}", noun_file.display());
        eprintln!("NOUN_FOLDER : {}", noun_folder.display());
        eprintln!("{} > {}", countzero, counto);
        eprintln!("-------------");
    }
}

fn main() -> Result<()> {
    // -------------------------------------------------------------------------
    // 1. Resolve configuration (environment variables, defaults, cwd)
    // -------------------------------------------------------------------------
    let cwd = env::current_dir().context("Unable to get current working directory")?;

    // `counto` – how many names we will emit.  The shell script used `tput lines`.
    // We replicate that with the `termsize` crate, but the user may override it.
    let counto: usize = env::var("counto")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(terminal_rows);

    let noun_folder = PathBuf::from(env_or_default(
        "NOUN_FOLDER",
        cwd.join("nouns").to_string_lossy(),
    ));
    let adj_folder = PathBuf::from(env_or_default(
        "ADJ_FOLDER",
        cwd.join("adjectives").to_string_lossy(),
    ));

    // -------------------------------------------------------------------------
    // 2. Pick random files inside the two folders (mimics the original `shuf …`)
    // -------------------------------------------------------------------------
    let noun_file = pick_random_file(&noun_folder)
        .with_context(|| format!("Could not pick a noun file from {}", noun_folder.display()))?;
    let adj_file = pick_random_file(&adj_folder)
        .with_context(|| format!("Could not pick an adjective file from {}", adj_folder.display()))?;

    // Resolve to absolute paths (like `realpath` in the shell script)
    let noun_file = noun_file.canonicalize()?;
    let adj_file = adj_file.canonicalize()?;

    // -------------------------------------------------------------------------
    // 3. Load the whole files into memory – cheap for the tiny word‑lists we
    //    expect, and lets us pick a random line with O(1) work.
    // -------------------------------------------------------------------------
    let noun_lines = read_lines(&noun_file)?;
    let adj_lines = read_lines(&adj_file)?;

    // Sanity check: both files must contain at least one line, otherwise the
    // script would have hung forever.
    if noun_lines.is_empty() {
        anyhow::bail!("Noun file {} is empty", noun_file.display());
    }
    if adj_lines.is_empty() {
        anyhow::bail!("Adjective file {} is empty", adj_file.display());
    }

    // -------------------------------------------------------------------------
    // 4. Main loop – emit `counto` names
    // -------------------------------------------------------------------------
    let debug = env::var("DEBUG")
        .map(|v| v.eq_ignore_ascii_case("true"))
        .unwrap_or(false);

    let mut rng = thread_rng();
    for countzero in 0..counto {
        // Randomly pick a line from each vector
        let noun_raw = noun_lines.choose(&mut rng).unwrap();
        let adj_raw = adj_lines.choose(&mut rng).unwrap();

        // The shell script lower‑cased the noun and left the adjective untouched.
        let noun = noun_raw.to_ascii_lowercase();
        let adjective = adj_raw.to_string();

        // Debug output (goes to stderr, just like `set -x` would have shown)
        maybe_debug(
            debug,
            &adjective,
            &noun,
            &adj_file,
            &adj_folder,
            &noun_file,
            &noun_folder,
            countzero,
            counto,
        );

        // The final name: “adjective‑noun”
        println!("{}-{}", adjective, noun);
    }

    Ok(())
}
