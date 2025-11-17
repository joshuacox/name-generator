use "files"
use "os"
use "random"
use "collections"
use "fmt"
use "process"

actor Main
  new create(env: Env) =>
    // -----------------------------------------------
    // Configuration – values are taken from the environment,
    // falling back to the same defaults the shell script uses.
    // -----------------------------------------------

    // Separator between adjective and noun (default " ")
    let separator = match env.var("SEPARATOR")
      | let s: String => s
      | None => " "
    end

    // Number of lines to emit – env var `counto` > `tput lines` > 24
    let count: USize = try
      // env.var returns (String | None); we unwrap each step,
      // raising an error if any part is missing.
      env.var("counto")?.as_int()?.usize()
    else
      // try `tput lines`
      try
        let proc = Process.create(env, "tput", ["lines"])
        proc.wait()
        let out = proc.stdout().read_string()
        out.trim().as_int()?.usize()
      else
        24
      end
    end

    // Base folders (default to sub‑folders of the current working dir)
    let cwd = Path.cwd()
    // Build Path objects from either the env var or the default location.
    let noun_folder = Path.from_string(
      match env.var("NOUN_FOLDER")
        | let s: String => s
        | None => cwd + "/nouns"
      end
    )?
    let adj_folder = Path.from_string(
      match env.var("ADJ_FOLDER")
        | let s: String => s
        | None => cwd + "/adjectives"
      end
    )?

    // Resolve the noun and adjective files (env var > random file in folder)
    let noun_file = resolve_file(env, "NOUN_FILE", noun_folder)?
    let adj_file  = resolve_file(env, "ADJ_FILE",  adj_folder )?

    // Load all non‑empty lines from the two files
    let noun_lines = read_nonempty_lines(noun_file)?
    let adj_lines  = read_nonempty_lines(adj_file )?

    // Debug flag – prints extra information to stderr when true
    let debug = match env.var("DEBUG")
      | let s: String => s == "true"
      | None => false
    end

    // -----------------------------------------------
    // Main generation loop
    // -----------------------------------------------
    var i: USize = 0
    while i < count do
      // Pick a random noun, lower‑casing it.
      let raw_noun = random_choice(noun_lines)?
      let noun = raw_noun.lower()

      // Pick a random adjective (preserve original case).
      let adjective = random_choice(adj_lines)?

      // Optional debug output (mirrors the shell script's `debugger` function)
      if debug then
        env.err.print("DEBUG:")
        env.err.print("  adjective : " + adjective)
        env.err.print("  noun      : " + noun)
        env.err.print("  ADJ_FILE  : " + adj_file.path())
        env.err.print("  ADJ_FOLDER: " + adj_folder.path())
        env.err.print("  NOUN_FILE : " + noun_file.path())
        env.err.print("  NOUN_FOLDER: " + noun_folder.path())
        env.err.print("  " + i.string() + " < " + count.string())
      end

      // Emit the generated name.
      env.out.print(adjective + separator + noun)

      i = i + 1
    end

  // -----------------------------------------------
  // Resolve an environment variable to a regular file.
  // If the variable is unset/blank, pick a random regular file from
  // the supplied folder (same behaviour as the shell script).
  // -----------------------------------------------
  fun resolve_file(env: Env, var_name: String, folder: Path): (Path | None) ? =>
    // 1️⃣  Environment variable overrides
    if let val = env.var(var_name) then
      if not val.trim().is_empty() then
        let p = Path.from_string(val)?
        if p.is_file() then
          return p
        else
          // The script would abort – we raise an error here.
          error
        end
      end
    end

    // 2️⃣  Random regular file from the folder
    let candidates = Array[Path]
    for info in FileInfo.iterate(folder) do
      if info.is_file() then
        candidates.push(Path.from_string(info.path())?)
      end
    end
    if candidates.size() == 0 then error end
    random_choice(candidates)?

  // -----------------------------------------------
  // Read a file and return all non‑empty, trimmed lines.
  // -----------------------------------------------
  fun read_nonempty_lines(file: Path): Array[String] ? =>
    let lines = Array[String]
    let f = OpenFile(file)?
    while not f.eof() do
      let raw = f.read_line()?
      let trimmed = raw.trim()
      if trimmed.size() > 0 then
        lines.push(trimmed)
      end
    end
    f.close()
    lines

  // -----------------------------------------------
  // Pick a random element from an Array.
  // -----------------------------------------------
  fun random_choice[T: (Any #read #send)](arr: Array[T]): T ? =>
    let idx = Random.int(arr.size())
    arr(idx)?
