use "files"
use "random"
use "process"
use "collections"
use "strings"

actor Main
  new create(env: Env) =>
    // --------------------------------------------------------
    // 1️⃣  Configuration – environment variables (with the same
    //     defaults as the original shell script)
    // --------------------------------------------------------
    let separator = if env.vars("SEPARATOR").size() > 0 then
        let s = env.vars("SEPARATOR")(0)
        if s != "" then s else "-" end
      else
        "-"
      end

    // Count of lines to emit – env var `counto` (or `COUNT`) wins,
    // otherwise try `tput lines`, finally fall back to 24.
    let count = this._determine_count(env)

    // Resolve the noun / adjective files.
    //   * If $NOUN_FILE (or $ADJ_FILE) is set and points to a regular file,
    //     use it.
    //   * Otherwise pick a random regular file from the corresponding folder.
    let noun_file   = this._resolve_file(env, "NOUN_FILE",   this._default_folder(env, "NOUN_FOLDER"))
    let adj_file    = this._resolve_file(env, "ADJ_FILE",    this._default_folder(env, "ADJ_FOLDER"))

    // Load the word lists (non‑empty, trimmed lines).
    let nouns       = this._read_nonempty_lines(env, noun_file)
    let adjectives  = this._read_nonempty_lines(env, adj_file)

    // --------------------------------------------------------
    // 2️⃣  Main generation loop – exactly the same output format as
    //     name‑generator.sh
    // --------------------------------------------------------
    let rand = Random.create()?

    var i: USize = 0
    while i < count do
      // pick a random noun, lower‑casing it
      let noun_idx = rand.int(nouns.size())
      let noun = nouns(noun_idx)?.lower()

      // pick a random adjective (preserve case)
      let adj_idx = rand.int(adjectives.size())
      let adj = adjectives(adj_idx)?

      env.out.print(adj + separator + noun)

      i = i + 1
    end

  // --------------------------------------------------------
  // Helpers
  // --------------------------------------------------------

  // Determine how many lines to emit.
  fun _determine_count(env: Env): USize =>
    // 1️⃣  env var `counto` (or legacy `COUNT`)
    let count_opt = if env.vars("counto").size() > 0 then
        this._parse_usize(env.vars("counto")(0)) ?
      else if env.vars("COUNT").size() > 0 then
        this._parse_usize(env.vars("COUNT")(0)) ?
      else
        None
      end
    match count_opt
    | let n: USize => n
    else
      // 2️⃣  try `tput lines`
      match this._tput_lines()
      | let n2: USize => n2
      else 24 end
    end

  // Parse a string to USize, returning `None` on failure.
  fun _parse_usize(s: String): USize ? =>
    try s.usize()? else error end

  // Run `tput lines` and return the result (or `None` on any error).
  fun _tput_lines(): (USize | None) =>
    try
      let proc = Process.create("tput", ["lines"])
      let out = proc.stdout().read_string()
      proc.wait()
      this._parse_usize(out.trim()) ?
    else
      None
    end

  // Resolve a folder path: env var overrides, otherwise default.
  fun _default_folder(env: Env, var_name: String): String =>
    if env.vars(var_name).size() > 0 then
      let p = env.vars(var_name)(0)
      if p != "" then p else "./" + var_name.lower() end
    else
      "./" + var_name.lower()
    end

  // Resolve a file path: env var overrides (must be a regular file),
  // otherwise pick a random regular file from `folder`.
  fun _resolve_file(env: Env, env_var: String, folder: String): String =>
    match env.vars(env_var)
    | let p: String =>
      if p != "" then
        // Verify that the path exists and is a regular file.
        try
          let fp = FilePath.from_string(p)?
          if fp.is_file() then
            return p.clone()
          else
            env.err.print("Environment variable " + env_var + " points to a non‑regular file: " + p)
          end
        else
          env.err.print("Cannot access file given by " + env_var + ": " + p)
        end
      end
    end
    // Fallback – pick a random regular file from the folder.
    this._pick_random_file(folder)

  // Pick a random regular file from `folder`.  Panics if the folder
  // contains no regular files (mirroring the behaviour of the shell
  // script which would fail later when trying to read).
  fun _pick_random_file(folder: String): String iso^ =>
    let dir = FilePath.from_string(folder)?
    let entries = dir.list()?
    // Filter only regular files.
    let files = Array[FilePath]
    for e in entries.values() do
      if e.is_file() then files.push(e) end
    end
    if files.size() == 0 then
      // Same error message style as the other implementations.
      error
    end
    let rand = Random
    let idx = rand.int(files.size())
    files(idx)?.string()

  // Read a file, return an Array of non‑empty, trimmed lines.
  fun _read_nonempty_lines(env: Env, path: String): Array[String] iso^ =>
    let fp = FilePath.from_string(path)?
    let file = fp.open()?
    let lines = Array[String]
    while true do
      match file.read_line()?
      | let line: String =>
        let trimmed = line.trim()
        if trimmed.size() > 0 then lines.push(trimmed) end
      | None => break
      end
    end
    lines
