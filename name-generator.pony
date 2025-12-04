use "files"
use "random"
use "collections"

actor Main
  new create(env: Env) =>
    // --------------------------------------------------------
    // Configuration – environment variables with the same defaults
    // --------------------------------------------------------
    // Separator – fallback to "-" when the env var is not set or empty
    let separator = 
      if env.vars("SEPARATOR").size() > 0 then env.vars("SEPARATOR")(0) else "-" end
    // Allow overriding the count via either COUNT (uppercase) or counto (lowercase)
    // If neither is set (or both are empty) we fall back to an empty string,
    // which later triggers the default of 24.
    let count_str = 
      if env.vars("COUNT").size() > 0 then env.vars("COUNT")(0)
      else if env.vars("counto").size() > 0 then env.vars("counto")(0)
      else "" end end
    let counto: USize =
      try count_str.parse_int()?.usize()
      else 24 end                     // fallback when not set or not a number

    // folders (same defaults as the shell script)
    let cwd = "."
    let noun_folder = 
      if env.vars("NOUN_FOLDER").size() > 0 then env.vars("NOUN_FOLDER")(0) else cwd + "/nouns" end
    let adj_folder = 
      if env.vars("ADJ_FOLDER").size() > 0 then env.vars("ADJ_FOLDER")(0) else cwd + "/adjectives" end

    // --------------------------------------------------------
    // Resolve the noun / adjective files (env var overrides)
    // --------------------------------------------------------
    let noun_file = resolve_file(env, "NOUN_FILE", noun_folder)?
    let adj_file  = resolve_file(env, "ADJ_FILE",  adj_folder )?

    // --------------------------------------------------------
    // Load the word lists (non‑empty, trimmed lines)
    // --------------------------------------------------------
    let nouns      = read_nonempty_lines(env, noun_file)?
    let adjectives = read_nonempty_lines(env, adj_file)?

    // --------------------------------------------------------
    // Debug flag – prints the same data the shell script prints
    // --------------------------------------------------------
    let debug = 
      if env.vars("DEBUG").size() > 0 then env.vars("DEBUG")(0) == "true"
      else false end

    // --------------------------------------------------------
    // Main generation loop
    // --------------------------------------------------------
    let rnd = Random.create()
    var i: USize = 0
    while i < counto do
      // pick a random noun and lower‑case it
      let noun = nouns(rnd.next_int(nouns.size()))?.lower()
      // pick a random adjective (preserve case)
      let adj  = adjectives(rnd.next_int(adjectives.size()))?

      // optional debug output (to stderr)
      if debug then
        env.err.print(adj)
        env.err.print(noun)
        env.err.print(noun_file)
        env.err.print(adj_file)
        env.err.print(noun_folder)
        env.err.print(adj_folder)
        env.err.print(i.string() + " > " + counto.string())
      end

      // emit the generated name
      env.out.print(adj + separator + noun)

      i = i + 1
    end

  // --------------------------------------------------------
  // Resolve a file: env‑var overrides, otherwise pick a random regular
  // file from the supplied folder.  Returns the absolute path as a
  // String.  On error the program aborts with a clear message.
  // --------------------------------------------------------
  fun resolve_file(env: Env, var_name: String, folder: String): String ? =>
    // Get the array of values for the variable
    let vals = env.vars(var_name)
    if vals.size() > 0 then
      let p = vals(USize(0))
      // non‑empty env var – must point to a regular file
      if p.trim().size() > 0 then
        if FileInfo(FilePath.from_string(p)).is_file() then p else pick_random_file(env, folder) end
      else
        // empty string – treat as “not set”
        pick_random_file(env, folder)
      end
    else
      // not set – pick a random file from the folder
      pick_random_file(env, folder)
    end

  // --------------------------------------------------------
  // Pick a random regular file from a directory.  Errors abort.
  // --------------------------------------------------------
  fun pick_random_file(env: Env, folder: String): (String | None) =>
    // collect regular files in the directory using FileInfo.list()
    // `FileInfo(folder)?` creates a FileInfo for the directory; `list()?`
    // returns an Array[FileInfo] for each entry inside it.
    let dir_info = FileInfo(FilePath.from_string(folder))?
    let entries = dir_info.list()?

    var files: Array[String] = Array[String]
    for entry in entries.values() do
      // `entry.path()` is the absolute path of the entry
      if entry.is_file() then files.push(entry.path()) end
    end
    if files.size() == 0 then
      env.err.print("Folder " + folder + " contains no regular files")
      None
    else
      let rnd = Random.create()
      let idx = rnd.next_int(files.size())
      files(idx)?
    end

  // --------------------------------------------------------
  // Read a file, return an Array of trimmed, non‑empty lines.
  // --------------------------------------------------------
  fun read_nonempty_lines(env: Env, path: String): (Array[String] | None) =>
    let file = OpenFile(FilePath.from_string(path))?
    var lines: Array[String] = Array[String]
    for raw in file.lines() do
      let line = raw.trim()
      if line.size() > 0 then lines.push(line) end
    end
    lines
