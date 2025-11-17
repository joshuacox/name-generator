use "files"
use "random"
use "collections"
use "sys"
use "os"

actor Main
  new create(env: Env) =>
    // --------------------------------------------------------
    // Configuration – environment variables with the same defaults
    // --------------------------------------------------------
    let separator = env.var("SEPARATOR").get_or_else("-")
    // Allow overriding the count via either COUNT (uppercase) or counto (lowercase)
    let count_str = env.var("COUNT").or_else(env.var("counto")).get_or_else("")
    let counto: USize =
      try count_str.parse_int()?.usize()
      else 24 end                     // fallback when not set or not a number

    // folders (same defaults as the shell script)
    let cwd = env.cwd()
    let noun_folder = env.var("NOUN_FOLDER").get_or_else(cwd + "/nouns")
    let adj_folder  = env.var("ADJ_FOLDER").get_or_else(cwd + "/adjectives")

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
    let debug = env.var("DEBUG") == "true"

    // --------------------------------------------------------
    // Main generation loop
    // --------------------------------------------------------
    let rnd = Random
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
  fun resolve_file(env: Env, var_name: String, folder: String): (String | None) =>
    match env.var(var_name)
    | let p: String =>
        // non‑empty env var – must point to a regular file
        if p.trim().size() > 0 then
          if FileInfo(p).is_file() then p else pick_random_file(env, folder) end
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
    // collect regular files in the directory
    let entries = Dir.open(folder)?
    var files: Array[String] = Array[String]
    for entry in entries.entries() do
      // entry.path() is relative to the opened directory
      let full = folder + "/" + entry.path()
      if FileInfo(full).is_file() then files.push(full) end
    end
    if files.size() == 0 then
      env.err.print("Folder " + folder + " contains no regular files")
      None
    else
      let rnd = Random
      let idx = rnd.next_int(files.size())
      files(idx)?
    end

  // --------------------------------------------------------
  // Read a file, return an Array of trimmed, non‑empty lines.
  // --------------------------------------------------------
  fun read_nonempty_lines(env: Env, path: String): (Array[String] | None) =>
    let file = OpenFile(path)?
    var lines: Array[String] = Array[String]
    for raw in file.lines() do
      let line = raw.trim()
      if line.size() > 0 then lines.push(line) end
    end
    lines
