// name-generator.pony - A Pony language version of the robust name generator

use "files"
use "random"
use "arrays"
use "env"

class EnvHelper
  fun env_or_default(var_name: String, default: String): String =>
    Process.env().get(var_name) ?: default

class Config
  let noun_folder: String
  let adj_folder: String
  let separator: String
  var count_o: U32
  
  new create(noun_folder': String = "nouns", adj_folder': String = "adjectives", separator': String = "-") =>
    noun_folder = noun_folder'
    adj_folder = adj_folder'
    separator = separator'
    
    // Number of lines to generate - try tput lines, fallback to 24
    let tput_lines = Process.env().get("LINES")
    match tput_lines | Some(val) => 
      try
        count_o = U32.from(String(val))
      else
        count_o = 0: U32
      end
    | None =>
      count_o = 24: U32
    end

class FileHandler
  let noun_folder: String
  let adj_folder: String
  
  new create(noun_folder': String, adj_folder': String) =>
    noun_folder = noun_folder'
    adj_folder = adj_folder'

  fun pick_random_file(folder: String): String =>
    // Pick a random regular file from the folder
    let files = Files.list(folder)
    let filtered_files = Array.filter(files, { (f) => f.find(".DS_Store") == -1 })
    if filtered_files.size > 0 then
      filtered_files.random()
    else
      ""
    end

class NameGenerator
  let config: Config
  let file_handler: FileHandler
  
  new create(config': Config) =>
    config = config'
    file_handler = FileHandler(config.noun_folder, config.adj_folder)

  fun generate_name(): String =>
    // Pick random adjective and noun files
    let adj_file = file_handler.pick_random_file(config.adj_folder)
    let noun_file = file_handler.pick_random_file(config.noun_folder)
    
    // Read lines from files
    let adj_lines = FileLines.read_and_filter(adj_file, { (l) => l != "" })
    let noun_lines = FileLines.read_and_filter(noun_file, { (l) => l != "" })
    
    // Pick random entries preserving case for adjectives, lowercasing nouns
    var adjective: String = ""
    match adj_lines.random() | Some(line) =>
      adjective = line
    | None =>
      ""
    end
    
    var noun: String = ""
    match noun_lines.random() | Some(line) =>
      noun = line.lower()
    | None =>
      ""
    end

    let full_name = adjective + config.separator + noun
    
    // Debug output if needed
    if Process.env().get("DEBUG") == "true" then
      Debug.out("Adjective: " + adjective)
      Debug.out("Noun: " + noun)
      Debug.out("Full Name: " + full_name)
  
    return full_name

actor Main
  new create(env: Env) =>
    let config = Config(EnvHelper.env_or_default("count_o", "24"), EnvHelper.env_or_default("separator", "-"))
    let name_generator = NameGenerator(config)
    
    // Generate names count_o times
    var name_count: U32 = 0
    while name_count < config.count_o do
      let name = name_generator.generate_name()
      env.out.print(name + "\n")
      name_count := name_count + 1
    end
